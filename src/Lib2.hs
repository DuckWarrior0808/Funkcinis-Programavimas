{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1
import Data.Char (isDigit, isLetter, isSpace)

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- PARSER 


orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input =
  case p1 input of
    Right result -> Right result
    Left _ -> p2 input


and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f p1 p2 input =
  case p1 input of
    Right (v1, rest1) ->
      case p2 rest1 of
        Right (v2, rest2) -> Right (f v1 v2, rest2)
        Left err -> Left err
    Left err -> Left err


and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f p1 p2 p3 input =
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
             Right (v3, r3) -> Right (f v1 v2 v3, r3)
             Left err -> Left err
        Left err -> Left err
    Left err -> Left err


and4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4 f p1 p2 p3 p4 input =
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
             Right (v3, r3) ->
               case p4 r3 of
                 Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                 Left err -> Left err
             Left err -> Left err
        Left err -> Left err
    Left err -> Left err


and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5 f p1 p2 p3 p4 p5 input =
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
             Right (v3, r3) ->
               case p4 r3 of
                 Right (v4, r4) ->
                   case p5 r4 of
                     Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                     Left err -> Left err
                 Left err -> Left err
             Left err -> Left err
        Left err -> Left err
    Left err -> Left err

and2_ :: Parser a -> Parser b -> Parser a
and2_ p1 p2 = and2 (\x _ -> x) p1 p2

_and2 :: Parser a -> Parser b -> Parser b
_and2 p1 p2 = and2 (\_ x -> x) p1 p2

-- PRIMITIVE PARSERS


parseChar :: Char -> Parser Char
parseChar c (x:xs)
  | x == c = Right (c, xs)
  | otherwise = Left $ "Expected '" ++ [c] ++ "', got '" ++ [x] ++ "'"
parseChar c [] = Left $ "Expected '" ++ [c] ++ "', got end of input"

parseLiteral :: String -> Parser String
parseLiteral [] input = Right ("", input)
parseLiteral (c:cs) (x:xs)
  | c == x = case parseLiteral cs xs of
               Right (_, rest) -> Right (c:cs, rest)
               Left err -> Left err
  | otherwise = Left $ "Expected " ++ (c:cs) ++ ", found mismatch"
parseLiteral s [] = Left $ "Expected " ++ s ++ ", got end of input"

parseWhitespace :: Parser String
parseWhitespace input = Right (span isSpace input)

parseToken :: String -> Parser String
parseToken s = and2_ (parseLiteral s) parseWhitespace

parseDigit :: Parser Char
parseDigit (x:xs) | isDigit x = Right (x, xs)
parseDigit _ = Left "Expected digit"

parseDigits :: Parser String
parseDigits input =
    let (d, r) = span isDigit input
    in if null d then Left "Expected digits" else Right (d, r)

parseNumber :: Parser Double
parseNumber input =
  case parseDigits input of
    Right (d1, rest1) ->
        case parseChar '.' rest1 of
            Right (_, rest2) ->
                case parseDigits rest2 of
                    Right (d2, rest3) -> Right (read (d1 ++ "." ++ d2), rest3)
                    Left _ -> Right (read d1, rest1)
            Left _ -> Right (read d1, rest1)
    Left err -> Left err

parseIdent :: Parser String
parseIdent input =
  case span (\c -> isLetter c || isDigit c) input of
    (m, r) | not (null m) && isLetter (head m) -> Right (m, r)
    _ -> Left "Expected identifier"

parseQName :: Parser String
parseQName input =
  case parseChar '"' input of
    Right (_, r1) ->
        let (content, rest) = span (/= '"') r1
        in case parseChar '"' rest of
             Right (_, finalRest) -> Right (content, finalRest)
             Left _ -> Left "Missing closing quote"
    Left err -> Left err


-- DOMAIN PARSERS


-- BNF primitives
parseAntenna :: Parser String
parseAntenna = (parseToken "Wi-fi 6") `orElse` (parseToken "Wi-fi 7")

parseKeypad :: Parser String
parseKeypad = (parseToken "German") `orElse` (parseToken "Swedish") `orElse` (parseToken "American")

parseCamera :: Parser String
parseCamera = (parseToken "Basic") `orElse` (parseToken "Pro Ultra") `orElse` (parseToken "Pro")

parseProcessor :: Parser String
parseProcessor = (parseToken "X6") `orElse` (parseToken "X7") `orElse` (parseToken "X8")

-- <phone>
parsePhone :: Parser Lib1.Command
parsePhone = and5 (\id ant key cam _ -> Lib1.CreatePhone id ant key cam)
             (and2_ parseIdent parseWhitespace)
             (and2_ parseAntenna parseWhitespace)
             (and2_ parseKeypad parseWhitespace)
             (and2_ parseCamera parseWhitespace)
             (and2_ parseProcessor parseWhitespace)

-- <mpath>
parseMPath :: Parser (String, String)
parseMPath = and3 (\i _ q -> (i, q))
             parseIdent
             (parseChar '/')
             parseQName

-- <segs>
parseSegs :: Parser [String]
parseSegs input =
    case parseQName input of
        Right (q, r1) ->
            case parseChar '/' r1 of
                Right (_, r2) -> case parseSegs r2 of
                        Right (qs, r3) -> Right (q:qs, r3)
                        Left err -> Left err
                Left _ -> Right ([q], r1)
        Left err -> Left err

-- <ppath>
parsePPath :: Parser (String, String, [String])
parsePPath = and3 (\(mId, mName) _ segs -> (mId, mName, segs))
             parseMPath
             (parseChar '/')
             parseSegs

-- <command_create> ::= "create" <part>
parseCreatePart :: Parser Lib1.Command
parseCreatePart = and4 (\_ name _ mpath -> Lib1.AddPart name mpath)
                  (parseToken "part")
                  (and2_ parseQName parseWhitespace)
                  (parseToken "to")
                  (and2_ parseMPath parseWhitespace)

parseCreate :: Parser Lib1.Command
parseCreate = _and2 (parseToken "create") (parseCreatePart `orElse` parsePhone)

-- <command_add> ::= "add" <subpart>
parseSubpart :: Parser Lib1.Command
parseSubpart = and4 (\_ name _ (mId, mName, segs) -> Lib1.AddSubpart name (mId, mName, segs))
               (parseToken "subpart")
               (and2_ parseQName parseWhitespace)
               (parseToken "under")
               (and2_ parsePPath parseWhitespace)

parseAdd :: Parser Lib1.Command
parseAdd = _and2 (parseToken "add") parseSubpart

-- <command_set> ::= "set" <setcost>
parseSetCost :: Parser Lib1.Command
parseSetCost = and4 (\_ (mId, mName, segs) _ val -> Lib1.SetCost (mId, mName, segs) val)
               (parseToken "cost")
               (and2_ parsePPath parseWhitespace)
               (and2_ (parseChar '=') parseWhitespace)
               (and2_ parseNumber parseWhitespace)

parseSet :: Parser Lib1.Command
parseSet = _and2 (parseToken "set") parseSetCost

-- <command_list>
parseList :: Parser Lib1.Command
parseList = and3 (\_ _ mpath -> Lib1.ListParts mpath)
            (parseToken "list")
            (parseToken "partsin")
            (and2_ parseMPath parseWhitespace)

-- <command_calculate>
parseCalculate :: Parser Lib1.Command
parseCalculate = and3 (\_ _ target -> Lib1.CalculateCost target)
                 (parseToken "calculate")
                 (parseToken "costof")
                 (and2_ parseEitherTarget parseWhitespace)

-- Helper for CalculateCost: parses either ppath (Right) or mpath (Left)
parseEitherTarget :: Parser (Either (String, String) (String, String, [String]))
parseEitherTarget input =
    case parsePPath input of
        Right ((mid, mname, segs), rest) -> Right (Right (mid, mname, segs), rest)
        Left _ ->
            case parseMPath input of
                Right ((mid, mname), rest) -> Right (Left (mid, mname), rest)
                Left err -> Left "Expected target (mpath or ppath)"

-- <command_dump>
parseDump :: Parser Lib1.Command
parseDump = and2 (\_ i -> if i == "Examples" then Lib1.Dump Lib1.Examples else Lib1.Dump Lib1.Examples)
            (parseToken "dump")
            (and2_ parseIdent parseWhitespace)


-- MAIN ENTRY POINTS


parseCommand :: Parser Lib1.Command
parseCommand = parseCreate
               `orElse` parseAdd
               `orElse` parseSet
               `orElse` parseList
               `orElse` parseCalculate
               `orElse` parseDump

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]


-- CLASSES


class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.CreatePhone ident ant key cam) =
      "create " ++ ident ++ " " ++ ant ++ " " ++ key ++ " " ++ cam ++ " X8"

  toCliCommand (Lib1.AddPart name (mId, mName)) =
      "create part \"" ++ name ++ "\" to " ++ mId ++ "/\"" ++ mName ++ "\""

  toCliCommand (Lib1.AddSubpart name (mId, mName, segs)) =
      "add subpart \"" ++ name ++ "\" under " ++ mId ++ "/\"" ++ mName ++ "\"" ++ formatSegs segs

  toCliCommand (Lib1.SetCost (mId, mName, segs) val) =
      "set cost " ++ mId ++ "/\"" ++ mName ++ "\"" ++ formatSegs segs ++ " = " ++ show val

  toCliCommand (Lib1.ListParts (mId, mName)) =
      "list partsin " ++ mId ++ "/\"" ++ mName ++ "\""

  toCliCommand (Lib1.CalculateCost (Left (mId, mName))) =
      "calculate costof " ++ mId ++ "/\"" ++ mName ++ "\""
  toCliCommand (Lib1.CalculateCost (Right (mId, mName, segs))) =
      "calculate costof " ++ mId ++ "/\"" ++ mName ++ "\"" ++ formatSegs segs

  toCliCommand (Lib1.Dump Lib1.Examples) = "dump Examples"

formatSegs :: [String] -> String
formatSegs [] = ""
formatSegs (s:ss) = "/\"" ++ s ++ "\"" ++ formatSegs ss

instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  (Lib1.CreatePhone i1 a1 k1 c1) == (Lib1.CreatePhone i2 a2 k2 c2) =
      i1 == i2 && a1 == a2 && k1 == k2 && c1 == c2
  (Lib1.AddPart n1 t1) == (Lib1.AddPart n2 t2) =
      n1 == n2 && t1 == t2
  (Lib1.AddSubpart n1 t1) == (Lib1.AddSubpart n2 t2) =
      n1 == n2 && t1 == t2
  (Lib1.SetCost t1 v1) == (Lib1.SetCost t2 v2) =
      t1 == t2 && v1 == v2
  (Lib1.ListParts t1) == (Lib1.ListParts t2) =
      t1 == t2
  (Lib1.CalculateCost e1) == (Lib1.CalculateCost e2) =
      e1 == e2
  (Lib1.Dump Lib1.Examples) == (Lib1.Dump Lib1.Examples) = True
  _ == _ = False