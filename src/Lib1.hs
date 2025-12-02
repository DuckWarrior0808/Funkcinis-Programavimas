module Lib1
  ( examples, Command(..), Dumpable(..)
  ) where

data Dumpable = Examples
  deriving Show

data Command
  = CreatePhone String String String String
  | AddPart String (String, String)
  | AddSubpart String (String, String, [String])
  | SetCost (String, String, [String]) Double
  | ListParts (String, String)
  | CalculateCost (Either (String, String) (String, String, [String]))
  | Dump Dumpable
  deriving Show

examples :: [Command]
examples =
  [ CreatePhone "LithuPhone" "Wi-fi 7" "German" "Pro Ultra"
  , AddPart "Main Board" ("LithuPhone", "LT-10")
  , AddSubpart "Power IC" ("LithuPhone", "LT-10", ["Main Board"])
  , SetCost ("LithuPhone", "LT-10", ["Main Board", "Power IC"]) 8.25
  , ListParts ("LithuPhone", "LT-10")
  , CalculateCost (Left ("LithuPhone", "LT-10"))
  , Dump Examples
  ]
