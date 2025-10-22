module Lib1
    ( examples, Command(..), Dumpable(..)
    ) where

data Dumpable = Examples
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed
data Command = Dump Dumpable
  deriving Show

examples :: [Command]
examples = [
    Dump Examples
    ]
