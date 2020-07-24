module Core where

import Data.Text

type Program = [Goal]

-- a collection of propositions, which may lead to the deduction of another proposition
data Goal = Goal [Prop] Prop

-- for circuit building at some later time
data Nand
  = Nand Nand Nand
  | NandVar Text
  | NandTrue
  | NandFalse
  deriving (Eq, Ord, Show)

-- propositions
data Prop
  = Var Text
  | Or Prop Prop
  | And Prop Prop
  | Implies Prop Prop
  | Not Prop
  | Iff Prop Prop
  | PropTrue
  | PropFalse
  deriving (Eq, Ord, Show)
