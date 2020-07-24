module Reduce where

import Core
import Control.Monad.Trans.Writer.Lazy (Writer, tell)
import Data.Set (Set, member)
import qualified Data.Set as Set
import Data.Traversable
import Data.Text (Text)
import qualified Data.Text as T

pipeline :: Goal -> Bool
pipeline = reduce 0 . normalize

wpipeline :: Goal -> Writer Text Bool
wpipeline = wreduce 0 . normalize

-- p and q -> r = p and q and ~r -> false
normalize :: Goal -> [Prop]
normalize (Goal props conclusion) =
  map norm (Not conclusion : props)

-- rewrite Implies and Iff in terms of And, Or, Not, and variables
-- this isn't strictly necessary, but if we want to extend the logic later we can
-- just add more rewrite cases to norm
-- nandify is an extreme example of this, rewriting every term in terms of one logical gate
norm :: Prop -> Prop
norm PropTrue = PropTrue
norm PropFalse = PropTrue
norm var@(Var _) = var
norm (Or p q) = Or (norm p) (norm q)
norm (And p q) = And (norm p) (norm q)
norm (Not p) = Not (norm p)
-- p -> q = ~p or q
norm (Implies p q) = Or (Not (norm p)) (norm q)
-- p <-> q = p -> q and q -> p = (~p or q) and (~q or p)
norm (Iff p q) = And (Or (Not (norm p)) (norm q)) (Or (Not (norm q)) (norm p))

-- the goal of reduce is to prove a contradiction, or to show that
-- such a contradiction cannot be found.
-- n is a recursion limit: no point in going over the same list twice
reduce :: Int -> [Prop] -> Bool
reduce n l@(var@(Var _) : rest)
  | n > length l = False                        -- hit recursion limit, goal does not obtain
  | otherwise = reduce (n+1) (rest <> [var])    -- otherwise cycle through and see if anything else can be reduced
reduce n (PropTrue : rest) = reduce (n+1) rest  -- no point in keeping PropTrue around
reduce _ (PropFalse : _) = True                 -- if False has been proven, we have proven contradiction
reduce n (And p q : rest) =
  reduce 0 (p : q : rest)                       -- p and q = p, q
reduce n (Or p q : rest) =
  reduce 0 (p : rest) &&                        -- check both cases of the or. both must either be inconclusive or
   reduce 0 (q : rest)                          -- explicitly prove a contradiction to prove an overall contradiction
reduce n l@(Not p : rest)
  | n > length l = False                        -- hit recursion limit
  | p `elem` rest = True                        -- Not p, p (instant contradiction)
  | otherwise = case p of
      PropTrue -> True                          -- Not PropTrue = PropFalse, we found our contradiction
      PropFalse -> reduceRest                   -- Not PropFalse = PropTrue, no point in keeping it around
      And p q ->
        reduce 0 (Or (Not p) (Not q) : rest)    -- De Morgan's laws
      Or p q ->
        reduce 0 (And (Not p) (Not q) : rest)   -- De Morgan's laws
      Not p -> reduce 0 (p : rest)              -- Not Not p = p
      Var x -> reduceRest                       -- if you just see a negated variable, cycle through
      Implies _ _ -> impliesError
      Iff _ _ -> iffError
      where reduceRest =
              reduce (n+1) $ rest <> [Not p]    -- cycle through
reduce _ (Implies _ _ : _) = impliesError
reduce _ (Iff _ _ : _) = iffError

impliesError = error "unexpected implication in reduce"
iffError = error "unexpected biconditional in reduce"

-- a version of reduce that tells you what it's up to
-- same as the above, but with a Writer for heavy/messy annotating
wreduce :: Int -> [Prop] -> Writer Text Bool
wreduce n l@(var@(Var _) : rest)
  | n > length l = t l >> tell "hit recursion limit\n" >> pure False
  | otherwise = t l >> tell "variable, cycling\n" >> wreduce (n+1) (rest <> [var])
wreduce n l@(PropTrue : rest) = t l >> tell "literal True, ignoring\n" >> wreduce (n+1) rest
wreduce _ l@(PropFalse : _) = t l >> tell "literal False, proof complete!\n" >> pure True
wreduce n l@(And p q : rest) = t l >> tell "and, adding parts to env\n" >> wreduce 0 (p : q : rest)
wreduce n l@(Or p q : rest) = do
  t l >> tell "or, splitting into cases\n"
  a <- wreduce 0 (p : rest)
  t l >> tell "second case of or\n"
  b <- wreduce 0 (q : rest)
  pure (a && b)
wreduce n l@(Not p : rest)
  | n > length l = t l >> tell "hit recursion limit\n" >> pure False
  | p `elem` rest = t l >> tell ("found contradiction, proof complete!\n") >> pure True
  | otherwise = case p of
      PropTrue -> t l >> tell "not True, proof complete!\n" >> pure True
      PropFalse -> t l >> tell "not False, continuing\n" >> wreduceRest
      And p q ->
        t l >> tell "not and, applying demorgan's law\n" >>
        wreduce 0 (Or (Not p) (Not q) : rest)
      Or p q ->
        t l >> tell "not or, applying demorgan's law\n" >>
        wreduce 0 (And (Not p) (Not q) : rest)
      Not p -> t l >> tell "double negation, canceling\n" >> wreduce 0 (p : rest)
      Var x -> t l >> tell "not var, cycling\n" >> wreduceRest
      Implies _ _ -> impliesError
      Iff _ _ -> iffError
      where wreduceRest =
              wreduce (n+1) $ rest <> [Not p]    -- cycle through
wreduce _ (Implies _ _ : _) = impliesError
wreduce _ (Iff _ _ : _) = iffError

t :: [Prop] -> Writer Text ()
t props = tell $ display (Goal props PropFalse)

display :: Goal -> Text
display (Goal props concl) = T.concat
  [ T.intercalate ", " (map displayProp props)
  , "\n"
  ]

displayProp :: Prop -> Text
displayProp (Var x)       = x
displayProp PropTrue      = "T"
displayProp PropFalse     = "F"
displayProp (Or p q)      = displayProp p <> " ∨ " <> displayProp q
displayProp (And p q)     = displayProp p <> " ∧ " <> displayProp q
displayProp (Not p)       = "¬" <> displayProp p
displayProp (Implies p q) = displayProp p <> " → " <> displayProp q
displayProp (Iff p q)     = displayProp p <> " ↔ " <> displayProp q

-- for circuit building at some later time
-- TODO: graphviz circuits?
nandify :: Prop -> Nand
nandify (Not p)   = Nand (nandify p) (nandify p)
nandify (Or p q)  = Nand (Nand (nandify p) (nandify p)) (Nand (nandify q) (nandify q))
nandify (And p q) = Nand (Nand (nandify p) (nandify q)) (Nand (nandify p) (nandify q))
nandify (Var x)   = NandVar x
nandify PropTrue  = NandTrue
nandify PropFalse = NandFalse
nandify _ = error "unexpected implication or biconditional during nandification"
