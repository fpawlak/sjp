module Common (shadow, aeval, beval, State) where

import AbstractSyntax
import Parser

shadow :: Eq a => (a -> b) -> a -> b -> (a -> b)
shadow f x y x' = if x == x' then y else f x'

type State = Varname -> Integer

emptyState :: State
emptyState = \_ -> 0

put :: State -> Varname -> Integer -> State
put st x n x' = if x == x' then n else st x

aeval :: Aexp -> State -> Integer
aeval (Nat n) _ = n
aeval (Var x) st = st x
aeval (Add a1 a2) st = v1 + v2
  where v1 = aeval a1 st
        v2 = aeval a2 st
aeval (Sub a1 a2) st = v1 - v2
  where v1 = aeval a1 st
        v2 = aeval a2 st
aeval (Mul a1 a2) st = v1 * v2
  where v1 = aeval a1 st
        v2 = aeval a2 st

beval :: Bexp -> State -> Bool
beval BTrue st = True
beval BFalse st = False
beval (Eq a1 a2) st = v1 == v2
  where v1 = aeval a1 st
        v2 = aeval a2 st
beval (Le a1 a2) st = v1 <= v2
  where v1 = aeval a1 st
        v2 = aeval a2 st
beval (Not b) st = not v
  where v = beval b st
beval (And b1 b2) st = v1 && v2
  where v1 = beval b1 st
        v2 = beval b2 st
beval (Or b1 b2) st = v1 || v2
  where v1 = beval b1 st
        v2 = beval b2 st



