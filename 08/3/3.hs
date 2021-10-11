import AbstractSyntax
import Parser

data State = State [(Varname, Integer)] deriving Show

emptyState :: State
emptyState = State []

get :: State -> Varname -> Integer
get (State []) x = 0
get (State ((var, val):t)) x = if var == x then val else get (State t) x

put :: State -> Varname -> Integer -> State
put (State l) x n = State ((x, n):l')
  where l' = filter (\(key, _) -> key /= x) l

aeval :: Aexp -> State -> Integer
aeval (Nat n) _ = n
aeval (Var x) st = get st x
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

data Domain = Term State
            | NoSuchLabel (GotoLabel, State)
              deriving Show

fix :: (a -> a) -> a
fix f = f (fix f)

-- semantyka kontynuacyjna

type K omega = State -> omega
type Env omega = GotoLabel -> K omega

type DenotationK omega = Env omega -> K omega -> K omega

emptyK :: K Domain
emptyK = Term

emptyEnv :: Env Domain
emptyEnv gotoLabel st = NoSuchLabel (gotoLabel, st)

condK :: (State -> Bool) -> DenotationK omega -> DenotationK omega -> DenotationK omega
condK bDen c1Den c2Den env k st = if bDen st then c1Den env k st else c2Den env k st

shadow :: Eq a => (a -> b) -> (a, b) -> (a -> b)
shadow f (x, y) x' = if x == x' then y else f x'

semK :: Com -> DenotationK omega
semK Skip _ k = k
semK (Ass x a) _ k = \st -> let st' = put st x (aeval a st) in k st'
semK (Seq c1 c2) env k = semK c1 env (semK c2 env k)
semK (If b c1 c2) env k = condK (beval b) (semK c1) (semK c2) env k
semK (While b c) env k = fix f
  where f g = condK (beval b) (\env _ -> semK c env g) (\_ k -> k) env k
semK (Goto gotoLabel) env k = env gotoLabel
-- chcemy listę:
-- [semK c1 env' k2,
--  ...
--  semK c(n-1) env' kn,
--  semK cn env' k]
-- konstruujemy ją od końca
semK (Block fragments) env k = case continuations of [] -> k -- pusty blok to skip
                                                     (k1:_) -> k1 -- wchodzimy do pierwszej instrukcji
  where continuations = reverse $ fix f
        f g = map (\(c, nextContinuation) -> semK c env' nextContinuation)
                  (zip commandList $ (k : g))
          where env' = foldl shadow env $ zip labelList g
                labelList = reverse $ map fst fragments
                commandList = reverse $ map snd fragments

run_semK file = do
  c <- parseFile file
  print $ semK c emptyEnv emptyK emptyState
