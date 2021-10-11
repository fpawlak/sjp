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
            | Abort State
            | Output Integer Domain
            | Input (Integer -> Domain)

fix :: (a -> a) -> a
fix f = f (fix f)

lift :: (State -> Domain) -> (Domain -> Domain)
lift f (Term st) = f st
lift f abort@(Abort _) = abort
lift f (Output out rest) = Output out (lift f rest)
lift f (Input fun) = Input (\input -> lift f (fun input))

compose :: (State -> Domain) -> (State -> Domain) -> (State -> Domain)
compose f g st = lift f (g st)

cond :: (State -> Bool) -> (State -> Domain) -> (State -> Domain) -> State -> Domain
cond bDen c1Den c2Den st = if bDen st then c1Den st else c2Den st

sem :: Com -> State -> Domain
sem Skip st = Term st
sem (Ass x a) st = Term st'
  where st' = put st x (aeval a st)
sem (Seq c1 c2) st = compose (sem c2) (sem c1) st
sem (If b c1 c2) st = cond (beval b) (sem c1) (sem c2) st
sem (While b c) st = fix f st
  where f g = cond (beval b) (compose g (sem c)) Term
sem Fail st = Abort st
sem (Print a) st = Output (aeval a st) (Term st)
sem (Read x) st = Input (\input -> Term (put st x input))

type K = State -> Domain

emptyK = Term

condK :: (State -> Bool) -> (K -> K) -> (K -> K) -> K -> K
condK bDen c1Den c2Den k st = if bDen st then c1Den k st else c2Den k st

semK :: Com -> K -> K
semK Skip k = k
semK (Ass x a) k = \st -> let st' = put st x (aeval a st) in k st'
semK (Seq c1 c2) k = semK c1 (semK c2 k)
semK (If b c1 c2) k = condK (beval b) (semK c1) (semK c2) k
semK (While b c) k = fix f
  where f g = condK (beval b) (\_ -> semK c g) id k
semK Fail k = \st -> Abort st
semK (Print a) k = \st -> Output (aeval a st) (k st)
semK (Read x) k = \st -> Input (\input -> k (put st x input))


run :: (Com -> State -> Domain) -> Com -> State -> [Integer] -> IO Domain
run sem c st = handle (sem c st)
  where handle res@(Term _) _ = return res
        handle res@(Abort _) _ = putStrLn "abort" >> return res
        handle (Output num rest) inputs = print num >> handle rest inputs
        handle (Input fun) (input:inputs) = handle (fun input) inputs

run_sem file inputs = do
  c <- parseFile file
  run sem c emptyState inputs

run_semK file inputs = do
  c <- parseFile file
  run (\c st -> semK c emptyK st) c emptyState inputs
