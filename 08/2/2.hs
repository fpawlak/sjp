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
            | Abort (FailLabel, State)
              deriving Show

fix :: (a -> a) -> a
fix f = f (fix f)

lift :: (State -> Domain) -> (Domain -> Domain)
lift f (Term st) = f st
lift f abort@(Abort _) = abort

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
sem (Fail l) st = Abort (l, st)
sem (Catch l c1 c2) st =
  case sem c1 st of term@(Term _) -> term
                    abort@(Abort (l', st')) -> if l == l' then sem c2 st'
                                               else abort

-- semantyka kontynuacyjna

type K omega = State -> omega
type FailK omega = FailLabel -> K omega

type DenotationK omega = K omega -> FailK omega -> K omega

emptyK :: K Domain
emptyK = Term

emptyFailK :: FailK Domain
emptyFailK failLabel st = Abort (failLabel, st)

condK :: (State -> Bool) -> DenotationK omega -> DenotationK omega -> DenotationK omega
condK bDen c1Den c2Den ks kf st = if bDen st then c1Den ks kf st else c2Den ks kf st

semK :: Com -> DenotationK omega
semK Skip ks _ = ks
semK (Ass x a) ks _ = \st -> let st' = put st x (aeval a st) in ks st'
semK (Seq c1 c2) ks kf = semK c1 (semK c2 ks kf) kf
semK (If b c1 c2) ks kf = condK (beval b) (semK c1) (semK c2) ks kf
semK (While b c) ks kf = fix f
  where f g = condK (beval b) (\_ kf -> semK c g kf) (\ks _ -> ks) ks kf
semK (Fail label) _ kf = kf label
semK (Catch l c1 c2) ks kf = semK c1 ks kf'
  where kf' failLabel = if failLabel == l then semK c2 ks kf
                        else kf failLabel

run_sem file = do
  c <- parseFile file
  print $ sem c emptyState

run_semK file = do
  c <- parseFile file
  print $ semK c emptyK emptyFailK emptyState
