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

-- małe kroki

astep :: Aexp -> State -> Aexp
astep a@(Nat n) _ = a
astep (Var x) st = Nat (get st x)
astep (Add (Nat n1) (Nat n2)) st = Nat (n1 + n2)
astep (Add a1@(Nat n1) a2) st = Add a1 (astep a2 st)
astep (Add a1 a2) st = Add (astep a1 st) a2
astep (Sub (Nat n1) (Nat n2)) st = Nat (n1 - n2)
astep (Sub a1@(Nat n1) a2) st = Sub a1 (astep a2 st)
astep (Sub a1 a2) st = Sub (astep a1 st) a2
astep (Mul (Nat n1) (Nat n2)) st = Nat (n1 * n2)
astep (Mul a1@(Nat n1) a2) st = Mul a1 (astep a2 st)
astep (Mul a1 a2) st = Mul (astep a1 st) a2

bvalue :: Bexp -> Maybe Bool
bvalue BTrue = Just True
bvalue BFalse = Just False
bvalue _ = Nothing

boolToBexp :: Bool -> Bexp
boolToBexp True = BTrue
boolToBexp False = BFalse

bstep :: Bexp -> State -> Bexp
bstep b@BTrue _ = b
bstep b@BFalse _ = b
bstep (Eq (Nat n1) (Nat n2)) st = boolToBexp (n1 == n2)
bstep (Eq a1@(Nat n1) a2) st = Eq a1 (astep a2 st)
bstep (Eq a1 a2) st = Eq (astep a1 st) a2
bstep (Le (Nat n1) (Nat n2)) st = boolToBexp (n1 <= n2)
bstep (Le a1@(Nat n1) a2) st = Eq a1 (astep a2 st)
bstep (Le a1 a2) st = Eq (astep a1 st) a2
bstep (Not b) st =
  case bvalue b of Just True -> BFalse
                   Just False -> BTrue
                   _ -> Not (bstep b st)
bstep (And b1 b2) st =
  case (bvalue b1, bvalue b2) of (Just v1, Just v2) -> boolToBexp (v1 && v2)
                                 (Just _, _) -> And b1 (bstep b2 st)
                                 (_, _) -> And (bstep b1 st) b2
bstep (Or b1 b2) st =
  case (bvalue b1, bvalue b2) of (Just v1, Just v2) -> boolToBexp (v1 || v2)
                                 (Just _, _) -> Or b1 (bstep b2 st)
                                 (_, _) -> Or (bstep b1 st) b2

cvalue :: Com -> State -> Maybe State
cvalue Skip st = Just st
cvalue _ _ = Nothing

cstep :: Com -> State -> (Com, State)
-- cstep Skip st = (Skip, st)
-- cstep (Ass x (Nat n)) st = (Skip, put st x n)
-- cstep (Ass x a) st = ((Ass x (astep a st)), st)
-- cstep (Seq Skip c2) st = cstep c2 st
-- cstep (Seq c1 c2) st = ((Seq c1' c2), st')
--   where (c1', st') = cstep c1 st
-- cstep (If b c1 c2) st =
--   case bvalue b of Just True -> cstep c1 st
--                    Just False -> cstep c2 st
--                    _ -> ((If (bstep b st) c1 c2), st)
-- cstep loop@(While b c) st = ((If b (Seq c loop) Skip), st)

cstep (Ass x (Nat n)) st = (Skip, put st x n)
cstep (Ass x a) st = ((Ass x (astep a st)), st)
cstep (Seq Skip c2) st = (c2, st)
cstep (Seq c1 c2) st = ((Seq c1' c2), st')
  where (c1', st') = cstep c1 st
cstep (If b c1 c2) st =
  case bvalue b of Just True -> (c1, st)
                   Just False -> (c2, st)
                   _ -> ((If (bstep b st) c1 c2), st)
cstep loop@(While b c) st = ((If b (Seq c loop) Skip), st)        

smallstep' :: Com -> State
smallstep' c = handle (c, emptyState)
  where handle (Skip, st) = st
        handle (c, st) = handle $ cstep c st

smallstep :: String -> IO State
smallstep file = do
  c <- parseFile file
  return $ smallstep' c

-- semantyka redukcyjna

contractA :: Aexp -> State -> Aexp
contractA (Var x) st = Nat $ get st x
contractA (Add (Nat n1) (Nat n2)) _ = Nat $ n1 + n2
contractA (Sub (Nat n1) (Nat n2)) _ = Nat $ n1 - n2
contractA (Mul (Nat n1) (Nat n2)) _ = Nat $ n1 * n2

contractB :: Bexp -> Bexp
contractB (Eq (Nat n1) (Nat n2)) = boolToBexp (n1 == n2)
contractB (Le (Nat n1) (Nat n2)) = boolToBexp (n1 <= n2)
contractB (Not BTrue) = BFalse
contractB (Not BFalse) = BTrue
contractB (And BTrue BTrue) = BTrue
contractB (And BTrue BFalse) = BFalse
contractB (And BFalse BTrue) = BFalse
contractB (And BFalse BFalse) = BFalse
contractB (Or BTrue BTrue) = BTrue
contractB (Or BTrue BFalse) = BTrue
contractB (Or BFalse BTrue) = BTrue
contractB (Or BFalse BFalse) = BFalse

contractC :: Com -> State -> (Com, State)
contractC (Ass x (Nat n)) st = (Skip, put st x n)
contractC (Seq Skip c2) st = (c2, st)
contractC (If BTrue c1 _) st = (c1, st)
contractC (If BFalse _ c2) st = (c2, st)
contractC loop@(While b c) st = ((If b (Seq c loop) Skip), st)

contract :: Phrase -> State -> (Phrase, State)
contract (Aexp a) st = (Aexp $ contractA a st, st)
contract (Bexp b) st = (Bexp $ contractB b, st)
contract (Com c) st = (Com c', st')
  where (c', st') = contractC c st

data Context = EmptyContext
             | Add1 Context Aexp
             | Add2 Aexp Context
             | Sub1 Context Aexp
             | Sub2 Aexp Context
             | Mul1 Context Aexp
             | Mul2 Aexp Context
             | Eq1 Context Aexp
             | Eq2 Aexp Context
             | Le1 Context Aexp
             | Le2 Aexp Context
             | CNot Context
             | And1 Context Bexp
             | And2 Bexp Context
             | Or1 Context Bexp
             | Or2 Bexp Context
             | CAss Varname Context
             | Seq1 Context Com
             | If1 Context Com Com

decomposeA :: Aexp -> (Phrase, Context)
decomposeA a@(Var _) = (Aexp a, EmptyContext)
decomposeA a@(Add (Nat _) (Nat _)) = (Aexp a, EmptyContext)
decomposeA (Add a1@(Nat _) a2) = (redex, Add2 a1 context)
  where (redex, context) = decomposeA a2
decomposeA (Add a1 a2) = (redex, Add1 context a2)
  where (redex, context) = decomposeA a1
decomposeA a@(Sub (Nat _) (Nat _)) = (Aexp a, EmptyContext)
decomposeA (Sub a1@(Nat _) a2) = (redex, Sub2 a1 context)
  where (redex, context) = decomposeA a2
decomposeA (Sub a1 a2) = (redex, Sub1 context a2)
  where (redex, context) = decomposeA a1        
decomposeA a@(Mul (Nat _) (Nat _)) = (Aexp a, EmptyContext)
decomposeA (Mul a1@(Nat _) a2) = (redex, Mul2 a1 context)
  where (redex, context) = decomposeA a2
decomposeA (Mul a1 a2) = (redex, Mul1 context a2)
  where (redex, context) = decomposeA a1

decomposeB :: Bexp -> (Phrase, Context)
decomposeB b@(Eq (Nat _) (Nat _)) = (Bexp b, EmptyContext)
decomposeB (Eq a1@(Nat _) a2) = (redex, Eq2 a1 context)
  where (redex, context) = decomposeA a2
decomposeB (Eq a1 a2) = (redex, Eq1 context a2)
  where (redex, context) = decomposeA a1
decomposeB b@(Le (Nat _) (Nat _)) = (Bexp b, EmptyContext)
decomposeB (Le a1@(Nat _) a2) = (redex, Le2 a1 context)
  where (redex, context) = decomposeA a2
decomposeB (Le a1 a2) = (redex, Le1 context a2)
  where (redex, context) = decomposeA a1
decomposeB b@(Not BTrue) = (Bexp b, EmptyContext)
decomposeB b@(Not BFalse) = (Bexp b, EmptyContext)
decomposeB (Not b') = (redex, CNot context)
  where (redex, context) = decomposeB b'
decomposeB b@(And BTrue BTrue) = (Bexp b, EmptyContext)
decomposeB b@(And BTrue BFalse) = (Bexp b, EmptyContext)
decomposeB b@(And BFalse BTrue) = (Bexp b, EmptyContext)
decomposeB b@(And BFalse BFalse) = (Bexp b, EmptyContext)
decomposeB (And BTrue b2) = (redex, And2 BTrue context)
  where (redex, context) = decomposeB b2
decomposeB (And BFalse b2) = (redex, And2 BFalse context)
  where (redex, context) = decomposeB b2        
decomposeB (And b1 b2) = (redex, And1 context b2)
  where (redex, context) = decomposeB b1
decomposeB b@(Or BTrue BTrue) = (Bexp b, EmptyContext)
decomposeB b@(Or BTrue BFalse) = (Bexp b, EmptyContext)
decomposeB b@(Or BFalse BTrue) = (Bexp b, EmptyContext)
decomposeB b@(Or BFalse BFalse) = (Bexp b, EmptyContext)
decomposeB (Or BTrue b2) = (redex, Or2 BTrue context)
  where (redex, context) = decomposeB b2
decomposeB (Or BFalse b2) = (redex, Or2 BFalse context)
  where (redex, context) = decomposeB b2        
decomposeB (Or b1 b2) = (redex, Or1 context b2)
  where (redex, context) = decomposeB b1        

decomposeC :: Com -> (Phrase, Context)
decomposeC c@(Ass _ (Nat _)) = (Com c, EmptyContext)
decomposeC (Ass x a) = (redex, CAss x context)
  where (redex, context) = decomposeA a
decomposeC c@(Seq Skip _) = (Com c, EmptyContext)
decomposeC (Seq c1 c2) = (redex, Seq1 context c2)
  where (redex, context) = decomposeC c1
decomposeC c@(If BTrue _ _) = (Com c, EmptyContext)        
decomposeC c@(If BFalse _ _) = (Com c, EmptyContext)
decomposeC (If b c1 c2) = (redex, If1 context c1 c2)
  where (redex, context) = decomposeB b
decomposeC c@(While _ _) = (Com c, EmptyContext)

plugA :: Phrase -> Context -> Aexp
plugA (Aexp a) EmptyContext = a
plugA p (Add1 context a2) = Add (plugA p context) a2
plugA p (Add2 a1 context) = Add a1 (plugA p context)
plugA p (Sub1 context a2) = Sub (plugA p context) a2
plugA p (Sub2 a1 context) = Sub a1 (plugA p context)
plugA p (Mul1 context a2) = Mul (plugA p context) a2
plugA p (Mul2 a1 context) = Mul a1 (plugA p context)

plugB :: Phrase -> Context -> Bexp
plugB (Bexp b) EmptyContext = b
plugB p (Eq1 context a2) = Eq (plugA p context) a2
plugB p (Eq2 a1 context) = Eq a1 (plugA p context)
plugB p (Le1 context a2) = Le (plugA p context) a2
plugB p (Le2 a1 context) = Le a1 (plugA p context)
plugB p (CNot context) = Not (plugB p context)
plugB p (And1 context b2) = And (plugB p context) b2
plugB p (And2 b1 context) = And b1 (plugB p context)
plugB p (Or1 context b2) = Or (plugB p context) b2
plugB p (Or2 b1 context) = Or b1 (plugB p context)

plugC :: Phrase -> Context -> Com
plugC (Com c) EmptyContext = c
plugC p (CAss x context) = Ass x (plugA p context)
plugC p (Seq1 context c2) = Seq (plugC p context) c2
plugC p (If1 context c1 c2) = If (plugB p context) c1 c2

redC :: Com -> State -> (Com, State)
redC c st = (plugC redex' context, st')
  where (redex, context) = decomposeC c
        (redex', st') = contract redex st

red_run' :: Com -> State
red_run' c = handle (c, emptyState)
  where handle (Skip, st) = st
        handle (c, st) = handle $ redC c st

red_run :: String -> IO State
red_run file = do
  c <- parseFile file
  return $ red_run' c

-- maszyna abstrakcyjna

data Operation = AOp1 Integer (Integer -> Integer -> Integer) -- add, sub, mul
               | AOp2 Aexp (Integer -> Integer -> Integer)
               | Comp1 Integer (Integer -> Integer -> Bool) -- eq, le
               | Comp2 Aexp (Integer -> Integer -> Bool)
               | BNot -- not
               | BOp1 Bool (Bool -> Bool -> Bool) -- and, or
               | BOp2 Bexp (Bool -> Bool -> Bool)                
               | Execute Com
               | AssignTo Varname
               | IfSelect Com Com

triple :: a -> b -> c -> (a, b, c)
triple a b c = (a, b, c)

trans :: Phrase -> State -> [Operation] -> (Phrase, State, [Operation])

trans (Aexp (Nat n)) st ((AOp2 a operator):ops) = triple (Aexp a) st $ (AOp1 n operator):ops
trans (Aexp (Nat n)) st ((AOp1 n' operator):ops) = triple (Aexp $ Nat $ operator n' n) st ops
trans (Aexp (Nat n)) st ((Comp2 a operator):ops) = triple (Aexp a) st $ (Comp1 n operator):ops
trans (Aexp (Nat n)) st ((Comp1 n' operator):ops) = triple (Bexp $ boolToBexp $ operator n' n) st ops
trans (Aexp (Nat n)) st ((AssignTo x):ops) = triple (Com Skip) (put st x n) ops
trans (Aexp (Var x)) st ops = triple (Aexp (Nat $ get st x)) st ops
trans (Aexp (Add a1 a2)) st ops = triple (Aexp a1) st $ (AOp2 a2 (+)):ops
trans (Aexp (Sub a1 a2)) st ops = triple (Aexp a1) st $ (AOp2 a2 (-)):ops
trans (Aexp (Mul a1 a2)) st ops = triple (Aexp a1) st $ (AOp2 a2 (*)):ops

trans (Bexp BTrue) st (BNot:ops) = triple (Bexp BFalse) st ops
trans (Bexp BTrue) st ((BOp2 b operator):ops) = triple (Bexp b) st $ (BOp1 True operator):ops
trans (Bexp BTrue) st ((BOp1 t operator):ops) = triple (Bexp $ boolToBexp $ operator t True) st ops
trans (Bexp BTrue) st ((IfSelect c1 c2):ops) = triple (Com c1) st ops
-- analogiczny zestaw dla BFalse
trans (Bexp BFalse) st (BNot:ops) = triple (Bexp BTrue) st ops
trans (Bexp BFalse) st ((BOp2 b operator):ops) = triple (Bexp b) st $ (BOp1 False operator):ops
trans (Bexp BFalse) st ((BOp1 t operator):ops) = triple (Bexp $ boolToBexp $ operator t False) st ops
trans (Bexp BFalse) st ((IfSelect c1 c2):ops) = triple (Com c2) st ops
--
trans (Bexp (Eq a1 a2)) st ops = triple (Aexp a1) st $ (Comp2 a2 (==)):ops
trans (Bexp (Le a1 a2)) st ops = triple (Aexp a1) st $ (Comp2 a2 (<=)):ops
trans (Bexp (Not b)) st ops = triple (Bexp b) st $ BNot:ops
trans (Bexp (And b1 b2)) st ops = triple (Bexp b1) st $ (BOp2 b2 (&&)):ops
trans (Bexp (Or b1 b2)) st ops = triple (Bexp b1) st $ (BOp2 b2 (||)):ops

trans (Com Skip) st ((Execute c):ops) = triple (Com c) st ops
trans (Com (Ass x a)) st ops = triple (Aexp a) st $ (AssignTo x):ops
trans (Com (Seq c1 c2)) st ops = triple (Com c1) st $ (Execute c2):ops
trans (Com (If b c1 c2)) st ops = triple (Bexp b) st $ (IfSelect c1 c2):ops
trans (Com loop@(While b c)) st ops = triple (Com (If b (Seq c loop) Skip)) st ops

am' :: Com -> State
am' c = handle ((Com c), emptyState, [])
  where handle ((Com Skip), st, []) = st
        handle (phrase, st, ops) = handle $ trans phrase st ops

am :: String -> IO State
am file = do
  c <- parseFile file
  return $ am' c
