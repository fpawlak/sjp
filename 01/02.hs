type Varname = String

data Aexp = Nat Integer | Var Varname | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp deriving(Eq)

data Bexp = BTrue | BFalse | Eq Aexp Aexp | Le Aexp Aexp | Not Bexp | And Bexp Bexp | Or Bexp Bexp deriving(Eq)

data Com = Skip | Ass Varname Aexp | Seq Com Com | If Bexp Com Com | While Bexp Com deriving(Eq)

data State = State [(Varname, Integer)]

instance Eq State where
  st1@(State l1) == st2@(State l2) = all (\varname -> get st1 varname == get st2 varname) varnames
    where varnames = map fst l1 ++ map fst l2

get :: State -> Varname -> Integer
get (State []) x = 0
get (State ((var, val):t)) x = if var == x then val else get (State t) x

put :: State -> Varname -> Integer -> State
put (State l) x n = State ((x, n):l)

type Ajudgment = (Aexp, State, Integer)
type Bjudgment = (Bexp, State, Bool)
type Cjudgment = (Com, State, State)
type Atree = (Ajudgment, [DerivationTree])
type Btree = (Bjudgment, [DerivationTree])
type Ctree = (Cjudgment, [DerivationTree])
data DerivationTree = Atree Atree | Btree Btree | Ctree Ctree

checkAtree :: Atree -> Maybe Ajudgment
checkAtree (j@(Nat(n1), _, n2), []) =
  if n1 == n2 then Just j
  else Nothing
checkAtree (j@(Var(x), state, n), []) =
  if n == get state x then Just j
  else Nothing
checkAtree (j@(Add a1 a2, state, n), [Atree at1, Atree at2]) =
  case (checkAtree at1, checkAtree at2) of
    (Just (a1', state1, n1),
     Just (a2', state2, n2)) ->
      if a1' == a1 &&
         state1 == state &&
         a2' == a2 &&
         state2 == state &&
         n1 + n2 == n
      then Just j
      else Nothing
    _ -> Nothing
checkAtree (j@(Sub a1 a2, state, n), [Atree at1, Atree at2]) =
  case (checkAtree at1, checkAtree at2) of
    (Just (a1', state1, n1),
     Just (a2', state2, n2)) ->
      if a1' == a1 &&
         state1 == state &&
         a2' == a2 &&
         state2 == state &&
         n1 - n2 == n
      then Just j
      else Nothing
    _ -> Nothing
checkAtree (j@(Mul a1 a2, state, n), [Atree at1, Atree at2]) =
  case (checkAtree at1, checkAtree at2) of
    (Just (a1', state1, n1),
     Just (a2', state2, n2)) ->
      if a1' == a1 &&
         state1 == state &&
         a2' == a2 &&
         state2 == state &&
         n1 + n2 == n
      then Just j
      else Nothing
    _ -> Nothing    
checkAtree _ = Nothing       

checkBtree :: Btree -> Maybe Bjudgment
checkBtree (j@(BTrue, _, True), []) = Just j
checkBtree (j@(BFalse, _, False), []) = Just j
checkBtree (j@(Eq a1 a2, state, t), [Atree at1, Atree at2]) =
  case (checkAtree at1, checkAtree at2) of
    (Just (a1', state1, n1),
     Just (a2', state2, n2)) ->
      if a1' == a1 &&
         state1 == state &&
         a2' == a2 &&
         state2 == state &&
         t == (n1 == n2)
      then Just j
      else Nothing
    _ -> Nothing
checkBtree (j@(Le a1 a2, state, t), [Atree at1, Atree at2]) =
  case (checkAtree at1, checkAtree at2) of
    (Just (a1', state1, n1),
     Just (a2', state2, n2)) ->
      if a1' == a1 &&
         state1 == state &&
         a2' == a2 &&
         state2 == state &&
         t == (n1 <= n2)
      then Just j
      else Nothing
    _ -> Nothing
checkBtree (j@(Not b, state, not_t), [Btree bt]) =
  case checkBtree bt of
    Just (b', state', t) ->
      if b' == b &&
         state' == state &&
         not_t == not t
      then Just j
      else Nothing
    Nothing -> Nothing
checkBtree (j@(And b1 b2, state, t), [Btree bt1, Btree bt2]) =
  case (checkBtree bt1, checkBtree bt2) of
    (Just (b1', state1, t1),
     Just (b2', state2, t2)) ->
      if b1' == b1 &&
         state1 == state &&
         b2' == b2 &&
         state2 == state &&
         t == t1 && t2
      then Just j
      else Nothing
    _ -> Nothing
checkBtree (j@(Or b1 b2, state, t), [Btree bt1, Btree bt2]) =
  case (checkBtree bt1, checkBtree bt2) of
    (Just (b1', state1, t1),
     Just (b2', state2, t2)) ->
      if b1' == b1 &&
         state1 == state &&
         b2' == b2 &&
         state2 == state &&
         t == t1 || t2
      then Just j
      else Nothing
    _ -> Nothing        
checkBtree _ = Nothing

checkCtree :: Ctree -> Maybe Cjudgment
checkCtree (j@(Skip, state, state'), []) =
  if state == state' then Just j else Nothing
checkCtree (j@(Ass var a, state1, state2), [Atree at]) =
  case checkAtree at of
    Just (a', state1', n) ->
      if a' == a &&
         state1' == state1 &&
         state2 == put state1 var n
      then Just j
      else Nothing
    Nothing -> Nothing
checkCtree (j@(Seq c1 c2, state1, state3), [Ctree ct1, Ctree ct2]) =
  case (checkCtree ct1, checkCtree ct2) of
    (Just (c1', state1', state2),
     Just (c2', state2', state3')) ->
      if c1' == c1 &&
         state1' == state1 &&
         state2 == state2' &&
         c2' == c2 &&
         state3' == state3
      then Just j
      else Nothing
    _ -> Nothing
checkCtree (j@(If b c1 c2, state1, state2), [Btree bt, Ctree ct]) =
  case (checkBtree bt, checkCtree ct) of
    (Just (b', state1', t),
     Just (c', state1'', state2')) ->
      if b' == b &&
         state1' == state1 &&
         state1'' == state1 &&
         state2' == state2 &&
         c' == (if t then c1 else c2)
      then Just j
      else Nothing
    _ -> Nothing
checkCtree (j@(While b c, state, state'), [Btree bt]) =
  case checkBtree bt of
    Just (b', state'', False) ->
      if b' == b &&
         state' == state &&
         state'' == state
      then Just j
      else Nothing
    _ -> Nothing
checkCtree (j@(loop@(While b c), state1, state3), [Btree bt, Ctree ct1, Ctree ct2]) =
  case (checkBtree bt, checkCtree ct1, checkCtree ct2) of
    (Just (b', state1', True),
     Just (c', state1'', state2),
     Just (loop', state2', state3')) ->
      if b' == b &&
         state1' == state1 &&
         c' == c &&
         state1'' == state1 &&
         state2 == state2' &&
         loop' == loop &&
         state3' == state3
      then Just j
      else Nothing
    _ -> Nothing
checkCtree _ = Nothing
