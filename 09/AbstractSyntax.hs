module AbstractSyntax where

type Varname = String
type Procname = String

data Aexp = Nat Integer
          | Var Varname
          | Add Aexp Aexp
          | Sub Aexp Aexp
          | Mul Aexp Aexp
          deriving (Show, Eq)

data Bexp = BTrue
          | BFalse
          | Eq Aexp Aexp
          | Le Aexp Aexp
          | Not Bexp
          | And Bexp Bexp
          | Or Bexp Bexp
          deriving (Show, Eq)

data Com = Skip
         | Ass Varname Aexp
         | Seq Com Com
         | If Bexp Com Com
         | While Bexp Com
         | Block Dec Com
         | Call Procname Aexp
         deriving (Show, Eq)

data Dec = Empty
         | Let Varname Aexp Dec
         | Proc Procname Varname Com Dec
         deriving (Show, Eq)
