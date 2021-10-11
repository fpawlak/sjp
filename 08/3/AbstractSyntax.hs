module AbstractSyntax where

type Varname = String
type GotoLabel = String

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
         | Goto GotoLabel
         | Block [(GotoLabel, Com)]
         deriving (Show, Eq)
