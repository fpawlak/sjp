module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Data.List

import AbstractSyntax

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "begin"
                                     , "in"
                                     , "end"
                                     , "call"
                                     , "let"
                                     , "proc"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", ":="
                                     , "<=", "=", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace



whileParser :: Parser Com
whileParser = whiteSpace >> statement

statement :: Parser Com
statement =   parens statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ makeSeqTree list
     where makeSeqTree [] = Skip
           makeSeqTree [stmt] = stmt
           makeSeqTree (stmt:stmts) = Seq stmt (makeSeqTree stmts)

statement' :: Parser Com
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> blockStmt
           <|> callStmt           
           <|> assignStmt


ifStmt :: Parser Com
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2
 
whileStmt :: Parser Com
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt
 
assignStmt :: Parser Com
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Ass var expr
 
skipStmt :: Parser Com
skipStmt = reserved "skip" >> return Skip

blockStmt :: Parser Com
blockStmt =
  do reserved "begin"
     dec <- declaration
     reserved "in"
     stmt <- statement
     reserved "end"
     return $ Block dec stmt

callStmt :: Parser Com
callStmt =
  do reserved "call"
     procname <- identifier
     arg <- aExpression
     return $ Call procname arg

declaration :: Parser Dec
declaration =   parens declaration
            <|> sequenceOfDecs

sequenceOfDecs =
  (do
    reserved "let"
    varname <- identifier
    reservedOp ":="
    expr <- aExpression
    rest <- sequenceOfDecs
    return $ Let varname expr rest)
  <|>
  (do
    reserved "proc"
    procname <- identifier
    varname <- identifier
    reservedOp "="
    stmt <- statement
    rest <- sequenceOfDecs
    return $ Proc procname varname stmt rest)
  <|>
  return Empty
      
aExpression :: Parser Aexp
aExpression = buildExpressionParser aOperators aTerm
 
bExpression :: Parser Bexp
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Infix  (reservedOp "*"   >> return (Mul    )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Add     )) AssocLeft]
             , [Infix  (reservedOp "-"   >> return (Sub     )) AssocLeft]
              ]
 
bOperators = [ [Prefix (reservedOp "not" >> return (Not     ))          ]
             , [Infix  (reservedOp "and" >> return (And     )) AssocLeft]
             , [Infix  (reservedOp "or"  >> return (Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM Nat integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return BTrue)
     <|> (reserved "false" >> return BFalse)
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ op a1 a2
 
relation =   (reservedOp "=" >> return Eq)
         <|> (reservedOp "<=" >> return Le)

parseString :: String -> Com
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
 
parseFile :: String -> IO Com
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
