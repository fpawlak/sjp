import AbstractSyntax
import Parser
import qualified Common as Common

fix f = f (fix f)

type Denotation = Store -> Store

type Loc = Int

type EnvV = Varname -> Loc

putVar :: EnvV -> Varname -> Loc -> EnvV
putVar = Common.shadow

type EnvP = Procname -> (Integer -> Denotation)

putProc :: EnvP -> Procname -> (Integer -> Denotation) -> EnvP
putProc = Common.shadow

type Store = (Loc -> Integer, Loc)

mem :: Store -> (Loc -> Integer)
mem = fst

next :: Store -> Loc
next = snd

get :: Store -> Loc -> Integer
get sto l = fst sto l

put :: Store -> Loc -> Integer -> Store
put sto l n = (Common.shadow mem l n, next)
  where (mem, next) = sto

updateNext :: Store -> Store        
updateNext (mem, next) = (mem, next + 1)

declareVariable :: EnvV -> Store -> Varname -> Integer -> (EnvV, Store)
declareVariable envV sto x v = (envV', sto')
  where
    envV' = putVar envV x l
    sto' = updateNext $ put sto l v
    l = next sto
    
adapt :: (exp -> Common.State -> result) -> (exp -> EnvV -> Store -> result)
adapt eval exp envV sto = eval exp $ mem sto . envV

aeval = adapt Common.aeval
beval = adapt Common.beval

process :: Dec -> (EnvV, EnvP, Store) -> (EnvV, EnvP, Store)
process Empty stuff = stuff
process (Let x a ds) (envV, envP, sto) = process ds (envV', envP, sto')
  where (envV', sto') = declareVariable envV sto x v
        v = aeval a envV sto
process (Proc procname param c ds) (envV, envP, sto) =
  process ds (envV, putProc envP procname (fix f), sto)
  where f g arg sto = sem c envV' (putProc envP procname g) sto'
          where (envV', sto') = declareVariable envV sto param arg

deallocate :: Loc -> Store -> Store
deallocate next' (mem, next) = (mem, next')

cond :: (Store -> Bool) -> Denotation -> Denotation -> Denotation
cond bDen c1Den c2Den sto = if bDen sto then c1Den sto else c2Den sto

sem :: Com -> EnvV -> EnvP -> Denotation
sem Skip envV envP = id
sem (Ass x a) envV envP = \sto -> put sto l $ aeval a envV sto
  where l = envV x
sem (Seq c1 c2) envV envP = sem c2 envV envP . sem c1 envV envP
sem (If b c1 c2) envV envP = cond (beval b envV) (sem c1 envV envP) (sem c2 envV envP)
sem (While b c) envV envP = fix f
  where f g = cond (beval b envV) (g . (sem c envV envP)) id
sem (Block ds c) envV envP = \sto ->
  let (envV', envP', sto') = process ds (envV, envP, sto)
  in deallocate (next sto) $ sem c envV' envP' sto'
sem (Call procname a) envV envP = \sto ->
  deallocate (next sto) $ envP procname (aeval a envV sto) sto

run :: String -> IO Store
run file =
  do
    c <- parseFile file
    return $ sem c envV envP sto
  where envV = (\x -> error $ "undeclared variable " ++ x)
        envP = \procname -> error $ "undeclared procedure " ++ procname
        sto = (\_ -> 0, 0)

examine :: IO Store -> Loc -> IO Integer
examine ioStore l =
  do
    sto <- ioStore
    return $ mem sto l
