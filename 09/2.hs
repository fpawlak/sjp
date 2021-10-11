import AbstractSyntax
import Parser
import qualified Common as Common

type Loc = Int

type EnvV = Varname -> Loc

putVar :: EnvV -> Varname -> Loc -> EnvV
putVar = Common.shadow

type Closure = (Varname, Com, EnvV, EnvP)
data EnvP = EnvP (Procname -> Closure)

putProc :: EnvP -> Procname -> Closure -> EnvP
putProc (EnvP envP) procname closure = EnvP $ Common.shadow envP procname closure

getProc :: EnvP -> Procname -> Closure
getProc (EnvP envP) = envP

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
  process ds (envV, putProc envP procname closure, sto)
  where closure = (param, c, envV, envP)

deallocate :: Loc -> Store -> Store
deallocate next' (mem, next) = (mem, next')

trans :: EnvV -> EnvP -> Com -> Store -> Store
trans _ _ Skip sto = sto
trans envV _ (Ass x a) sto = put sto l v
  where l = envV x
        v = aeval a envV sto
trans envV envP (Seq c1 c2) sto =
  let sto'  = trans envV envP c1 sto
      sto'' = trans envV envP c2 sto'
  in sto''
trans envV envP (If b c1 c2) sto =
  if beval b envV sto then
    let sto' = trans envV envP c1 sto
    in sto'
  else
    let sto' = trans envV envP c2 sto
    in sto'
trans envV envP loop@(While b c) sto =
  if not $ beval b envV sto then sto
  else let sto' = trans envV envP c sto
           sto'' = trans envV envP loop sto'
       in sto''
trans envV envP (Block ds c) sto =
  let sto'' = trans envV' envP' c sto'
  in deallocate (next sto) sto''
  where (envV', envP', sto') = process ds (envV, envP, sto)
trans envV envP (Call procname a) sto =
  let sto'' = trans envV' (putProc procEnvP procname closure) body sto'
  in deallocate (next sto) sto''
  where closure@(param, body, procEnvV, procEnvP) = getProc envP procname
        (envV', sto') = declareVariable procEnvV sto param v
        v = aeval a envV sto

run :: String -> IO Store
run file =
  do
    c <- parseFile file
    return $ trans envV envP c sto
  where envV = (\x -> error $ "undeclared variable " ++ x)
        envP = EnvP (\procname -> error $ "undeclared procedure " ++ procname)
        sto = (\_ -> 0, 0)

examine :: IO Store -> Loc -> IO Integer
examine ioStore l =
  do
    sto <- ioStore
    return $ mem sto l

