{-
Syntax and Implementation of Natural Number Expressions
================================================
-}

module NatExp where

import Pi
import qualified Data.Map.Strict as M 
import qualified Data.IORef as R

data Nat = Z | S Nat
  deriving Show

data NatExp
  = NVar Name
  | NVal Nat
  | NatExp :+: NatExp
  | NatExp :*: NatExp
  deriving Show

-- Environments for interpreting boolean expressions
type NEnv = M.Map Name NatExp

type Counter = R.IORef Integer

nameGenerator :: Counter -> IO Name
nameGenerator counter = do
  n <- R.readIORef counter
  R.modifyIORef' counter (+1)
  return ("x" ++ show n)

-- TASK!
-- compile_n tchan fchan b
-- returns a process p that when juxtaposed with a compatible environment
compile_n :: IO Name -> Name -> NatExp -> IO Pi
compile_n fresh ch (NVar name)  = do
  top <- fresh
  --putStrLn ("var_"++(build_name (NVar name)))
  return $ RepInp ch (PVar top) (Out ("var_"++(build_name (NVar name))) (EVar top)) 
compile_n fresh ch (NVal Z)     = do
  --top <- fresh
  return $ Inp ch Wild Nil
compile_n fresh ch (NVal (S n)) = do
  top <- fresh
  next_ch <- fresh
  q <- compile_n fresh next_ch (NVal n)
  --putStrLn $ show (S n)
  --putStrLn $ "ch: " ++ ch
  --putStrLn $ "top: " ++ top
  --putStrLn $ "next_ch: " ++ next_ch
  return $ New next_ch unitT (RepInp ch (PVar top) ((Out top unitE)  :|: (Out next_ch (EVar top))) :|: q)
compile_n fresh ch (n1 :+: n2)  = do
  ch1 <- fresh
  ch2 <- fresh
  top <- fresh
  q1 <- compile_n fresh ch1 n1
  q2 <- compile_n fresh ch2 n2
  return (newChs [ch1, ch2] (TChan unitT) $ (RepInp ch (PVar top) ((Out ch1 (EVar top)) :|: (Out ch2 (EVar top))) :|: q1 :|: q2))
compile_n fresh ch (n1 :*: n2)  = do
  pchan <- fresh
  ch1 <- fresh
  ch2 <- fresh
  top <- fresh
  q1 <- compile_n fresh ch1 n1
  q2 <- compile_n fresh ch2 n2
  return (newChs [ch1, ch2] (TChan unitT) $ New pchan unitT $ (RepInp ch (PVar top) ((Out ch1 (EVar pchan)) :|: (RepInp pchan unitP (Out ch2 (EVar top)))) :|: q1 :|: q2))


--((Out (build_name n1) (PVar "top")) :|: (Out (build_name n2) (PVar "top")))

build_name :: NatExp -> Name
build_name (NVar name)  = name
build_name (NVal (Z))   = "x"
build_name (NVal (S n)) = (build_name (NVal n)) ++ "#"
build_name (n1 :+: n2)  = build_name n1 ++ "+" ++ build_name n2
build_name (n1 :*: n2)  = build_name n1 ++ "*" ++ build_name n2

--Some Helper funcitons:
--Creates new channels from a list
newChs :: [Name] -> Typ -> Pi -> Pi
newChs [] _ p = p
newChs (ch:lch) t p = New ch t (newChs lch t p)

--Receives unit from first channel and sends unit to the the second channel
relayU :: Name -> Name -> Pi
relayU ch1 ch2 = Inp ch1 (PVar "_temp_var!") (Out ch2 unitE)


--Server: Receives unit from first channel and sends unit to the the second channel
relayU_serv :: Name -> Name -> Pi
relayU_serv ch1 ch2 = RepInp ch1 (PVar "_temp_var!") (Out ch2 unitE)

--Receives unit from a list of channels and then sends unit to the the last channel
multiRelay :: [Name] -> Name -> Pi
multiRelay [] ch = (Out ch unitE)
multiRelay (ch1:lch) ch2 = Inp ch1 (PVar "_temp_var!") (multiRelay lch ch2)



-- TASK!
-- compile a natural number variable environment into a process that
-- communicates with a compiled Natural number expression containing free
-- variables from the environment
--compile_nenv :: NEnv -> Pi -> Pi
--compile_nenv nenv p = compile_nlist (M.toList nenv) p where
--  compile_nlist [] p = p
--  compile_nlist ((str, n):lpair) p =  newChs [query_ch, true_ch, false_ch] unitT ( relayU_serv query_ch ans_ch :|: compile_nlist lpair p) where
--    query_ch = str++"_query_"
--    ans_ch = str++"_"++show(n)++"_"
--    true_ch = str++"_True_"
--    false_ch = str++"_False_"

compile_nenv :: IO Name -> NEnv -> Pi -> IO Pi
compile_nenv fresh nenv p = compile_nlist (M.toList nenv) where
    compile_nlist [] = do return $ p
    compile_nlist ((str, n):lpair) = do 
        newchan <- fresh
        var <- compile_n fresh newchan n
        q <- compile_nlist lpair
        --putStrLn ("reply"++str)
        --putStrLn ("var_"++str)
        --putStrLn $ show n
        return $ New ("var_"++str) (TChan unitT) $
                 New newchan (TChan unitT) $
                 ((RepInp ("var_"++str) (PVar ("reply"++str)) (var :|: (Out newchan (EVar ("reply"++str))) )) :|: q)

getNames :: NatExp -> [Name]
getNames (NVar name)    = [build_name (NVar name)]
getNames (NVal (Z))     = [build_name (NVal Z)]
getNames (NVal (S n))   = (build_name (NVal (S n))):(getNames (NVal n))
getNames (n1 :+: n2)    = (getNames n1) ++ (getNames n2)
getNames (n1 :*: n2)    = (getNames n1) ++ (getNames n2)

start_nat :: NEnv -> NatExp -> IO ()
start_nat nenv nexp = do
    r <- R.newIORef 0
    let fresh = nameGenerator r
    let topchan = "top"
    let nchan = build_name nexp
    q <- compile_n fresh nchan nexp
    q1 <- compile_nenv fresh nenv q
    let pi = New topchan unitT $
             New nchan (TChan unitT) $
             --newChs (getNames nexp) unitT $
             q1 :|:
             Out nchan (EVar topchan) :|:
             RepInp topchan unitP (printer "#")  
    start pi

start_nat_simple :: NEnv -> NatExp -> IO ()
start_nat_simple nenv nexp = do
    r <- R.newIORef 0
    let fresh = nameGenerator r
    let topchan = "top"
    let nchan = build_name nexp
    q <- compile_n fresh nchan nexp
    let pi = New topchan unitT $
             New nchan (TChan unitT) $
             --newChs (getNames nexp) unitT $
             q :|:
             Out nchan (EVar topchan) :|:
             RepInp topchan unitP (printer "#")  
    start pi

