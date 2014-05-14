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
type NEnv = M.Map Name Nat

type Counter = R.IORef Integer

nameGenerator :: Counter -> IO Name
nameGenerator counter = do
  n <- R.readIORef counter
  R.modifyIORef' counter (+1)
  return ("x" ++ show n)

-- TASK!
-- compile_n tchan fchan b
-- returns a process p that when juxtaposed with a compatible environment
compile_n :: IO Name -> NatExp -> IO Pi
compile_n fresh (NVar name)  = undefined
compile_n fresh (NVal Z)     = do return (RepInp (build_name (NVal Z)) (PVar "top") Nil)
compile_n fresh (NVal (S n)) = do
                            q <- compile_n fresh (NVal n)
                            return (RepInp (build_name (NVal (S n))) (PVar "top") ((Out "top" unitE)  :|: (Out (build_name (NVal n)) (EVar "top"))) :|: q)
compile_n fresh (n1 :+: n2)  = do
                         q1 <- compile_n fresh n1
                         q2 <- compile_n fresh n2
                         let chanplus = (build_name (n1 :+: n2))
                         let chan1 = (build_name n1)
                         let chan2 = (build_name n2)
                         return (newChs [chan1, chan2] (TChan unitT) $ (RepInp chanplus (PVar "top") ((Out chan1 (EVar "top")) :|: (Out chan2 (EVar "top"))) :|: q1 :|: q2))
compile_n fresh (n1 :*: n2)  = do
                         pchan <- fresh
                         q1 <- compile_n fresh n1
                         q2 <- compile_n fresh n2
                         let chanmul = (build_name (n1 :*: n2))
                         let chan1 = (build_name n1)
                         let chan2 = (build_name n2)
                         return (newChs [chan1, chan2] (TChan unitT) $ New pchan unitT $ (RepInp chanmul (PVar "top") ((Out chan1 (EVar pchan)) :|: (RepInp pchan unitP (Out chan2 (EVar "top")))) :|: q1 :|: q2))


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
compile_nenv :: NEnv -> Pi -> Pi
compile_nenv nenv p = compile_nlist (M.toList nenv) p where
  compile_nlist [] p = p
  compile_nlist ((str, n):lpair) p =  newChs [query_ch, true_ch, false_ch] unitT ( relayU_serv query_ch ans_ch :|: compile_nlist lpair p) where
    query_ch = str++"_query_"
    ans_ch = str++"_"++show(n)++"_"
    true_ch = str++"_True_"
    false_ch = str++"_False_"

getNames :: NatExp -> [Name]
getNames (NVar name)    = [build_name (NVar name)]
getNames (NVal (Z))     = [build_name (NVal Z)]
getNames (NVal (S n))   = (build_name (NVal (S n))):(getNames (NVal n))
getNames (n1 :+: n2)    = (getNames n1) ++ (getNames n2)
getNames (n1 :*: n2)    = (getNames n1) ++ (getNames n2)

--start_nat :: NEnv -> NatExp -> IO ()
--start_nat nenv nexp = start pi
--    where
--        topchan = "top"
--        nchan = build_name nexp
--        pi = New topchan unitT $
--             New nchan (TChan unitT) $
--             newChs (getNames nexp) unitT $
--             compile_nenv nenv (compile_n nexp) :|:
--             Out nchan (EVar topchan) :|:
--             RepInp topchan unitP (printer "#")           

start_nat_simple :: NEnv -> NatExp -> IO ()
start_nat_simple nenv nexp = do
    r <- R.newIORef 0
    let fresh = nameGenerator r
    q <- compile_n fresh nexp
    let topchan = "top"
    let nchan = build_name nexp
    let pi = New topchan unitT $
             New nchan (TChan unitT) $
             newChs (getNames nexp) unitT $
             q :|:
             Out nchan (EVar topchan) :|:
             RepInp topchan unitP (printer "#")  
    start pi

--start_nat nenv (NVar Z) = start Nil
--start_nat nenv (NVar (S n)) = start pi
--    where
--        nchan = build_name n
--        topchan = "top"
--        pi = NewChs (recNewChs (S n)) (TChan UnitT) $
--             compile_nenv nenv (compile_n topchan (NVar (S n))) :|:
--             RepInp topchan unitP (printer "#")
