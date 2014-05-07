{-
Syntax and Implementation of Boolean Expressions
================================================
-}

module BoolExp where

import Pi
import qualified Data.Map.Strict as M 

data BoolExp
  = BVar Name
  | BVal Bool
  | BoolExp :&&: BoolExp
  | BoolExp :||: BoolExp
  | Not BoolExp
  deriving Show

-- Environments for interpreting boolean expressions
type BEnv = M.Map Name Bool

-- TASK!
-- compile_b tchan fchan b
-- returns a process p that when juxtaposed with a compatible environment
-- sends a message on tchan if the boolean expression evaluates to true
-- sends a message on fchan if the boolean expression evaluates to false
compile_b :: Name -> Name -> BoolExp -> Pi
compile_b tchan fchan (BVar str) = undefined
compile_b tchan fchan (BVal b) = if b then (Out tchan unitE) else (Out fchan unitE)
compile_b tchan fchan (b1 :&&: b2) = newChs ["_and_chan_left_true", "_and_chan_left_false","_and_chan_right_true", "_and_chan_right_false"] unitT
                                               ((compile_b "_and_chan_left_true" "_and_chan_left_false" b1) :|:
                                                (compile_b "_and_chan_right_true" "_and_chan_right_false" b2) :|:
                                                (relayU "_and_chan_left_false" fchan) :|:
                                                (relayU "_and_chan_right_false" fchan) :|:
                                                (multiRelay ["_and_chan_left_true", "_and_chan_right_true"] tchan) :|:
                                                (multiRelay ["_and_chan_right_true", "_and_chan_left_true"] tchan))
compile_b tchan fchan (b1 :||: b2) = newChs ["_or_chan_left_true", "_or_chan_left_false","_or_chan_right_true", "_or_chan_right_false"] unitT
                                               ((compile_b "_or_chan_left_true" "_or_chan_left_false" b1) :|:
                                                (compile_b "_or_chan_right_true" "_or_chan_right_false" b2) :|:
                                                (relayU "_or_chan_left_true" tchan) :|:
                                                (relayU "_or_chan_right_true" tchan) :|:
                                                (multiRelay ["_or_chan_left_false", "_or_chan_right_false"] tchan) :|:
                                                (multiRelay ["_or_chan_right_false", "_or_chan_left_false"] tchan))
compile_b tchan fchan (Not b) = newChs ["_not_chan_true", "_not_chan_false"] unitT
                                               ((compile_b "_not_chan_true" "_not_chan_false" b) :|:
                                                (relayU "_not_chan_true" fchan) :|:
                                                (relayU "_not_chan_false" tchan))

--Some Helper funcitons:
--Creates new channels from a list
newChs :: [Name] -> Typ -> Pi -> Pi
newChs [] _ p = p
newChs (ch:lch) t p = New ch t (newChs lch t p)

--Receives unit from first channel and sends unit to the the second channel
relayU :: Name -> Name -> Pi
relayU ch1 ch2 = Inp ch1 (PVar "_temp_var!") (Out ch2 unitE)

--Receives unit from a list of channels and then sends unit to the the last channel
multiRelay :: [Name] -> Name -> Pi
multiRelay [] ch = (Out ch unitE)
multiRelay (ch1:lch) ch2 = Inp ch1 (PVar "_temp_var!") (multiRelay lch ch2)





-- TASK!
-- compile a boolean variable environment into a process that
-- communicates with a compiled Boolean expression containing free
-- variables from the environment
compile_benv :: BEnv -> Pi -> Pi
compile_benv benv p = undefined

start_bool :: BEnv -> BoolExp -> IO ()
start_bool benv bexp = 
  start pi
    where
      tchan = "t"
      fchan = "f"   
      pi = New tchan unitT $ 
           New fchan unitT $ 
           compile_benv benv (compile_b tchan fchan bexp) :|:
           Inp tchan unitP (printer "true") :|:
           Inp fchan unitP (printer "false")
           
