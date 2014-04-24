module Translate where

import TMinML as M
import EDMinML as D

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad (guard)

-- Take type, expression, returns a tagged expression
tagify :: M.Typ -> M.Exp -> M.Exp
-- tagify t e =  ...
tagify INT e = TagInt e
tagify BOOL e = TagBool e
--tagify TAGGED e =
tagify (ARROW TAGGED TAGGED) e = TagFun e
--tagify (ARROW t1 t2) e = 
tagify _ _ = error "Called tagify with something that isn't taggable!"

-- Takes type, expression, casts it to that type
-- Int and Bool, resulting thing wouldn't type check - maybe we want to reject, maybe never call that way
-- cast Int tagged_bool(True) --> as_int (tagged_bool True)
-- This makes sense - it type checks, but it will fail at runtime
cast :: M.Typ -> M.Exp -> M.Exp
-- cast t e = ...
cast INT (TagInt e) = e
cast BOOL (TagBool e) = e
--cast TAGGED e = Tag
cast ARROW (TagFun e) = e
cast _ _ = error "Called cast with something that isn't castable!"

translateOp :: D.PrimOp -> M.PrimOp
translateOp D.Equal = M.Equal
translateOp D.Plus = M.Plus
translateOp D.Minus = M.Minus
translateOp D.Times = M.Times
translateOp D.Negate = M.Negate

translateType :: D.Typ -> M.Typ
translateType D.INT = M.INT
translateType D.BOOL = M.BOOL
translateType (D.ARROW t1 t2) = M.ARROW t1' t2'
  where
    t1' = translateType t1
    t2' = translateType t2

-- Inference rules w/ the four questions marks for the expression
-- probably checking (Int i), (Bool b) isn't enough - need to do translateExp on the subexpression
translateExp :: Map.Map String M.Typ -> D.Exp -> Maybe (M.Exp, M.Typ)
-- translateExp g e = ...
-- Ints
translateExp m (Int i) = Just (Int i, INT)
translateExp m _ = Nothing
-- Bools
translateExp m (Bool b) = Just (Bool b, BOOL)
translateExp m _ = Nothing
-- Ifs
translateExp m (If e1 e2 e3) = case (translateExp m e1, translateExp m e2, translateExp m e3) of
                                    (Just (e1', BOOL), Just (e2', t2), Just (e3', t2)) -> Just (If e1' e2' e3', t2)
                                    _ -> Nothing
-- PrimOps
translateExp m (PrimOp Equal  [e1, e2]) = case (translateExp m e1, translateExp m e2) of
                                                (Just (e1', INT), Just (e2', INT)) -> Just (PrimOp Equal [e1', e2'], BOOL)
                                                _ -> Nothing
translateExp m (PrimOp Plus   [e1, e2]) = case (translateExp m e1, translateExp m e2) of
                                                (Just (e1', INT), Just (e2', INT)) -> Just (PrimOp Plus [e1', e2'], INT)
                                                _ -> Nothing 
translateExp m (PrimOp Minus  [e1, e2]) = case (translateExp m e1, translateExp m e2) of
                                                (Just (e1', INT), Just (e2', INT)) -> Just (PrimOp Minus [e1', e2'], INT)
                                                _ -> Nothing 
translateExp m (PrimOp Times  [e1, e2]) = case (translateExp m e1, translateExp m e2) of
                                                (Just (e1', INT), Just (e2', INT)) -> Just (PrimOp Times [e1', e2'], INT)
                                                _ -> Nothing 
translateExp m (PrimOp Negate [e1]) = case (translateExp m e1) of
                                            Just (e1', INT) -> Just (PrimOp Negate [e1'], INT)
                                                _ -> Nothing 
translateExp m (PrimOp _ _) = Nothing
-- Funs
translateExp m (Fun s1 s2 t1 t2 e) = let type1 = translateType t1
    in let type2 = translateType t2
    in case (translateExp (Map.insert s2 type2 (Map.insert s1 type1 m)) e) of
                                            Just (e', type2) -> Just ((Fun s1 s2 type1 type2 e'), ARROW type1 type2)
-- Checks
translateExp m (Check (Int i) INT) = Just (Int i, INT)
translateExp m (Check _ INT) = Nothing
translateExp m (Check (Bool b) BOOL) = Just (Bool b, BOOL)
translateExp m (Check _ BOOL) = Nothing
translateExp m (Check e (ARROW t1 t2)) = case (translateExp m e) of 
                                            (Just ((Fun s1 s2 t1 t2 e'), ARROW t1 t2)) -> Just ((Fun s1 s2 t1 t2 e'), ARROW t1 t2)
                                            _ -> Nothing
-- Untyped funs
translateExp m (UTFun String String Exp)
-- Apply
translateExp m (Apply e1 e2) = case (translateExp m e1, translateExp m e2) of
                                    (Just (e1', t1), Just (e2' t2)) -> Just ((Apply e1' e2'), t2)
                                    _ -> Nothing
-- Vars
translateExp m (Var s) = Just (Var s, 
      
-- Nice use of point-free style here
translate :: D.Exp -> Maybe (M.Exp, M.Typ)
translate = translateExp Map.empty
