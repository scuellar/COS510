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
translateExp g (Int i) = Just (Int i, INT)
-- Bools
translateExp g (Bool b) = Just (Bool b, BOOL)
-- Ifs
translateExp g (If e1 e2 e3) = case translateExp g e1 of
                                Just 
-- PrimOps
translateExp g (PrimOp Equal  [e1, e2]) = 
translateExp g (PrimOp Plus   [e1, e2]) =
translateExp g (PrimOp Minus  [e1, e2]) = 
translateExp g (PrimOp Times  [e1, e2]) = 
translateExp g (PrimOp Negate [e1]) = 
translateExp g (PrimOp _ _) = 
-- Funs
translateExp g (Fun s1 s2 t1 t2 e) = 
-- Checks
translateExp g (Check e INT) = case translateExp g e of
                                Just (e', t') -> if t == INT then Just (e', INT)
                                Nothing -> Nothing
let trans = translateExp g e in if trans == Just INT
translateExp g (Check _ INT) = 
translateExp g (Check (Bool b) BOOL) = 
translateExp g (Check _ BOOL) = 
translateExp g (Check e (ARROW t1 t2)) = 
-- Untyped funs
translateExp g (UTFun String String Exp) = 
-- Apply
translateExp g (Apply e1 e2) = 
-- Vars
translateExp g (Var s) = if (Map.lookup s g == Just TAGGED) then Just (Var s, TAGGED) else Nothing
      
-- Nice use of point-free style here
translate :: D.Exp -> Maybe (M.Exp, M.Typ)
translate = translateExp Map.empty
