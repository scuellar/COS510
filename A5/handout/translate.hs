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
cast (ARROW TAGGED TAGGED) (TagFun e) = e
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
translateExp g (If e1 e2 e3) = 
    case translateExp g e1 of
        Nothing -> Nothing
        Just (e1', t1') ->
            if t1' == BOOL then 
                (case (translateExp g e2, translateExp g e3) of
                ((Just e2', t2'), (Just e3', t3')) -> if t2' /= t3' then Nothing else Just (If e1' e2' e3', t3')
                _ -> Nothing) else
            if t1' == TAGGED then
               (case (translateExp g e2, translateExp g e3) of
                ((Just e2', t2'), (Just e3', t3')) -> if t2' /= t3' then Nothing else Just (If (cast BOOL e1') e2' e3', t3')
                _ -> Nothing) 
            else Nothing

-- PrimOps
translateExp g (PrimOp Equal  [e1, e2]) = 
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', t1'), Just (e2', t2')) -> 
            if t1' == INT && t2' == INT then Just (PrimOp Equal [e1', e2'], BOOL) else
            if t1' == TAGGED && t2' == TAGGED then Just (PrimOp Equal [cast INT e1', cast INT e2'], BOOL)
            if t1' == INT && t2' == TAGGED then Just (PrimOp Equal [e1', cast INT e2'], BOOL)
            if t1' == TAGGED && t2' == INT then Just (PrimOp Equal [cast INT e1', e2'], BOOL)
            else Nothing
        _ -> Nothing
translateExp g (PrimOp Plus   [e1, e2]) =
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', t1'), Just (e2', t2')) -> 
            if t1' == INT && t2' == INT then Just (PrimOp Plus [e1', e2'], INT) else
            if t1' == TAGGED && t2' == TAGGED then Just (PrimOp Plus [cast INT e1', cast INT e2'], INT)
            if t1' == INT && t2' == TAGGED then Just (PrimOp Plus [e1', cast INT e2'], INT)
            if t1' == TAGGED && t2' == INT then Just (PrimOp Plus [cast INT e1', e2'], INT)
            else Nothing
        _ -> Nothing
translateExp g (PrimOp Minus  [e1, e2]) = 
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', t1'), Just (e2', t2')) -> 
            if t1' == INT && t2' == INT then Just (PrimOp Minus [e1', e2'], INT) else
            if t1' == TAGGED && t2' == TAGGED then Just (PrimOp Minus [cast INT e1', cast INT e2'], INT)
            if t1' == INT && t2' == TAGGED then Just (PrimOp Minus [e1', cast INT e2'], INT)
            if t1' == TAGGED && t2' == INT then Just (PrimOp Minus [cast INT e1', e2'], INT)
            else Nothing
        _ -> Nothing
translateExp g (PrimOp Times  [e1, e2]) = 
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', t1'), Just (e2', t2')) -> 
            if t1' == INT && t2' == INT then Just (PrimOp Times [e1', e2'], INT) else
            if t1' == TAGGED && t2' == TAGGED then Just (PrimOp Times [cast INT e1', cast INT e2'], INT)
            if t1' == INT && t2' == TAGGED then Just (PrimOp Times [e1', cast INT e2'], INT)
            if t1' == TAGGED && t2' == INT then Just (PrimOp Times [cast INT e1', e2'], INT)
            else Nothing
        _ -> Nothing
translateExp g (PrimOp Negate [e1]) = 
    case translateExp g e1 of
        Just (e1', t1') -> 
            if t1' == INT then Just (PrimOp Negate [e1'], INT) else
            if t1' == TAGGED then Just (PrimOp Negate [cast INT e1'], INT)
            else Nothing
        _ -> Nothing
translateExp g (PrimOp _ _) = Nothing
-- Funs
translateExp g (Fun s1 s2 (ARROW t1 t1') t2 e) = if t1 /= t2 then Nothing else
    case translateExp (Map.insert s2 t2 (Map.insert s1 (ARROW t1 t1') g)) e of
        Just (e2', t2') -> if t2' /= t1' then Nothing else Just (Fun s1 s2 (ARROW t1 t1') t2 e2', ARROW t1 t1')
translateExp g (Fun _ _ _ _ _) = Nothing
-- Checks
translateExp g (Check e INT) = 
    case translateExp g e of
        Just (e', t') -> if t' == INT then Just (e', INT) else Nothing
        Nothing -> Nothing
translateExp g (Check e BOOL) = 
    case translateExp g e of
        Just (e', t') -> if t' == BOOL then Just (e', BOOL) else Nothing
        Nothing -> Nothing 
translateExp g (Check e (ARROW t1 t2)) = 
    case translateExp g e of
        Just (e', t') -> if t' == (ARROW t1 t2) then Just (e', ARROW t1 t2) else Nothing
        Nothing -> Nothing 
-- Untyped funs
translateExp g (UTFun String String Exp) = 
-- Apply
translateExp g (Apply e1 e2) = 
    case (translateExp g e1, translateExp g e2) of
        (Just e1' (ARROW t1 t1'), Just e2' t2') -> if t1 /= t2' then Nothing else Just (Apply e1' e2', t2') -- Is this the right case? Seems like we should reduce the expression where we can...
        _ -> Nothing
-- Vars
translateExp g (Var s) = if (Map.lookup s g == Just TAGGED) then Just (Var s, TAGGED) else Nothing
      
-- Nice use of point-free style here
translate :: D.Exp -> Maybe (M.Exp, M.Typ)
translate = translateExp Map.empty
