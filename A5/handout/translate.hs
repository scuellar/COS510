module Translate where

import TMinML as M
import EDMinML as D

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad (guard)

-- Take type, expression, returns a tagged expression
tagify :: M.Typ -> M.Exp -> M.Exp
tagify M.INT e = M.TagInt e
tagify M.BOOL e = M.TagBool e
tagify (M.ARROW M.TAGGED M.TAGGED) e = M.TagFun e
tagify _ _ = error "Called tagify with something that isn't taggable!"

-- Takes type, expression, casts it to that type
-- Int and Bool, resulting thing wouldn't type check - maybe we want to reject, maybe never call that way
-- cast Int tagged_bool(True) --> as_int (tagged_bool True)
-- This makes sense - it type checks, but it will fail at runtime
cast :: M.Typ -> M.Exp -> M.Exp
cast M.INT (M.TagInt e) = e
cast M.BOOL (M.TagBool e) = e
cast (M.ARROW M.TAGGED M.TAGGED) (M.TagFun e) = e
cast M.INT e = M.AsInt e
cast M.BOOL e = M.AsBool e
cast (M.ARROW M.TAGGED M.TAGGED) e = M.AsFun e
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
translateExp g (D.Int i) = Just (M.Int i, M.INT)
-- Bools
translateExp g (D.Bool b) = Just (M.Bool b, M.BOOL)
-- Ifs
translateExp g (D.If e1 e2 e3) = 
    case (translateExp g e1, translateExp g e2, translateExp g e3) of
        (Just (e1', M.BOOL), (Just (e2', t2')), (Just (e3', t3'))) -> 
            if t2' == t3' 
            then Just (M.If e1' e2' e3', t3')
            else Just (M.If e1' (tagify t2' e2') (tagify t3' e3'), M.TAGGED)
        (Just (e1', M.TAGGED), (Just (e2', t2')), (Just (e3', t3'))) -> 
            if t2' == t3'
            then Just (M.If (cast M.BOOL e1') e2' e3', t3')
            else Just (M.If (cast M.BOOL e1') (tagify t2' e2') (tagify t3' e3'), M.TAGGED)
        (Just (e1', t1'), (Just (e2', t2')), (Just (e3', t3'))) -> 
            if t2' == t3'
            then Just (M.If (cast M.BOOL (tagify t1' e1')) e2' e3', t3')
            else Just (M.If (cast M.BOOL (tagify t1' e1')) (tagify t2' e2') (tagify t2' e3'), M.TAGGED)
        _ -> Nothing
-- PrimOps
translateExp g (D.PrimOp D.Equal  [e1, e2]) = 
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', M.INT), Just (e2', M.INT))      -> Just (M.PrimOp M.Equal [e1', e2'], M.BOOL)
        (Just (e1', M.TAGGED), Just (e2', M.TAGGED))-> Just (M.PrimOp M.Equal [cast M.INT e1', cast M.INT e2'], M.BOOL)
        (Just (e1', M.INT), Just (e2', M.TAGGED))   -> Just (M.PrimOp M.Equal [e1', cast M.INT e2'], M.BOOL)
        (Just (e1', M.TAGGED), Just (e2', M.INT))   -> Just (M.PrimOp M.Equal [cast M.INT e1', e2'], M.BOOL)
        (Just (e1', t1'), Just (e2', t2'))          -> Just (M.PrimOp M.Equal [cast M.INT (tagify t1' e1'), cast M.INT (tagify t2' e2')], M.INT)
        _ -> Nothing
translateExp g (D.PrimOp D.Plus   [e1, e2]) =
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', M.INT), Just (e2', M.INT))      -> Just (M.PrimOp M.Plus [e1', e2'], M.INT)
        (Just (e1', M.TAGGED), Just (e2', M.TAGGED))-> Just (M.PrimOp M.Plus [cast M.INT e1', cast M.INT e2'], M.INT)
        (Just (e1', M.INT), Just (e2', M.TAGGED))   -> Just (M.PrimOp M.Plus [e1', cast M.INT e2'], M.INT)
        (Just (e1', M.TAGGED), Just (e2', M.INT))   -> Just (M.PrimOp M.Plus [cast M.INT e1', e2'], M.INT)
        (Just (e1', t1'), Just (e2', t2'))          -> Just (M.PrimOp M.Plus [cast M.INT (tagify t1' e1'), cast M.INT (tagify t2' e2')], M.INT)
        _ -> Nothing
translateExp g (D.PrimOp D.Minus  [e1, e2]) = 
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', M.INT), Just (e2', M.INT))      -> Just (M.PrimOp M.Minus [e1', e2'], M.INT)
        (Just (e1', M.TAGGED), Just (e2', M.TAGGED))-> Just (M.PrimOp M.Minus [cast M.INT e1', cast M.INT e2'], M.INT)
        (Just (e1', M.INT), Just (e2', M.TAGGED))   -> Just (M.PrimOp M.Minus [e1', cast M.INT e2'], M.INT)
        (Just (e1', M.TAGGED), Just (e2', M.INT))   -> Just (M.PrimOp M.Minus [cast M.INT e1', e2'], M.INT)
        (Just (e1', t1'), Just (e2', t2'))          -> Just (M.PrimOp M.Minus [cast M.INT (tagify t1' e1'), cast M.INT (tagify t2' e2')], M.INT)
        _ -> Nothing
translateExp g (D.PrimOp D.Times  [e1, e2]) = 
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', M.INT), Just (e2', M.INT))      -> Just (M.PrimOp M.Times [e1', e2'], M.INT)
        (Just (e1', M.TAGGED), Just (e2', M.TAGGED))-> Just (M.PrimOp M.Times [cast M.INT e1', cast M.INT e2'], M.INT)
        (Just (e1', M.INT), Just (e2', M.TAGGED))   -> Just (M.PrimOp M.Times [e1', cast M.INT e2'], M.INT)
        (Just (e1', M.TAGGED), Just (e2', M.INT))   -> Just (M.PrimOp M.Times [cast M.INT e1', e2'], M.INT)
        (Just (e1', t1'), Just (e2', t2'))          -> Just (M.PrimOp M.Times [cast M.INT (tagify t1' e1'), cast M.INT (tagify t2' e2')], M.INT)
        _ -> Nothing
translateExp g (D.PrimOp D.Negate [e1]) = 
    case translateExp g e1 of
        Just (e1', M.INT)   -> Just (M.PrimOp M.Negate [e1'], M.INT)
        Just (e1', M.TAGGED)-> Just (M.PrimOp M.Negate [cast M.INT e1'], M.INT)
        Just (e1', t1')     -> Just (M.PrimOp M.Negate [cast M.INT (tagify t1' e1')], M.INT)
        _ -> Nothing
-- Funs
translateExp g (D.Fun s1 s2 (D.ARROW t1 t1') t2 e) = if t1 /= t2 then Nothing else
    case translateExp (Map.insert s2 (translateType t2) (Map.insert s1 (translateType $ D.ARROW t1 t1') g)) e of
        Just (e2', t2') -> if t2' == (translateType t1') then Just (M.Fun s1 s2 (translateType $ D.ARROW t1 t1') t2' e2', (translateType $ D.ARROW t1 t1')) else Nothing
-- Checks
translateExp g (D.Check e t) = 
    case translateExp g e of
        Just (e', t') -> 
            if t' == (translateType t) 
            then Just (e', t') 
            else   (if t' == M.TAGGED
                    then Just (cast (translateType t) e', (translateType t))
                    else Nothing)
        Nothing -> Nothing
-- Untyped funs
translateExp g (D.UTFun s1 s2 e) = 
    case translateExp (Map.insert s2 M.TAGGED (Map.insert s1 M.TAGGED g)) e of -- How to show that the tagged thing is a function?!
        Just (e2', M.TAGGED) -> Just (M.Fun s1 s2 M.TAGGED M.TAGGED e2', M.TAGGED)
        Nothing -> Nothing
-- Apply
translateExp g (D.Apply e1 e2) = 
    case (translateExp g e1, translateExp g e2) of
        (Just (e1', (M.ARROW t1 t1')), (Just (e2', M.TAGGED))) -> Just (M.Apply e1' (tagify t1 e2'), t1')
        (Just (e1', (M.ARROW t1 t1')), Just (e2', t2')) -> 
            if t1 == t2'
            then Just (M.Apply e1' e2', t1')
            else Just (M.Apply e1' (cast t1 (tagify t2' e2')), t1')
        (Just (e1', M.TAGGED), Just (e2', M.TAGGED)) -> Just (M.Apply (cast (M.ARROW M.TAGGED M.TAGGED) e1') e2', M.TAGGED)
        (Just (e1', M.TAGGED), Just (e2', t2')) -> Just (M.Apply (cast (M.ARROW M.TAGGED M.TAGGED) e1') (tagify t2' e2'), M.TAGGED)
        (Just (e1', t1'), Just (e2', t2')) -> Just (M.Apply (cast (M.ARROW M.TAGGED M.TAGGED) (tagify t1' e1')) (tagify t2' e2'), M.TAGGED)
        _ -> Nothing
-- Vars
translateExp g (D.Var s) = if (Map.lookup s g == Just M.TAGGED) then Just (M.Var s, M.TAGGED) else Nothing
      
-- Nice use of point-free style here
translate :: D.Exp -> Maybe (M.Exp, M.Typ)
translate = translateExp Map.empty
