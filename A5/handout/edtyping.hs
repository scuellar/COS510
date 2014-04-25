module EDTyping where

import EDMinML
import Data.Map as Map

check_wf :: Map.Map String () -> Exp -> Maybe ()
-- check_wf g e = ...
-- The gist of this is just scoping
-- Map String to unit, if it's there, it's in scope
-- typed and untyped functions are both well-formed under the same conditions
-- the type annotations aren't special
check_wf _ (Int i) = Just ()
check_wf _ (Bool b) = Just ()
check_wf g (If e1 e2 e3) = case (check_wf g e1, check_wf g e2, check_wf g e3) of
                                (Just (), Just (), Just()) -> Just ()
                                _ -> Nothing
check_wf g (PrimOp Equal [e1, e2]) = case (check_wf g e1, check_wf g e2) of
                                            (Just (), Just ()) -> Just ()
                                            _ -> Nothing
check_wf g (PrimOp Plus [e1, e2]) = case (check_wf g e1, check_wf g e2) of
                                            (Just (), Just ()) -> Just ()
                                            _ -> Nothing
check_wf g (PrimOp Minus [e1, e2]) = case (check_wf g e1, check_wf g e2) of
                                            (Just (), Just ()) -> Just ()
                                            _ -> Nothing
check_wf g (PrimOp Times [e1, e2]) = case (check_wf g e1, check_wf g e2) of
                                            (Just (), Just ()) -> Just ()
                                            _ -> Nothing
check_wf g (PrimOp Negate [e1]) = check_wf g e1
check_wf g (Fun f x tf tx e) = check_wf g (UTFun f x e)
check_wf g (Check e t) = check_wf g e
check_wf g (UTFun f x e) = check_wf (Map.insert x () (Map.insert f () g)) e
check_wf g (Apply e1 e2) = case (check_wf g e1, check_wf g e2) of
                                    (Just (), Just ()) -> Just ()
                                    _ -> Nothing
check_wf g (Var s) = Map.lookup s g
check_wf _ _ = Nothing

check_wellformed :: Exp -> Maybe ()
check_wellformed e = check_wf Map.empty e
