module TMinMLEval where

import TMinML

import qualified Data.Map as Map
           
data Tag = TInt | TBool | TFun | TTagged           
           
instance Show Tag where
  show TInt = "int"
  show TBool = "bool"
  show TFun = "closure"
  show TTagged = "tagged value"
                                 
data Err = TypeError { teExpected :: Tag, teFound :: Tag }
         | PrimError PrimOp
         | ScopeError String
           
instance Show Err where
  show (TypeError expected got) = 
    "expected value tagged as " ++ show expected ++ ", got " ++ show got
  show (PrimError op) = "undefined primitive behavior in " ++ show op
  show (ScopeError id) = "Runtime error: no such identifier " ++ show id
           
subst :: String -> Exp -> Exp -> Exp
subst x v e@(Int _) = e
subst x v e@(Bool _) = e
subst x v (If e1 e2 e3) = If (subst x v e1) (subst x v e2) (subst x v e3)
subst x v (PrimOp op es) = PrimOp op $ map (subst x v) es
subst x v e@(Fun name arg dom cod e') = 
  if x == name || x == arg then e else Fun name arg dom cod (subst x v e')
subst x v (Apply e1 e2) = Apply (subst x v e1) (subst x v e2)
subst x v e@(Var y) = if x == y then v else e
subst x v (TagInt e) = TagInt $ subst x v e
subst x v (TagBool e) = TagBool $ subst x v e
subst x v (TagFun e) = TagFun $ subst x v e
subst x v (AsInt e) = AsInt $ subst x v e
subst x v (AsBool e) = AsBool $ subst x v e
subst x v (AsFun e) = AsFun $ subst x v e

tagOfVal :: Exp -> Tag
tagOfVal (Int _) = TInt
tagOfVal (Bool _) = TBool
tagOfVal (Fun _ _ _ _ _) = TFun
tagOfVal (TagInt _) = TTagged
tagOfVal (TagBool _) = TTagged
tagOfVal (TagFun _) = TTagged
tagOfVal _ = error "not a value"

eval ::  Exp -> Either Err Exp
eval (Int i) = Right $ Int i
eval (Bool b) = Right $ Bool b
eval (If e1 e2 e3) = do
  v <- eval e1
  case v of
    Bool b -> eval (if b then e2 else e3)
    _ -> Left $ TypeError TBool $ tagOfVal v
eval e@(PrimOp op es) = do
  vs <- mapM eval es
  case evalPrimOp (op,vs) of
    Just e -> eval e
    Nothing -> error $ show e -- Left $ PrimError op
eval v@(Fun _ _ _ _ _) = Right v
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    Fun name arg _dom _cod body -> eval (subst arg v2 (subst name v1 body))
    _ -> Left $ TypeError TFun $ tagOfVal v2
eval (Var x) = Left $ ScopeError x
eval (TagInt e) = do
  v <- eval e
  case v of
    Int _ -> Right $ TagInt v
    _ -> Left $ TypeError TInt $ tagOfVal v
eval (TagBool e) = do
  v <- eval e
  case v of
    Bool _ -> Right $ TagBool v
    _ -> Left $ TypeError TBool $ tagOfVal v
eval (TagFun e) = do
  v <- eval e
  case v of
    Fun _ _ _ _ _ -> Right $ TagFun v
    _ -> Left $ TypeError TFun $ tagOfVal v
eval (AsInt e) = do
  v <- eval e
  case v of
    TagInt v' -> Right $ v'
    _ -> Left $ TypeError TTagged $ tagOfVal v
eval (AsBool e) = do
  v <- eval e
  case v of
    TagBool v' -> Right $ v'
    _ -> Left $ TypeError TTagged $ tagOfVal v
eval (AsFun e) = do
  v <- eval e
  case v of
    TagFun v' -> Right $ v'
    _ -> error $ show (AsFun e) --Left $ TypeError TTagged $ tagOfVal v
