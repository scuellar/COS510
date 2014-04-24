module EDMinML where

data Typ = INT | BOOL | ARROW Typ Typ
  deriving (Eq, Show)

data PrimOp = Equal | Plus | Minus | Times | Negate

data Exp =
   Int Int
 | Bool Bool
 | If Exp Exp Exp
 | PrimOp PrimOp [Exp]
 | Fun String String Typ Typ Exp
 | Check Exp Typ
 | UTFun String String Exp
 | Apply Exp Exp
 | Var String

-- Argument and result types of all the primops.
typeOfPrimOp :: PrimOp -> ([Typ], Typ)
typeOfPrimOp Equal  = ([INT, INT], BOOL)
typeOfPrimOp Plus   = ([INT, INT], INT)
typeOfPrimOp Minus  = ([INT, INT], INT)
typeOfPrimOp Times  = ([INT, INT], INT)
typeOfPrimOp Negate = ([INT], INT)

-- Evaluation for all the primops.
evalPrimOp :: (PrimOp, [Exp]) -> Maybe Exp
evalPrimop (Equal, [Int i, Int i']) = Just (Bool (i == i'))
evalPrimOp (Plus, [Int i, Int i'])  = Just (Int (i + i'))
evalPrimOp (Minus, [Int i, Int i']) = Just (Int (i - i'))
evalPrimOp (Times, [Int i, Int i']) = Just (Int (i * i'))
evalPrimOp (Negate, [Int i])        = Just (Int (-i))
evalPrimOp _                        = Nothing

