module TMinML where

data Typ = INT | BOOL | TAGGED | ARROW Typ Typ
  deriving (Eq, Show)

data PrimOp = Equal | Plus | Minus | Times | Negate
  deriving (Eq, Show)

data Exp =
   Int Int
 | Bool Bool
 | If Exp Exp Exp
 | PrimOp PrimOp [Exp]
 | Fun String String Typ Typ Exp
 | Apply Exp Exp
 | Var String
 | TagInt Exp
 | TagBool Exp
 | TagFun Exp
 | AsInt Exp
 | AsBool Exp
 | AsFun  Exp
 deriving (Eq, Show)

-- Argument and result types of all the primops.
typeOfPrimOp :: PrimOp -> ([Typ], Typ)
typeOfPrimOp Equal  = ([INT, INT], BOOL)
typeOfPrimOp Plus   = ([INT, INT], INT)
typeOfPrimOp Minus  = ([INT, INT], INT)
typeOfPrimOp Times  = ([INT, INT], INT)
typeOfPrimOp Negate = ([INT], INT)

-- Evaluation for all the primops.
evalPrimOp :: (PrimOp,[Exp]) -> Maybe Exp
evalPrimOp (Equal,[Int i, Int i']) = Just (Bool (i == i'))
evalPrimOp (Plus,[Int i, Int i'])  = Just (Int (i + i'))
evalPrimOp (Minus,[Int i, Int i']) = Just (Int (i - i'))
evalPrimOp (Times,[Int i, Int i']) = Just (Int (i * i'))
evalPrimOp (Negate,[Int i])        = Just (Int (-i))
evalPrimOp _                       = Nothing

