module TTyping where

import TMinML
import Data.Map as Map
import Data.List as List

--I instantiate a global state, to retrive fresh variables 
import Data.IORef

type FreshVar = String -> IO String

makeCounter :: IO FreshVar
makeCounter = do
    r <- newIORef "."
    return (\i -> do modifyIORef r (++ "+")
                     readIORef r)


-- Note that these are TML types, not DML types
-- If it returns None, then it doesn't type check
-- TML does support recursive functions
-- Can get 90% of the way there if all of the types match up
-- the output of your translation must type check at compile time by the TML type rules
-- direct translation of the inference rules in the handout
typing :: Map.Map String Typ -> Exp -> Maybe Typ
typing _ (Int _) = Just INT
typing _ (Bool _) = Just BOOL
typing g (If e e1 e2) = do
  t <- typing g e
  t1 <- typing g e1
  t2 <- typing g e2
  if t == BOOL && t1 == t2 then Just t1 else Nothing
typing g (PrimOp p es) =
  let (args, res) = typeOfPrimOp p
  in do
    ts <- mapM (\e -> typing g e) es
    mapM (\(t,t') -> if t == t' then Just () else Nothing) $ List.zip ts args
    Just res
-- typing g e = ...
typing g (Fun f x tx tf e) = 
    if typing (Map.insert x tx (Map.insert f (ARROW tx tf) g)) e == Just tf
    then Just (ARROW tx tf)
    else Nothing
typing g (Fun f x tx TAGGED e) = 
    if typing (Map.insert x tx (Map.insert f TAGGED g)) e == Just TAGGED
    then Just (ARROW tx TAGGED)
    else Nothing
typing g (Apply e1 e2) =
    case (typing g e1, typing g e2) of
        (Just (ARROW t1 t2), Just t2') -> if t1 == t2' then Just t2 else Nothing
        (Just TAGGED, Just t2') -> Just TAGGED
        _ -> Nothing
typing g (Var s) = Map.lookup s g
typing g (TagInt e) = if (typing g e) == Just INT then Just TAGGED else Nothing
typing g (TagBool e) = if (typing g e) == Just BOOL then Just TAGGED else Nothing
typing g (TagFun e) = if (typing g e) == Just TAGGED then Just TAGGED else Nothing
typing g (AsInt e) = if (typing g e) == Just TAGGED then Just INT else Nothing
typing g (AsBool e) = if (typing g e) == Just TAGGED then Just BOOL else Nothing
typing g (AsFun e) = if (typing g e) == Just TAGGED then Just TAGGED else Nothing
typing _ _ = Nothing


typeOf :: Exp -> Maybe Typ
typeOf e = typing Map.empty e
