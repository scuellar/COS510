{-# LANGUAGE FlexibleInstances #-}

-- Implementation of the Syntax and Operational Semantics of the Pi Calculus

module Pi where

-- For documentation, see the following pages:
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent.html
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent-Chan.html
-- http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map.html

import Concurrent

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Data.Map as M 
import qualified Data.List as L

-- Syntax of the Pi Calculus

type Name = String

instance Show (Chan Value) where
  show chan = "<channel>"

-- When reading through these data types, it is worth noting that *all* values
-- in this pi calculus are like locations in the STLC with references: they only
-- show up during evaluation, but *not* in programs a user might write.
--
-- In other words, the "abstract channel" object defined in your handout (as
-- "c" in the syntax) will actually be a Haskell channel (VChan below).  But
-- your translation will generate Pi terms, which only include expressions
-- (Exp), not values.

data Value
  = VChan (Chan Value)  -- channel value
  | VTup [Value]        -- tuple of values
  deriving Show    

data Exp
  = EVar Name           -- variable expression
  | ETup [Exp]          -- tuple of expressions
  deriving Show    

data Pattern
  = PVar Name           -- variable pattern
  | PTup [Pattern]      -- tuple pattern
  | Wild                -- wildcard pattern
  deriving Show    

data Typ
  = TChan Typ           -- channel type
  | TTup [Typ]          -- tuple type
  deriving Eq

instance Show Typ where
  show (TChan t) = "Chan " ++ (show t)
  show (TTup []) = "()"
  show (TTup (h:ts)) = "(" ++ (show h) ++
    (L.concatMap (\x -> ", " ++ (show x)) ts) ++ ")"

instance Show (Env -> IO ()) where
  show f = "<function>"

data Pi
  = Nil
  | Pi :|: Pi
  | New Name Typ Pi
  | Out Name Exp 
  | Inp Name Pattern Pi
  | RepInp Name Pattern Pi   -- repeated input
  | Embed (Env -> IO ()) Pi

instance Show Pi where
  show Nil = "0"
  show (p1 :|: p2) =
    "(" ++ (show p1) ++ ") | (" ++ (show p2) ++ ")"
  show (New x t p) =
    "new " ++ x ++ " : " ++ (show t) ++ ". " ++ (show p)
  show (Out x e) =
    "send " ++ x ++ "(" ++ (show e) ++ ")"
  show (Inp x pat p) =
    "rec " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (RepInp x pat p) =
    "rec! " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (Embed _ p) = "<function> " ++ (show p)

-- Useful Abbreviations

unitT :: Typ
unitT = TTup []

unitE :: Exp
unitE = ETup []

unitP :: Pattern
unitP = PTup []

printer :: String -> Pi
printer s = Embed (\_ -> putStr $ s ++ "\n") Nil

-- Static type checking

-- TASK!
-- Implement your pi calculus type checker here!

-- Recall that in Haskell, the Either type is used to represent computations
-- that may fail.  Either a b has two constructors: Left a, and Right b.
-- In the following functions, Left String represents an error with an error
-- message, and Right b represents success, returning a value of type b.

type Gamma = M.Map Name Typ

typeExp :: Gamma -> Exp -> Either String Typ
typeExp g (EVar str) = case (M.lookup str g) of
  Just t -> Right t
  Nothing -> Left $ ("Variable not in the environment: " ++ str )
typeExp g (ETup ls) = case (typeExp' g ls) of
  Left e -> Left e
  Right ls' -> Right (TTup ls')
  where
    typeExp' g (x:ls') = case (typeExp g x, typeExp' g ls) of
      (Left e, _) -> Left e
      (_, Left e) -> Left e
      (Right t, Right ls') -> Right (t:ls')
    typeExp' g [] = Right []

typePat :: Gamma -> Pattern -> Typ -> Either String Gamma
typePat g Wild _ = Right g
typePat g (PVar str) t = case (M.lookup str g) of
  Just t -> Left $ "Variable "++ str++" already in context."
  Nothing -> Right (M.insert str t g)
typePat g (PTup ls) (TChan t) = Left "List of channels expected, but got channel type."
typePat g (PTup ls) (TTup t) = case (ls,t) of
  ([], unitT) -> Right g
  (x:ls', tau:t') -> case (typePat g (PTup ls') (TTup t')) of
    Right g' -> typePat g' x tau
    Left e -> Left e
  _ -> Left "Mismatch of type for a pattern."

checkPi :: Gamma -> Pi -> Either String ()
checkPi _ Nil = Right ()
checkPi g (p1 :|: p2) = case (checkPi g p1, checkPi g p2) of
  (Left e, _) -> Left e
  (_ , Left e) -> Left e
  (Right t1, Right t2) -> Right t1
checkPi g (New str t p) = case (M.lookup str g) of
  Just _ -> Left $ "Varaible "++str++" already in context."
  Nothing -> checkPi (M.insert str t g) p
checkPi g (Out str e) = case (M.lookup str g, typeExp g e) of
  (Nothing, _) -> Left $ "Variable "++ str++" not in the context"
  (_ , Left e) -> Left e
  (Just (TTup t1), Right t2) -> Left $ "Channel expected but found a tupple in: " ++ str
  (Just (TChan t1), Right t2) -> if (t1==t2) then Right () else (Left $ "Mismatching chennel types: "++ show(t1) ++", "++ show(t2))
checkPi g (Inp str pat p) = case (M.lookup str g) of
  Nothing -> Left $ "Variable not in context: " ++ show(str)
  Just t -> case (typePat g pat t) of
    Left e -> Left e
    Right g' -> checkPi g' p
checkPi g (RepInp str pat p) = checkPi g (Inp str pat p)
checkPi g (Embed f p) = checkPi g p --TODO Check Embed

check :: Pi -> Either String ()
check p = checkPi M.empty p

-- Signals a dynamic error

type_error :: String -> a
type_error s = error $ "Run-time Type Error:" ++ s

-- Environments for interpreters

-- TASK!
-- Implement your interpreter here!

type Env = M.Map Name Value

-- eval_p env p v 
-- match a value v against a pattern p and extend environment env
eval_p :: Env -> Pattern -> Value -> Env
eval_p s pat val = case (pat, val) of
  (PVar str, _ ) -> M.insert str val s
  (PTup _, VChan _ ) -> error $ "Can't unpack value " ++ show(val) ++ " into pattern " ++ show(pat)
  (PTup [], VTup []) -> s
  (PTup [], VTup lval) -> error $ "Unpacked values " ++ show(lval)
  (PTup lpat, VTup []) -> error $ "Not enough values to unpack " ++ show(lpat)
  (PTup (pat':lpat), VTup (val':lval)) -> eval_p (eval_p s pat' val) (PTup lpat) (VTup lval)
    
-- eval_e env e
-- evaluates e to a value in environment env
eval_e :: Env -> Exp -> Value
eval_e env (EVar x) = env M.! x
eval_e env (ETup es) = VTup (eval_es env es)
  where
    eval_es env [] = []
    eval_es env (e:es) = eval_e env e : eval_es env es

--Some helper funcitons
getCh :: Env -> String -> Chan Value
getCh s str = case s M.! str of
  VChan c -> c
  VTup v -> error $ "Expecting a channel from '" ++ str++ "' but got " ++ show(v)

run :: Env -> Pi -> IO ()
run s Nil = putStr "Thread done. \n" --Is there a better way to do nothing?
run s (p1 :|: p2) = parallel [run s p1, run s p2] --wait_forIO
run s (New str t p) = do
                      c <- newChan
                      run (M.insert str (VChan c) s) p
run s (Out str e) = writeChan (getCh s  str) (eval_e s e)
run s (Inp str pat p) = do
  inVal <- readChan $ getCh s  str
  run (eval_p s pat inVal) p
run s (RepInp str pat p) = do
  inVal <- readChan $ getCh s  str
  parallel [run (M.insert str inVal s) p, run s (RepInp str pat p)] --wait_forIO
run s (Embed printF p) = do
  printF s
  run s p

start :: Pi -> IO ()
start p = run M.empty p