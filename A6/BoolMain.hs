--module BoolMain where
import BoolExp
import Pi
import qualified Data.Map as M 

t = BVal True
f = BVal False

x = "x"
y = "y"

env1 = M.empty
env2 = M.insert x True env1 
env3 = M.insert y False env2 

type Test = (BEnv, BoolExp, [Char])

test1 = (env1, t, "True = ")
test2 = (env1, f, "False = ")
test3 = (env1, t :&&: (f :||: t) :&&: (Not f), "True && (False || True) && ~False = ")
test4 = (env2, BVar x, "x = ")
test5 = (env3, BVar y, "y = ")
test6 = (env3, BVar x :&&: BVar y, "x && y = ")
test7 = (env3, BVar x :&&: (BVar y :||: (t :&&: (f :||: t))), "x && (y || (True && (False || True)) = ") --True
test8 = (env3, BVar x :&&: (BVar y :||: (t :&&: (f :||: f))), "x && (y || (True && (False || False)) = ") --False
test9 = (env3, Not (BVar y), "~y = ")
test10 = (env3, Not (BVar y :&&: BVar x), " ~(y && x) = ")
test11 = (env3, BVar y :||: (BVar x :&&: BVar x), "y || (x && x) = ")

tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]

run_tests :: [Test] -> IO ()
run_tests ts = run_t 1 ts
  where
    run_t n []     = return ()
    run_t n ((env,b,text):ts) = do
      putStr ("test " ++ show n ++ ":\n")
      putStr (text)
      start_bool env b
      putStr "\n"
      run_t (n+1) ts

main = run_tests tests
