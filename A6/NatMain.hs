module NatMain where
import NatExp
import qualified Data.Map as M 

zero = NVal (Z)
one = NVal (S Z)

env1 = M.empty

type Test = (NEnv, NatExp)

test1 = (env1, zero)
test2 = (env1, one)
test3 = (env1, zero :+: zero)
test4 = (env1, zero :+: one)
test5 = (env1, one :+: zero)
test6 = (env1, one :+: one)

tests = [test1, test2, test3, test4, test5, test6]

run_tests :: [Test] -> IO ()
run_tests ts = run_t 1 ts
  where
    run_t n []     = return ()
    run_t n ((env,b):ts) = do
      putStr ("test " ++ show n ++ ":\n")
      start_nat env b
      putStr "\n"
      run_t (n+1) ts

main = run_tests tests
