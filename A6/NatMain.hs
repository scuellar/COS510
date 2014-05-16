--module NatMain where
import NatExp
import Pi
import qualified Data.Map as M 

zero = NVal (Z)
one = NVal (S Z)
two = NVal (S $ S Z)
three = NVal (S $ S $ S Z)
four = NVal (S $ S $ S $ S Z)
five = NVal (S $ S $ S $ S $ S Z)

zz = "zz"
opo = "opo"
ftt = "ftt"

oneplusone = one :+: one
fivetimesthree = five :*: three

env1 = M.empty :: NEnv
env2 = M.insert zz three env1
env3 = M.insert opo oneplusone env2
env4 = M.insert ftt fivetimesthree env3

type Test = (NEnv, NatExp)

test1  = (env1, zero)
test2  = (env1, one)
test3  = (env1, zero :+: zero)
test4  = (env1, zero :+: one)
test5  = (env1, one :+: zero)
test6  = (env1, one :+: one)
test7  = (env1, zero :*: zero)
test8  = (env1, zero :*: one) 
test9  = (env1, one :*: zero)
test10 = (env1, one :+: (zero :*: two))
test11 = (env1, two :*: (one :+: zero))
test12 = (env1, two :*: (two :*: (zero :*: zero)))
test13 = (env1, two :*: (two :+: (zero :*: zero)))
test14 = (env2, (NVar zz))
test15 = (env3, (NVar opo))
test16 = (env4, (NVar ftt))

tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16]

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
