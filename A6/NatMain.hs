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

type Test = (NEnv, NatExp, [Char])

test1  = (env1, zero, "0 = ")
test2  = (env1, one, "1 = ")
test3  = (env1, zero :+: zero, "0 + 0 = ")
test4  = (env1, zero :+: one, "0 + 1 = ")
test5  = (env1, one :+: zero, "1 + 0 = ")
test6  = (env1, one :+: one, "1 + 1 = ")
test7  = (env1, zero :*: zero, "0 * 0 = ")
test8  = (env1, zero :*: one, "0 * 1 = ") 
test9  = (env1, one :*: zero, "1 * 0 = ")
test10 = (env1, one :+: (zero :*: two), "1 * 0 = ")
test11 = (env1, two :*: (one :+: zero), "2 * (1 + 0) = ")
test12 = (env1, two :*: (two :*: (zero :*: zero)), "2 * (2 *(0 * 0)) = ")
test13 = (env1, two :*: (two :+: (zero :*: zero)), "2 + (0 * 0) = ")
test14 = (env2, (NVar zz), "zz = ")
test15 = (env3, (NVar opo), "opo = ")
test16 = (env4, (NVar ftt), "ftt = ")
test17 = (env2, (two :+: (NVar zz)), "2 + zz = ")
test18 = (env2, (two :*: (NVar opo)), "2 * opo = ")

tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18]

run_tests :: [Test] -> IO ()
run_tests ts = run_t 1 ts
  where
    run_t n []     = return ()
    run_t n ((env,b,text):ts) = do
      putStr ("test " ++ show n ++ ":\n")
      putStrLn (text)
      start_nat env b
      putStr "\n"
      run_t (n+1) ts

main = run_tests tests
