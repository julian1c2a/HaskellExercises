module Calcs (fact, euler, euleraprox) where

import Data.Ratio

fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact 2 = 2
fact n = n * fact (n - 1)

euler :: Integer -> Rational
euler 0 = 1 % 1
euler 1 = 2 % 1
euler 2 = 5 % 2
euler n = (1 % fact n) + euler (n - 1)

euleraprox :: Integer -> Double
euleraprox n = fromRational (euler n)

binom :: Integer -> Integer -> Integer
binom n k
  | n < 0       = 0
  | k < 0       = 0
  | n < k       = 0
  | n == 0      = 0
  | k == 0      = 1
  | n == k      = 1
  | k == 1      = n
  | k + 1 == n  = n
  | otherwise   = div (n * binom (n - 1) (k - 1)) k

testbinom :: Integer -> Bool 
testbinom n
  | (n <= 0)&&(binom 0 n == 0) = True 
  | sum [binom n i | i <- [0..n]] == 2^n = True
  | otherwise = False
  where
    sum [] = 0
    sum (x:xs) = x + sum xs
