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
  | n < 0       = -1
  | k < 0       = -1
  | n < k       =  0
  | n == 0      =  0
  | k == 0      =  1
  | n == k      =  1
  | k == 1      =  n
  | k + 1 == n  =  n
  | otherwise   =  div (n * binom (n - 1) (k - 1)) k

test_binom_property_0 :: Integer -> Integer -> Bool
test_binom_property_0 n k
  | (n < 0)  && (k < 0)  && (binom n k == -1) = True
  | (n < 0)  && (k >= 0) && (binom n k == -1) = True
  | (n >= 0) && (k < 0)  && (binom n k == -1) = True
  | (n == 0) && binom n k == 0                = True
  | (n < k)  && binom n k == 0                = True
  | binom n k == div (fact n) (fact k * fact (n - k)) = True
  | otherwise = False

test_binom_property_1 :: Integer -> Integer -> Bool
test_binom_property_1 n k
  | (n  < 0)  && (k  < 0)  && (binom n k == -1) = True
  | (n  < 0)  && (k >= 0)  && (binom n k == -1) = True
  | (n >= 0)  && (k  < 0)  && (binom n k == -1) = True
  | (n == 0)  && binom n k == 0                 = True
  | (n < k)   && binom n k == 0                 = True
  | binom n k == binom n (n - k)                = True
  | otherwise = False

test_binom_property_2 :: Integer -> Bool 
test_binom_property_2 n
  | (n < 0)&&(binom n 1 == (-1)) = True 
  | sum [binom n i | i <- [0..n]] == 2^n = True
  | otherwise = False
  where
    sum [] = 0
    sum (x:xs) = x + sum xs

test_binom_property_3 :: Integer -> Integer -> Bool 
test_binom_property_3 n k
  | (n  < 0)  && (k  < 0)  && (binom n k == -1) = True
  | (n  < 0)  && (k >= 0)  && (binom n k == -1) = True
  | (n >= 0)  && (k  < 0)  && (binom n k == -1) = True
  | (n == 0)  && binom n k == 0                 = True
  | (n < k)   && binom n k == 0                 = True
  | (k == 0)  && (binom n k == 1)               = True
  | (n == 1)  && (binom n k == 1)               = True
  | equaltosumofbinoms n k                      = True
  | otherwise                                   = False
    where
      equaltosumofbinoms n k = binom n k == binom (n - 1) (k - 1) + binom (n - 1) k






