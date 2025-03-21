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
