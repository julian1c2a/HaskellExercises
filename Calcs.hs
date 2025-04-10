module Calcs ( fact, euler, euleraprox , binom ) where

import Data.Ratio

fact :: Integer -> Integer
fact 0 = 1 -- CASO BASE IMPRESCINDIBLE
fact 1 = 1 -- CASO BASE PRESCINDIBLE
fact 2 = 2 -- CASO BASE PRESCINDIBLE
fact n = n * fact (n - 1) -- CASO DE RECURRENCIA GENERAL

euler :: Integer -> Rational
euler 0 = 1 % 1 -- CASO BASE IMPRESCINDIBLE
euler 1 = 2 % 1 -- CASO BASE PRESCINDIBLE
euler 2 = 5 % 2 -- CASO BASE PRESCINDIBLE
euler n = (1 % fact n) + euler (n - 1) -- CASO DE RECURRENCIA GENERAL

euleraprox :: Integer -> Double
euleraprox n = fromRational (euler n) -- CAST EN LIBRARÍA DE RATIONAL A DOUBLE

binom :: Integer -> Integer -> Integer
binom n k
  | n < 0 = -1 -- EL CONJUNTO TOTAL DE CARDINAL NEGATIVO ES UN SINSENTIDO
  | k < 0 = -1 -- QUE LOS SUBCONJUNTOS A COGER SEAN DE CARDINAL
  -- NEGATIVO ES UN SINSENTIDO
  | n < k = 0 -- LOS SUBCONJUNTOS DEL TOTAL CON CARDINAL ESTRICTAMENTE
  -- MAYOR QUE EL TOTAL: ES EL CONJUNTO VACÍO DE CARDINAL 0
  | n == 0 && k == 0 = 1 -- EL SUBCONJUNTO VACÍO ES ÚNICO Y ESTÁ TAMBIÉN EN EL
  -- VACIO DE CARDINAL 0
  | n == 0 = 0 -- LOS SUBCONJUNTOS DEL VACÍO SON {{}} CUYO CARDINAL ES 1
  | k == 0 = 1 -- LOS SUBCONJUNTOS DE CARDINAL 0 SON EXACTAMENTE EL
  -- CONJUNTO QUE CONTIENE EL VACÍO DE CARDINAL 1
  | n == k = 1 -- EL CONJUNTO DE LOS SUBCONJUNTOS TAN GRANDES COMO EL
  -- TOTAL ES NECESARIAMENTE EL QUE CONTIENE SOLO EL TOTAL
  -- DE CARDINAL 1
  | k == 1 = n -- EL CONJUNTO DE LOS SUBCONJUNTOS DEL TOTAL PERO DE UN
  -- SOLO ELEMENTO SON EXACTAMENTE LOS UNITARIOS CON UN
  -- ELEMENTO DEL TOTAL: TIENE CARDINAL n
  | k + 1 == n = n -- EL CONJUNTO DE LOS SUBCONJUNTOS DEL TOTAL PERO CON UN
  -- SOLO ELEMENTO MENOS QUE EL TOTAL SON BIYECTABLES CON LOS
  -- DE UN SOLO ELEMENTOS DEL TOTAL: TIENE CARDINAL n
  | otherwise = div (n * binom (n - 1) (k - 1)) k -- FÓRMULA DE RECURRENCIA

bernoulli_number :: Integer -> Rational
bernoulli_number n =
  | n       <  0 =  0 %  1 -- NO HAY NÚMEROS DE BERNOULLI DE ÍNDICE NEGATIVOS
  | n       == 0 =  1 %  1 -- CASO BASE IMPRESCINDIBLE
  | n       == 1 = -1 %  2 -- CASO BASE PRESCINDIBLE
  | n       == 2 =  1 %  6 -- CASO BASE PRESCINDIBLE
  | n       == 3 =  0 %  1 -- CASO BASE PRESCINDIBLE
  | n       == 4 = -1 % 30 -- CASO BASE PRESCINDIBLE
  | n mod 2 == 1 =  0 %  1 -- CASO DE DE NÚMEROS DE BERNOULLI IMPARES
  | otherwise    = sum [binom n k * bernoulli_number k | k <- [0..(n-1)]] % (n + 1) 
                  -- CASO DE RECURRENCIA GENERAL

-- | Bernoulli numbers are a sequence of rational numbers 
-- | with many applications in number theory and combinatorics.
-- | The first few Bernoulli numbers are:
-- | B_0 = 1
-- | B_1 = -1/2
-- | B_2 = 1/6
-- | B_3 = 0
-- | B_4 = -1/30
-- | B_5 = 0
-- | B_6 = 1/42
-- | B_7 = 0
-- | B_8 = -1/30
-- | B_9 = 0
-- | B_10 = 5/66
-- | B_11 = 0
-- | B_12 = -691/2730

bernoulli_poly :: Integer -> (Rational -> Rational)
bernoulli_poly n =
  | n       <= 0 =  \x ->   1 % 1 --CASO BASE IMPRESICINDIBLE
  | n       == 1 =  \x -> - 1 % 2 + x --CASO BASE PRESCINDIBLE
  | n       == 2 =  \x ->   1 % 6 - x + x^^2 --CASO BASE PRESCINDIBLE
  | n       == 3 =  \x ->   (1 % 2) * x − (3 % 2) * x^^2 + x^^3 
  -- CASO BASE PRESCINDIBLE
  | n       == 4 =  \x -> − (1 % 30) + x^^2 − 2 * x^^3 + x^^4
  -- CASO BASE PRESCINDIBLE
  | otherwise    =  \x -> sum [ (term n k) x | k <- [0..n]]
  -- CASO DE RECURRENCIA GENERAL
    where
      term n k = \x -> binom n k * bernoulli_number k * x^^(n - k)