module Calcs (fact, euler, euleraprox) where

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

test_binom_property_0 :: Integer -> Integer -> Bool
test_binom_property_0 n k -- PROPERTY_0 := C(n k) == n! / ((n-k)! k!)
  | (n < 0) && (k < 0) && (binom n k == -1) = True -- PROPIEDAD CORRECTA POR
  -- QUE LAS ENTRADAS SON ERRÓNEAS (SIN SENTIDO)
  | (n < 0) && (k >= 0) && (binom n k == -1) = True -- PROPIEDAD CORRECTA POR
  -- QUE LAS ENTRADAS SON ERRÓNEAS (SIN SENTIDO)
  | (n >= 0) && (k < 0) && (binom n k == -1) = True -- PROPIEDAD CORRECTA POR
  -- QUE LAS ENTRADAS SON ERRÓNEAS (SIN SENTIDO)
  | (n == 0) && (k == 0) && (binom n k == 1) = True -- CASO CORRECTO EN SI MISMO
  | (n == 0) && (binom n k == 0) = True -- CASO CORRECTO EN SI MISMO
  | (n < k) && binom n k == 0 = True -- CASO CORRECTO EN SI MISMO
  | binom n k == div (fact n) (fact k * fact (n - k)) = True -- CASO GENERAL
  -- CORRECTO
  | otherwise = False -- CASO CON SENTIDO POR LAS
  -- ENTRADAS Y FALSO  (INCORRECTO)

test_binom_property_1 :: Integer -> Integer -> Bool
test_binom_property_1 n k -- PROPERTY_1 := C(n k) == C(n (n-k))
  | (n < 0) && (k < 0) && (binom n k == -1) = True
  | (n < 0) && (k >= 0) && (binom n k == -1) = True
  | (n >= 0) && (k < 0) && (binom n k == -1) = True
  | (n == 0) && (k == 0) && (binom n k == 1) = True
  | (n == 0) && binom n k == 0 = True
  | (n < k) && binom n k == 0 = True
  | binom n k == binom n (n - k) = True
  | otherwise = False

test_binom_property_2 :: Integer -> Bool
test_binom_property_2 n -- PROPERTY_2 := SUM BINOM(N K) FROM K=0 TO K=N == 2^N
  | (n < 0) && (n <= k) && (binom n 1 == (-1)) = True -- CASOS SIN SENTIDO
  | (n >= 0) && (n < k) && (binom n 1 == (-1)) = True -- CASOS SIN SENTIDO
  | sum [binom n i | i <- [0 .. n]] == 2 ^ n = True   -- CASOS DOMINIO CORRECTOS
  | otherwise = False                                 -- CASOS DOMINIO INCORRECTOS
  where
    sum [] = 0
    sum (x : xs) = x + sum xs

test_binom_property_3 :: Integer -> Integer -> Bool
test_binom_property_3 n k -- PROPERTY_3 :=
-- BINOM(N K) == BINOM(N-1 K-1)+BINOM(N-1 K)
-- PROPIEDAD DEL TRIÁNGULO DE PASCAL
  | (n < 0) && (k < 0) && (binom n k == -1) = True
  | (n < 0) && (k >= 0) && (binom n k == -1) = True
  | (n >= 0) && (k < 0) && (binom n k == -1) = True
  | (n == 0) && (k == 0) && (binom n k == 1) = True
  | (n == 0) && binom n k == 0 = True
  | (n < k) && binom n k == 0 = True
  | (k == 0) && (binom n k == 1) = True
  | (n == 1) && (binom n k == 1) = True
  | eq_to_sum_binoms n k = True
  | otherwise = False
  where
    -- CASO GENERAL DEL TRIÁNGULO DE PASCAL
    eq_to_sum_binoms n k = binom n k == binom (n - 1) (k - 1) + binom (n - 1) k
