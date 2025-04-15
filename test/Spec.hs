import Test.QuickCheck
import Calcs (  fact, euler, euleraprox , 
                binom , bern , 
                bern_poly , fnAT         )

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
  | (n  < 0)  && (binom n 1 == (-1))         = True   -- CASOS SIN SENTIDO
  | sum [binom n i | i <- [0 .. n]] == 2 ^ n = True   -- CASOS DOMINIO CORRECTOS
  | otherwise                                = False  -- CASOS DOMINIO INCORRECTOS
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


main :: IO ()
main = do 
    quickCheck test_binom_property_0
    quickCheck test_binom_property_1
    quickCheck test_binom_property_2
    quickCheck test_binom_property_3