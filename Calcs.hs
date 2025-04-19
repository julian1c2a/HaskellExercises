module Calcs (  fact, euler, euleraprox , 
                binom , bern , 
                bern_poly , fnAT         ) 
      where

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

bern :: Integer -> Rational
bern n
  | n  < 0       =  0 %  1 -- NO HAY NÚMEROS DE BERNOULLI DE ÍNDICE NEGATIVOS
  | n == 0       =  1 %  1 -- CASO BASE IMPRESCINDIBLE
  | n == 1       = -1 %  2 -- CASO BASE PRESCINDIBLE
  | n == 2       =  1 %  6 -- CASO BASE PRESCINDIBLE
  | n == 3       =  0 %  1 -- CASO BASE PRESCINDIBLE
  | n == 4       = -1 % 30 -- CASO BASE PRESCINDIBLE
  | mod n 2 == 0 = -sum [ coef (n + 1) k | k <- [0..(n-1)] ] * ( 1 % (n + 1) )
  -- CASO DE RECURRENCIA GENERAL PARA ÍNDICES PARES
  | otherwise    =  0 %  1 -- CASO DE ÍNDICES IMPARES
    where
      coef :: Integer -> Integer -> Rational
      coef n k = ( ( binom n k ) % 1 ) * bern k

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

bern_monomial :: Integer -> Integer -> (Rational -> Rational)
bern_monomial n k = \x -> ((binom n k) % 1) * (bern k) * (x^^(n - k))

bern_poly :: Integer -> (Rational -> Rational)
bern_poly n
  | n       <= 0 =  \x ->   1 % 1 --CASO BASE IMPRESICINDIBLE
  | n       == 1 =  \x ->  -1 % 2 + x --CASO BASE PRESCINDIBLE
  | n       == 2 =  \x ->   1 % 6 - x + x^^2 --CASO BASE PRESCINDIBLE
  | n       == 3 =  \x ->   (1 % 2) * x + (-3 % 2) * x^^2 + x^^3 
  -- CASO BASE PRESCINDIBLE
  | n       == 4 =  \x -> (-1 % 30) + x^^2 + (-2 % 1) * x^^3 + x^^4
  -- CASO BASE PRESCINDIBLE
  | otherwise    =  \x -> sum [ ( bern_monomial n (2*k) ) x | k <- [0..(div n 2)]]
  -- CASO DE RECURRENCIA GENERAL

-- | Bernoulli polynomials are a sequence of polynomials on the rational field 
-- | with many applications in number theory and combinatorics.
-- | The first few Bernoulli polynomials are:
-- | B_0(x) = 1
-- | B_1(x) = x-(1/2)
-- | B_2(x) = x^2-x+(1/6)
-- | B_3(x) = x^3-(3/2)x^2+(1/2)x
-- | B_4(x) = x^4-2x^3+x^2-(1/30)
-- | B_5(x) = x^5-(5/2)x^4+(5/3)x^3-(1/6)x
-- | B_6(x) = x^6-3x^5+(5/2)x^4-(1/2)x^2+(1/42)
-- | B_7(x) = x^7-(7/2)x^6+(7/2)x^5-(7/6)x^3+(1/6)x
-- | B_8(x) = x^8-4x^7+(14/3)x^6-(7/3)x^4+(2/3)x^2-(1/30)
-- | B_9(x) = x^9-(9/2)x^8+6x^7-(21/5)x^5+2x^3-(3/10)x
-- | B_10(x) = x^10-5x^9+(15/2)x^8-7x^6+5x^4-(3/2)x^2+(5/66)
-- | B_11(x) = x^11-(11/2)x^10+(55/6)x^9-11x^7+11x^5-(11/2)x^3+(5/6)x^2
-- | B_12(x) = x^12-6x^11+11x^10-(33/2)x^8+22x^6-(33/2)x^4+5x^2-(691/2730)

fnAT :: Integer -> Integer -> Rational
-- Akiyama-Tanigawa numbers (table 2D of numbers)
fnAT n m
    | n < 0 || m < 0          = 0%1
    | n == 0 && m == 0        = 1%1
    | n == 0                  = 1%(m+1)
    | n == 1                  = ( (m + 1) % 1 )*( (fnAT 0 m) - (fnAT 0 (m+1)) )
    | n == 2                  = ( (m + 1) % 1 )*( (fnAT 1 m) - (fnAT 1 (m+1)) )
    | otherwise               = ( (m + 1) % 1 )*( (fnAT (n-1) m)-(fnAT (n-1) (m+1)) )
	
fnJC :: Integer -> Integer -> Rational
-- Julián-Calderón numbers (table 2D of numbers)
{-
    n        in [1..]
    m        in [1..]
    fnJC n m in [0..]
    fnJC n m = 0 ES EL CASO FUERA DEL DOMINIO 
-}
fnJC n m
    | n <= 0 || m <= 0        = 0%1 -- CASO ERRÓNEO
    | m == 1                  = 1%n -- CASO BASE
    | otherwise               = -(coef n m) * sumtorio n m -- CASO GENERAL
		where
      sumtorio :: Integer -> Integer -> Rational
      sumtorio n m = sum[ (sumndo n m) * (fnJC n (m-l+1)) | l <- [2..m] ]
          where
            sumndo :: Integer -> Integer .> Rational
            sumndo n m = (binom (n-m+l) (n-m)) % 1
			coef :: Integer -> Integer -> Rational
			coef n m = 1 % ( n - m + 1 )