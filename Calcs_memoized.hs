
import Data.Ratio ((%))
import Calcs (binom) -- Asegúrate de que binom esté disponible

-- ===========================================================
-- == Versión Memoizada de fnAT (Números Akiyama-Tanigawa)  ==
-- ===========================================================

-- Definimos una estructura (lista de listas) perezosa que contendrá todos los resultados.
-- fnAT_results !! n !! m contendrá el valor de fnAT n m.
fnAT_results :: [[Rational]]
fnAT_results = [ [ compute_fnAT n m | m <- [0..] ] | n <- [0..] ]
  where
    compute_fnAT :: Integer -> Integer -> Rational
    compute_fnAT n m
      -- Casos base directos
      | n == 0 = 1 % (m + 1) -- Cubre n=0, m=0 (da 1%1) y n=0, m>0
      -- Caso recursivo: busca los valores necesarios en la tabla de resultados
      | otherwise = ((m + 1) % 1) * ( (fnAT_lookup (n-1) m) - (fnAT_lookup (n-1) (m+1)) )

    -- Función auxiliar segura para buscar en la tabla (maneja índices negativos)
    fnAT_lookup :: Integer -> Integer -> Rational
    fnAT_lookup n m
        | n < 0 || m < 0 = 0 % 1
        -- Usamos fromInteger porque los índices de lista son Int
        | otherwise      = fnAT_results !! fromInteger n !! fromInteger m

-- La función pública que el usuario llama (más segura con índices negativos)
fnAT' :: Integer -> Integer -> Rational
fnAT' n m
    | n < 0 || m < 0 = 0%1
    | otherwise      = fnAT_lookup n m -- Usa la función de búsqueda

-- ===========================================================
-- == Versión Memoizada de fnJC (Números Julián-Calderón)   ==
-- ===========================================================

-- Estructura perezosa similar para los resultados de fnJC
fnJC_results :: [[Rational]]
fnJC_results = [ [ compute_fnJC n m | m <- [0..] ] | n <- [0..] ]
  where
    compute_fnJC :: Integer -> Integer -> Rational
    compute_fnJC n m
        -- Casos base directos
        | n <= 0 || m <= 0      = 0 % 1
        | m == 1                = if n == 0 then error "Division by zero: 1 % 0 in fnJC" else 1 % n -- Caso base m=1, ¡cuidado con n=0!
        -- Caso recursivo: calcula el coeficiente y la sumatoria usando búsquedas
        | otherwise             = let coef_val = if (n - m + 1) == 0
                                                  then error "Division by zero: 1 % 0 in fnJC coef"
                                                  else 1 % (n - m + 1)
                                      -- La sumatoria ahora busca los valores fnJC necesarios en la tabla
                                      sum_val  = sum [ (((binom (n-m+l) (n-m)) % 1)) * (fnJC_lookup n (m-l+1))
                                                     | l <- [2..m] ]
                                  in -coef_val * sum_val

    -- Función auxiliar segura para buscar en la tabla fnJC
    fnJC_lookup :: Integer -> Integer -> Rational
    fnJC_lookup n m
        | n < 0 || m < 0 = 0 % 1 -- Los índices negativos no están en la tabla, devuelven 0 según definición
        | otherwise      = fnJC_results !! fromInteger n !! fromInteger m

-- La función pública fnJC'
fnJC' :: Integer -> Integer -> Rational
fnJC' n m
    | n <= 0 || m <= 0 = 0 % 1 -- Manejo explícito antes de la búsqueda
    | otherwise        = fnJC_lookup n m

-- NOTA: El manejo de errores de división por cero se ha hecho explícito con `error`.
--       Podrías preferir devolver un Maybe Rational o manejarlo de otra forma.