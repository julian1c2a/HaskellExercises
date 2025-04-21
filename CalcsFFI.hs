{-# LANGUAGE ForeignFunctionInterface #-}
module CalcsFFI where

import Calcs (fact, binom, euler, euleraprox, bern, fnJC)

import Data.Ratio (numerator, denominator, (%))

import Foreign.C.Types (CULLong(..),CLLong(..),CInt(..),CDouble(..),CBool(..))
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff, sizeOf, alignment)
import Foreign.C.Error
import Foreign.Ptr
import Foreign.C.String

{-
Tipos Haskell compatibles con C

Los tipos Haskell que son compatibles con C están definidos en el módulo `Foreign.C.Types`. A continuación te muestro los principales tipos y sus equivalencias:

| `char`               | `CChar`      | `Foreign.C.Types`  |
| `unsigned char`      | `CUChar`     | `Foreign.C.Types`  |
| `short`              | `CShort`     | `Foreign.C.Types`  |
| `unsigned short`     | `CUShort`    | `Foreign.C.Types`  |
| `int`                | `CInt`       | `Foreign.C.Types`  |
| `unsigned int`       | `CUInt`      | `Foreign.C.Types`  |
| `long`               | `CLong`      | `Foreign.C.Types`  |
| `unsigned long`      | `CULong`     | `Foreign.C.Types`  |
| `long long`          | `CLLong`     | `Foreign.C.Types`  |
| `unsigned long long` | `CULLong`    | `Foreign.C.Types`  |
| `float`              | `CFloat`     | `Foreign.C.Types`  |
| `double`             | `CDouble`    | `Foreign.C.Types`  |
| `size_t`             | `CSize`      | `Foreign.C.Types`  |
| `ptrdiff_t`          | `CPtrdiff`   | `Foreign.C.Types`  |
| `void`               | `()`         | Estándar Haskell   |
| `void*`              | `Ptr a`      | `Foreign.Ptr`      |
| `char*` (string)     | `CString`    | `Foreign.C.String` |
| `bool`               | `CBool`      | `Foreign.C.Types`  |
| `wchar_t`            | `CWchar`     | `Foreign.C.Types`  |
| Structs              |  empty       | `Storable`         |
| Arrays               |  empty       | `Ptr` y `Storable` |

Para tipos complejos como estructuras y arrays, se necesita definir instancias de la clase `Storable` para tu tipo Haskell, lo que permite la conversión entre representaciones de Haskell y C.

Para `Rational` (como en la función `euler`), no hay un equivalente directo en C, por lo que se necesita convertirlo a un tipo compatible con C (como `CDouble`), como ya estás haciendo en tu función `euler_c`.

Para usar estos tipos, necesitas importar los módulos correspondientes:

-}


{-
   El esquema es el siguiente:
    1. Definimos la función en Haskell.
    2. Definimos la función wrapper en Haskell para tener tipos compatibles en C.
    3. Exportamos la función wrapper para que pueda ser llamada desde C/C++ con `foreign export ccall`.
    4. En C/C++, incluimos el encabezado generado por GHC y llamamos a la función.
-}

-- Función wrapper con tipos C-compatibles
fact_c :: CLLong -> CLLong
fact_c n = fromIntegral (fact (fromIntegral n))
-- Exportar la función para uso desde C/C++
foreign export ccall fact_c :: CLLong -> CLLong

-- Función wrapper con tipos C-compatibles
euler_c :: CInt -> CDouble
euler_c n = realToFrac (euleraprox (fromIntegral n))
-- Exportar la función para uso desde C/C++
foreign export ccall euler_c :: CInt -> CDouble

-- Función wrapper con tipos C-compatibles
binom_c :: CInt -> CInt -> CULLong -- No hacemos abs por que siempre da positivo
binom_c n k = fromIntegral (binom (fromIntegral n) (fromIntegral k))
-- Exportar la función para uso desde C/C++
foreign export ccall binom_c ::  CInt -> CInt -> CULLong

{-
Aquí definimos un tipo que nos traerá un número racional desde Haskell a C y viceversa.
-}


-- Definición de la estructura equivalente a la de C
data RationalC = RationalC CULLong CULLong CBool

-- Implementación de Storable para RationalC
instance Storable RationalC where
    sizeOf _ = sizeOf (undefined :: CULLong) * 2 + sizeOf (undefined :: CBool)
    alignment _ = alignment (undefined :: CULLong)
    peek ptr = do
        num <- peekByteOff ptr 0
        den <- peekByteOff ptr (sizeOf (undefined :: CULLong))
        neg <- peekByteOff ptr (sizeOf (undefined :: CULLong) * 2)
        return $ RationalC num den neg
    poke ptr (RationalC num den neg) = do
        pokeByteOff ptr 0 num
        pokeByteOff ptr (sizeOf (undefined :: CULLong)) den
        pokeByteOff ptr (sizeOf (undefined :: CULLong) * 2) neg

-- Función para convertir Rational a RationalC
rationalToC :: Rational -> RationalC
rationalToC r = 
    let n = numerator r
        d = denominator r -- Siempre positivo
        isNegative = n < 0 -- Número negativo si numerador negativo
        absNum = fromIntegral (abs n)
        absDen = fromIntegral d -- Denominador siempre positivo
    in RationalC absNum absDen (CBool (if isNegative then 1 else 0))

-- Función para convertir RationalC a Rational
rationalFromC :: RationalC -> Rational
rationalFromC (RationalC num den (CBool neg)) = 
    let r = (fromIntegral num) % (fromIntegral den)
    in if neg /= 0 then negate r else r

-- Función wrapper con tipos C-compatibles
bern_c :: CInt -> Ptr RationalC -> IO ()
bern_c n ptr = do
    let result = bern (fromIntegral n)
    let rationalC = rationalToC result
    poke ptr rationalC
-- Exportar la función para uso desde C/C++
foreign export ccall bern_c :: CInt -> Ptr RationalC -> IO ()

-- Función wrapper con tipos C-compatibles
fnJC_c :: CInt -> CInt -> Ptr RationalC -> IO ()
fnJC_c n k ptr = do
    let result = fnJC (fromIntegral n) (fromIntegral k)
    -- Convertir el resultado a RationalC y almacenarlo en el puntero
    let rationalC = rationalToC result
    poke ptr rationalC
-- Exportar la función para uso desde C/C++
foreign export ccall fnJC_c :: CInt -> CInt -> Ptr RationalC -> IO ()
