# HaskellExercises

Esto es exactamente lo que parece. Ejecuto ejercicios 
para ir aprendiendo.

## Calcs.hs

En este archivo codifico tres funciones.

Primero declaro que se trata de un módulo que exporta
tres funciones:

	module Calcs (Calcs, euler, euleraprox) where

A continuación digo que voy a usar una módulo que
permite usar un tipo de números, racionales exactos,
mediante la forma de dos enteros (numerador/denominador)
de precisión arbitraria (*Integer*), el tipo se llama
*Rational*.

    import Data.Ratio

### Función **Calcs.Calcs**

Y ya comenzamos por la funcion **Calcs**. Definimos 
tres casos base, y después el caso general, por la 
definición de la función Calcsorial. Entra un *Integer*
y sale un *Integer*. Bastaría con el primer caso base.

    Calcs :: Integer -> Intege
    Calcs 0 = 1
    Calcs 1 = 1
    Calcs 2 = 2
    Calcs n = n * Calcs (n - 1)

### Función **Calcs.euler**

La siguiente función tiene entrada de entero positivo,
*Integer*, y sale un *Rational*. En realidad es la
definición del número **e** de Euler, definido como
una suma infinita (rápidamente convergente), de
términos de la forma \(1/n!\),

```math
    e = {{\sum}_{i=0}^{\infty}} {1\over{i!}}
```

Como la serie es infinita, el entero de entrada tiene
como finalidad solo ver hasta qué término sumamos.

    euler :: Integer -> Rational
    euler 0 = 1 % 1
    euler 1 = 2 % 1
    euler 2 = 5 % 2
    euler n = (1 % Calcs n) + euler (n - 1)

### Función **Calcs.euleraprox**

La última función es simplemente el paso de
racional exacto a formato punto flotante de
doble precision.

    euleraprox :: Integer -> Double
    euleraprox n = fromRational (euler n)

# README.md en construcción
