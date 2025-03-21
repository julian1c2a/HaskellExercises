# HaskellExercises

Esto es exactamente lo que parece. Ejecuto ejercicios 
para ir aprendiendo.

## Fact.hs

En este archivo codifico tres funciones.

Primero declaro que se trata de un módulo que exporta
tres funciones:
    
	
	module Fact (fact, euler, euleraprox) where

A continuación digo que voy a usar una módulo que 
permite usar un tipo de números, racionales exactos,
mediante la forma de dos enteros (numerador/denominador)
de precisión arbitraria (*Integer*), el tipo se llama
*Rational*.

    import Data.Ratio

### Función **Fact.fact**
	
Y ya comenzamos por la funcion **fact**. Definimos 
tres casos base, y después el caso general, por la 
definición de la función factorial. Entra un *Integer*
y sale un *Integer*. Bastaría con el primer caso base.

    fact :: Integer -> Intege
    fact 0 = 1
    fact 1 = 1
    fact 2 = 2
    fact n = n * fact (n - 1)


### Función **Fact.euler**
	
La siguiente función tiene entrada de entero positivo,
*Integer*, y sale un *Rational*. En realidad es la 
definición del número **e** de Euler, definido como 
una suma infinita (rápidamente convergente), de 
términos de la forma \(1/n!\), 


$ e = \sum_{i=0}^\infty 1\over{i!} $


Como la serie es infinita, el entero de entrada tiene 
como finalidad solo ver hasta qué término sumamos.

    euler :: Integer -> Rational
    euler 0 = 1 % 1
    euler 1 = 2 % 1
    euler 2 = 5 % 2
    euler n = (1 % fact n) + euler (n - 1)

### Función **Fact.euleraprox**
	
La última función es simplemente el paso de 
racional exacto a formato punto flotante de 
doble precision.

    euleraprox :: Integer -> Double
    euleraprox n = fromRational (euler n)
	
# README.md en construcción