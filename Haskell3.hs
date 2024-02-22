module Homework where

    -- Ejercicio 1: la función recibe una lista de enteros y regresa la suma de la lista. 
    sumaLista :: [Int] -> Int
    sumaLista [] = 0
    sumaLista (x:xs) = x + sumaLista xs

    -- Ejercicio 2: la función recibe un entero y regresa el resultado de realizar el factorial del número
    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n* factorial (n-1)

    -- Ejercicio 3: la función recibe un entero y regresa una lista con los números pares antes del número 
    numerosPares :: Int -> [Int]
    numerosPares 0 = []
    numerosPares n = [x | x <- [2,4..n]]

    -- Ejercicio 4: la función recibe una cadena y regresa la longitud de la cadena
    longitudCadena :: String -> Int
    longitudCadena = length

    -- Ejercicio 5: la función recibe una lista de enteros y regresa la misma lista de enteros al reverso
    reversoLista :: [Int] -> [Int]
    reversoLista [] = []
    reversoLista lista = reverse lista

    -- Ejercicio 6: la función recibe una lista de enteros y regresa una lista con los elementos duplicados
    duplicarElementos :: [Int] -> [Int]
    duplicarElementos [] = []
    duplicarElementos lista = concatMap (replicate 2 ) lista

    -- Ejercicio 7: la función recibe una lista de enteros y regresa una lista de enteros filtrada donde solo se
    -- encuentran los números pares
    filtrarPares :: [Int] -> [Int]
    filtrarPares [] = []
    filtrarPares lista = filter even lista

    -- Ejercicio 8: la función recibe un entero y te regresa un entero que se encuentra en la posición n en la secuencia de fibonacci
    fibonacci :: Int -> Int
    fibonacci 0 = 0
    fibonacci 1 = 1
    fibonacci n = fibonacci (n-1) + fibonacci (n-2)

    -- Ejercicio 9: la función recibe un entero y regresa una lista con los divisores del parametro
    divisores :: Int -> [Int]
    divisores 0 = []
    divisores n = [x | x <- [1..n], n `mod` x == 0]

    -- Ejercicio 10: la función recibe una cadena y devuelve un booleano - True si la cadena es palindromo - False en caso contrario
    esPalindromo :: String -> Bool
    esPalindromo cadena = cadena == reverse cadena