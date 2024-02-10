module Haskel1 where

    import Data.Char (toUpper)

    -- Ejercicio 1
    -- Función que aplica el descuento a un producto
    descuento :: Float -> Float -> Float
    descuento precio desc 
        = precio - (precio * (desc / 100))

    -- Función que aplica el IVA a un producto
    iva :: Float -> Float -> Float
    iva precio porIva 
        = precio + (precio * (porIva / 100))
    
    -- Función que recibe un diccionario de datos y una función, aplica la funcion a cada parte del diccionario y devuelve un resultado
    precioFinal :: [(Float, Float)] -> (Float -> Float -> Float) -> Float
    precioFinal cesta funcion = sum [funcion precio descuento | (precio, descuento) <- cesta]

    -- Ejercicio 2:
    -- Funcion que recibe una función junto con una lista y regresa una lista en la cual se aplico la funcion dada a cada uno de los
    -- elementos
    aplicarFuncionALista :: (a -> b) -> [a] -> [b]
    aplicarFuncionALista _ [] = []
    aplicarFuncionALista f (x : xs) = f x : aplicarFuncionALista f xs

    -- función que eleva al cuadrado un número
    cuadrado :: (Num a) => a -> a
    cuadrado n = n * n

    -- Ejercicio 3: 
    -- Función que calcula la longitud de cada palabra en una frase, la función divide una frase en palabras y crea una lista de tuplas 
    -- para cada palabra (palabra,longitud)
    longitudFrase :: String -> [(String, Int)]
    longitudFrase frase = [(palabra, length palabra) | palabra <- palabras] 
        where 
            palabras = words frase

    -- Ejercicio 4: 
    -- Función que convierte notas a calificaciones según ciertos criterios
    calificaciones :: [(String, Int)] -> [(String, String)]
    calificaciones rendimiento = [(map toUpper asignatura, convertirCalificacion nota) | (asignatura, nota) <- rendimiento]
        where
            convertirCalificacion :: Int -> String
            convertirCalificacion nota
                | nota >= 95 = "Excelente"
                | nota >= 85 = "Notable"
                | nota >= 75 = "Bueno"
                | nota >= 70 = "Suficiente"
                | otherwise = "Desempeño insuficiente"

    -- Ejercicio 5: 
    -- Función que calcula el módulo de un vector aplicando la fórmula del módulo vectorial
    moduloVector :: [Float] -> Float
    moduloVector vector = sqrt (sum [x ^ 2 | x <- vector]) 

    -- Ejercicio 6: 
    -- Función que identifica valores atípicos en una muestra de números, primero calcula la media de la muestra, despues
    -- la desviacion estandar de la muestra y al final filtra los valores atípicos basados en la puntuación típica
    valoresAtipicos :: [Double] -> [Double]
    valoresAtipicos muestra =
        let mediaMuestra = sum muestra / fromIntegral (length muestra)
            desviacion = sqrt (sum [(x - mediaMuestra) ^ 2 | x <- muestra] / fromIntegral (length muestra)) 
        in [x | x <- muestra, abs ((x - mediaMuestra) / desviacion) > 3] 
