module Haskel2 where
    import Data.Char (toUpper)

    -- Ejercicio 1:
    import System.IO

    -- Funciones para calcular las operaciones solicitadas
    sine :: Double -> Double
    sine x = sin x

    cosine :: Double -> Double
    cosine x = cos x

    tangent :: Double -> Double
    tangent x = tan x

    exponential :: Double -> Double
    exponential x = exp x

    logarithm :: Double -> Double
    logarithm x = log x

    -- Función para aplicar una función a una lista de valores
    aplicarFuncion :: (Double -> Double) -> [Int] -> [(Int, Double)]
    aplicarFuncion f valores = [(x, f $ fromIntegral x) | x <- valores]

    -- Función principal que simula la calculadora
    calculadoraCientifica :: IO ()
    calculadoraCientifica = do
        putStrLn "Calculadora Científica"
        putStr "Ingrese un valor entero: "
        hFlush stdout
        entrada <- getLine
        let valor = read entrada :: Int
        putStr "Seleccione la función a aplicar (seno, coseno, tangente, exponencial, logaritmo): "
        hFlush stdout
        funcion <- getLine
        let resultado = case funcion of
                            "seno" -> aplicarFuncion sine [1..valor]
                            "coseno" -> aplicarFuncion cosine [1..valor]
                            "tangente" -> aplicarFuncion tangent [1..valor]
                            "exponencial" -> aplicarFuncion exponential [1..valor]
                            "logaritmo" -> aplicarFuncion logarithm [1..valor]
                            _ -> []
        putStrLn "Tabla de resultados:"
        putStrLn "---------------------"
        putStrLn " Entero |   Resultado  "
        putStrLn "---------------------"
        mapM_ (\(x, res) -> putStrLn $ "   " ++ show x ++ "   |    " ++ show res) resultado
        putStrLn "---------------------"

    -- Función main para ejecutar la calculadora
    main :: IO ()
    main = calculadoraCientifica

    --Ejercicio 2:
    -- Función que filtra una lista según una función booleana, regresa una lista con los elementos que cumplan con la condición de la
    -- función booleana dada como parámetro, en caso que la lista este vacía se devolvera una lista vacía ya que no hay elementos 
    -- para filtrar
    filtrarTrue :: (a -> Bool) -> [a] -> [a]
    filtrarTrue _ [] = [] 
    filtrarTrue f (x:xs)
        | f x       = x : filtrarTrue f xs 
        | otherwise = filtrarTrue f xs 

    -- Función booleana que verifica si un número es par
    esPar :: Int -> Bool
    esPar x = x `mod` 2 == 0
    
    --Ejercicio 3:
    -- Función que recibe una lista de enteros (calificaciones) y devuelve una lista de cadenas (Notas) 
    calificaciones :: [Int] -> [String]
    calificaciones rendimiento = [convertirCalificacion nota | nota <- rendimiento]
        where
            convertirCalificacion :: Int -> String
            convertirCalificacion nota
                | nota >= 95 = "Excelente"
                | nota >= 85 = "Notable"
                | nota >= 75 = "Bueno"
                | nota >= 70 = "Suficiente"
                | otherwise = "Desempenio insuficiente"

    --Ejercicio 4:
    -- Función que recibe un diccionario (cadena, entero), regresa un diccionario con la cadena en mayuscula y el entero converido en 
    -- una cadena dependiendo de ciertos criterios. 
    calificacionesAsig :: [(String, Int)] -> [(String, String)]
    calificacionesAsig rendimiento = [(map toUpper asignatura, convertirCalificacion nota) | (asignatura, nota) <- rendimiento]
        where
            convertirCalificacion :: Int -> String
            convertirCalificacion nota
                | nota >= 95 = "Excelente"
                | nota >= 85 = "Notable"
                | nota >= 75 = "Bueno"
                | nota >= 70 = "Suficiente"
                | otherwise = "Desempeño insuficiente"

    --Ejercicio 5:
    data Inmueble = Inmueble { año :: Int
                         , metros :: Int
                         , habitaciones :: Int
                         , garaje :: Bool
                         , zona :: Char
                         } deriving (Show)

    -- Función para calcular el precio de un inmueble según la zona y antigüedad
    precioInmueble :: Inmueble -> Float
    precioInmueble inmueble =
        let base = fromIntegral (metros inmueble * 1000 + habitaciones inmueble * 5000 + if garaje inmueble then 15000 else 0)
            descuento = 1 - fromIntegral (2024 - año inmueble) / 100
        in case zona inmueble of
            'A' -> base * descuento
            'B' -> base * descuento * 1.5
            _   -> error "Zona no válida"

    -- Función para filtrar inmuebles según un presupuesto dado
    buscarInmuebles :: [Inmueble] -> Float -> [Inmueble]
    buscarInmuebles inmuebles presupuesto = filter (\inmueble -> precioInmueble inmueble <= presupuesto) inmuebles

    -- Función principal en donde se lee un valor que sera el presupuesto base con el cual se filtraran los datos 
    -- guardados en la lista y se imprimen en pantalla solo aquellos que cumplan con la condición 
    main2 :: IO ()
    main2 = do
        putStr "Ingrese un presupuesto: "
        hFlush stdout
        entrada <- getLine
        let presupuesto = read entrada :: Float
        let inmuebles = [ Inmueble { año = 2000, metros = 100, habitaciones = 3, garaje = True, zona = 'A' } 
                        , Inmueble { año = 2012, metros = 60, habitaciones = 2, garaje = True, zona = 'B' }  
                        , Inmueble { año = 1980, metros = 120, habitaciones = 4, garaje = False, zona = 'A' }
                        , Inmueble { año = 2005, metros = 75, habitaciones = 3, garaje = True, zona = 'B' }
                        , Inmueble { año = 2015, metros = 90, habitaciones = 2, garaje = False, zona = 'A' }
                        ]
            --presupuesto = 100000.0
            -- presupuesto = valor
            inmueblesFiltrados = buscarInmuebles inmuebles presupuesto
        putStrLn "Inmuebles encontrados dentro del presupuesto:" 
        mapM_ (\inmueble -> putStrLn $ "Año: " ++ show (año inmueble) ++
                                   ", Metros: " ++ show (metros inmueble) ++
                                   ", Habitaciones: " ++ show (habitaciones inmueble) ++
                                   ", Garaje: " ++ show (garaje inmueble) ++
                                   ", Zona: " ++ [zona inmueble] ++
                                   ", Precio: " ++ show (precioInmueble inmueble))
          inmueblesFiltrados