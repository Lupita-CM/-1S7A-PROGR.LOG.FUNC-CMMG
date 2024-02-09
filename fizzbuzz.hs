module FizzBuzz where 

    -- Función principal la cual recibe un número entero y manda llamar a la función number para realizar la validación
    fizzbuzz :: Int -> String
    fizzbuzz n = number(n)

    -- Función que recibe un entero y regresa una cadena el numero en ingles si este es menor a 20
    lessThan20 :: Int -> String
    lessThan20 n 
        | n > 0 && n < 20 =
        let answers = words("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")
        in answers !! (n-1)

    -- Función que recibe un entero y regresa una cadena para los numeros multiplos de 10
    tens :: Int -> String 
    tens n
        | n >=2 && n<=9 =
            answers !! (n-2)
            where 
                answers = words ("twenty thirty fourty fifty sixty seventy eighty ninety")

    -- Función que recibe un entero y regresa una cadena dependiendo de la condicion que se cumpla, esta utiliza las funciones 
    -- anteriores para validar los posibles resultados.
    number :: Int -> String 
    number n 
     | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz!"
     | n `mod` 5 == 0 = "Fizz!"
     | n `mod` 3 == 0 = "Buzz!"
     | 1 <= n && n< 20 = lessThan20 (n) ++"!"
     | n `mod` 10 == 0 && n < 100 = tens(n `div` 10) ++"!"
     | n < 100 = tens(n `div` 10) ++ " " ++ lessThan20(n `mod` 10) ++"!"
