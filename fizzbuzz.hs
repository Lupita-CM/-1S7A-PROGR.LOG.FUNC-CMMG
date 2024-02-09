module FizzBuzz where 

    fizzbuzz :: Int -> String
    fizzbuzz n = number(n)
    lessThan20 :: Int -> String
    lessThan20 n 
        | n > 0 && n < 20 =
        let answers = words("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")
        in answers !! (n-1)


    tens :: Int -> String 
    tens n
        | n >=2 && n<=9 =
            answers !! (n-2)
            where 
                answers = words ("twenty thirty fourty fifty sixty seventy eighty ninety")

    number :: Int -> String 
    number n 
     | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz!"
     | n `mod` 3 == 0 = "Fizz!"
     | n `mod` 5 == 0 = "Buzz!"
     | 1 <= n && n< 20 = lessThan20 (n) ++"!"
     | n `mod` 10 == 0 && n < 100 = tens(n `div` 10) ++"!"
     | n < 100 = tens(n `div` 10) ++ " " ++ lessThan20(n `mod` 10) ++"!"