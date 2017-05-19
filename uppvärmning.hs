module F1 where
        
-- Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Rövarspråk
rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs) --Separate head and tail
        | x `notElem` "aeiouy"   = x:'o':x: rovarsprak xs -- If head not vocal
        | otherwise              = x: rovarsprak xs

-- Omvändt rövarspråk
karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:xs) --Separate head and tail
        | x `notElem` "aeiouy"   = x: karpsravor (tail (tail xs)) -- If head not vocal
        | otherwise              = x: karpsravor xs


medellangd s = 1.0
skyffla s = s