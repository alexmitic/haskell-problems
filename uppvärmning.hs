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
        | x `notElem` "aeiouy"   = x:'o':x: rovarsprak xs -- If tail not vocal
        | otherwise              = x: rovarsprak xs

-- Omvändt rövarspråk
karpsravor s = s

medellangd s = 1.0
skyffla s = s