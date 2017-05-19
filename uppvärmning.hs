module F1 where

import Data.Char

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

-- Medellängd av alla ord i en sträng. Separeras på allt förutom bokstäver
medellangd :: String -> Double
medellangd [] = 0 
medellangd x = fromIntegral (sum list) / fromIntegral (length list) -- Convert all to floats, divied sum of list with length of list
        where list = map length (words (separate x)) -- Use map to apply length to every word in string x

separate :: String -> String -- Helper for medellängd
separate [] = []
separate (x:xs)
        | isAlpha x = x: separate xs -- If head is char go to next
        | otherwise = ' ': separate xs -- If not char replace with space


skyffla s = s