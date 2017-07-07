module F1 where

import Data.Char

-- Fibonacci
fib :: Integer -> Integer
fib n  
    | n < 0 = n
    | otherwise = fiblist !! fromIntegral n
        where
            fiblist = 0 : 1 : zipWith (+) fiblist (tail fiblist)

-- Rövarspråk
rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak x = map helperSprak x

helperSprak :: Char -> Char
helperSprak s 
        | s `notElem` "aeiouy"   = s:'o':s
        | otherwise              = s

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

-- Skyffla om lista
skyffla :: [a] -> [a] -- Recive list of generic and return list of same type
skyffla [] = []
skyffla list = (head list : everyOther (drop 2 list)) ++ skyffla (everyOther (drop 1 list)) -- Separate every second element from list then recursivly do the same on the ones left

everyOther :: [a] -> [a]
everyOther [] = []
everyOther x = head x : everyOther (drop 2 x) 