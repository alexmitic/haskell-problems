module F1 where

import Data.Char

--Code written by Aleksandar Mitic 9607278737 and Oskar Nehlin 9603035214 

-- Fibonacci
-- This implementation is inspired by the linear https://wiki.haskell.org/The_Fibonacci_sequence

-- fib takes an integer index (n) and returns the number nth in the fib sequence
-- the infinite list is possible thanks to Haskells lazy nature, the recursive definition creates a list beginning with 0,1
-- and the rest is generateraded as the sum of the 2 elements before it. 
-- this works because the tail of the fiblist is one element shorter than the entire list appening 0:1 and the zipWith 
-- where zipwith is the entire list summed with the index n-1 (tail fiblist) and n (fiblist), will give the entire correct sequence
fib :: Integer -> Integer
fib n  = fiblist !! fromIntegral n -- returns the nth index from the infinte list
        where
            fiblist = 0 : 1 : zipWith (+) fiblist (tail fiblist) -- fiblist is a infinite list of the fibo-sequence thanks to the recursion in the definition.

-- Rövarspråk
rovarsprak :: String -> String
rovarsprak [] = [] 
rovarsprak (x:xs) --Separate head and tail
        | notElem x "aeiouy"   = x:'o':x: rovarsprak xs -- If x not vocal add x:'o':x
        | otherwise              = x: rovarsprak xs -- Otherwise just add back the x 

-- Omvändt rövarspråk
karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:xs) --Separate head and tail
        | notElem x "aeiouy"   = x: karpsravor (drop 2 xs) -- If x not vocal remove the following two character 
        | otherwise              = x: karpsravor xs -- Otherwise add back the x

-- Medellängd 
-- Given a string x create list by for every character that is not a letter add space,
-- then use words to create a list of all space-separated words and lastly with the 
-- help of map apply length to all elements in list. The result is that list is a 
-- list where every element represents the length of every word. 

-- Finally return the sum of all elements in list divided by length of list (both converted to floats)  
medellangd :: String -> Double
medellangd [] = 0 
medellangd x = fromIntegral (sum list) / fromIntegral (length list) 
        where list = map length (words (separate x)) -- words removes the spaces and returns a list of the words

-- Helper for medellängd
separate :: String -> String 
separate [] = []
separate (x:xs)
        | isAlpha x = x: separate xs -- If head is letter go to next
        | otherwise = ' ': separate xs -- If not letter replace with space

-- Skyffla om lista
-- Separate every second element from list then recursivly do the same but start with the second element giving us the odd indexes
skyffla :: [a] -> [a] -- Recive list of any type and return list of same type
skyffla [] = []
skyffla list = (head list : everyOther (drop 2 list)) ++ skyffla (everyOther (drop 1 list)) -- we use drop to give every even and odd index respectivly 


--this returnes every other element starting from the head of the list
everyOther :: [a] -> [a]
everyOther [] = []
everyOther x = head x : everyOther (drop 2 x)