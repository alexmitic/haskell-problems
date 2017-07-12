module Main where
import Data.List

main = do
    trash <- getLine -- Ignore number of busses 
    line <- getLine 

    -- Separate busses into a list
    -- Convert to int
    -- Sort the list
    -- Call findBusNumbers
    let list = findBusNumbers (sort (map (read::String -> Int) (words line))) 
    putStrLn (unwords list)

findBusNumbers :: [Int] -> [String]
findBusNumbers [] = []
findBusNumbers (x:xs) 
            | xs == [] = show x : [] -- If only one element left
            | xs /= [] && (tail xs) == [] = show x : show (head xs) : [] -- If only to elements left
            | x /= (head xs) - 1 = show x : findBusNumbers xs -- If not ascending
            | x == (head xs) - 1 && head xs /= (head (tail xs)) - 1 = show x : show (head xs) : findBusNumbers (tail xs) -- If only two ascending
            | otherwise = ((show x) ++ "-" ++ (show (fst lastNum))) : findBusNumbers (snd lastNum) -- Otherwise find first and last of ascending list
                where
                    lastNum = findConsec xs 


findConsec :: [Int] -> (Int, [Int])
findConsec (x:xs)
        | xs == [] = (x, [])
        | x == (head xs) - 1 = findConsec xs
        | otherwise = (x, xs)

