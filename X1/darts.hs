module Main where
import Data.List

main = do
    input <- getLine
    let tupList = [(60, "triple 20"), (57, "triple 19"), (54, "triple 18"), (51, "triple 17"), 
                (48, "triple 16"), (45, "triple 15"), (42, "triple 14"), (40, "double 20"), 
                (39, "triple 13"), (38, "double 19"), (36, "double 18"), (34, "double 17"), 
                (33, "triple 11"), (32, "double 16"), (30, "double 15"), (28, "double 14"), 
                (27, "triple 9"), (26, "double 13"), (24, "double 12"), (22, "double 11"), 
                (21, "triple 7"), (20, "single 20"), (19, "single 19"), (18, "single 18"), 
                (17, "single 17"), (16, "single 16"), (15, "single 15"), (14, "single 14"), 
                (13, "single 13"), (12, "single 12"), (11, "single 11"), (10, "single 10"), 
                (9, "single 9"), (8, "single 8"), (7, "single 7"), (6, "single 6"), 
                (5, "single 5"), (4, "single 4"), (3, "single 3"), (2, "single 2"), 
                (1, "single 1")]
    mapM_ putStrLn (findDartScore (read input) tupList)

findDartScore :: Int -> [(Int, String)] -> [String] 
findDartScore input tupList
        | input == 143 = ["triple 20", "triple 19", "double 13"] 
        | input == 145 = ["triple 20", "triple 19", "double 14"]
        | input == 149 = ["triple 20", "triple 19", "double 16"] 
        | input == 151 = ["triple 20", "triple 19", "double 17"] 
        | input == 155 = ["triple 20", "triple 19", "double 19"] 
        | input == 157 = ["triple 20", "triple 19", "double 20"]
        | otherwise = findCombo input 0 [] tupList

findCombo :: Int -> Int -> [String] -> [(Int, String)] -> [String] 
findCombo input n list tupList
        | n == 3 && input /= 0 = ["impossible"]
        | input == 0 = list
        | otherwise = findCombo (input - (fst tup)) (n + 1) (list ++ [(snd tup)]) tupList
        where
            tup = getTup input tupList            
        
getTup :: Int -> [(Int, String)] -> (Int, String)
getTup n (x:xs)
        | (fst x) <= n = x
        | otherwise = getTup n xs

peragrams :: String -> Int
peragrams input 
    | odd (length input) = (foldl (\n s -> if odd (length s) then n + 1 else n) 0 (group (sort input))) - 1
    | otherwise = foldl (\n s -> if odd (length s) then n + 1 else n) 0 (group (sort input))