module Main where
import System.IO
import Data.List
import F2

main = do
    content <- getContents
    length content `seq` return ()
    let molekyler =  mols (words content) -- Splits input into string array and create list of molseqs

    let dist1 = seqDistMatrix molekyler
    let unit1 = molNames molekyler

    putStrLn ("(" ++ (concat (runEvo dist1 unit1 (length molekyler))) ++ ")")

--------------------------------------------General-----------------------------------------------------
runEvo :: [[(String, String, Double)]] -> [String] -> Int -> [String]
runEvo dist unit n
            | n < 4 = unit
            | otherwise = runEvo newDist newUnit (n - 1)
        where

            lowest = lowestNJElement dist

            a = first lowest
            b = second lowest

            newUnit = updateString a b unit
            newDist = updateDist dist (filter (\n -> n /= ",") newUnit) (head newUnit) a b

-- Calcualtes the new value for every distance using neighbor joining and finds the lowest value
-- Recursivly checks the element in all the lists, calculates thier NJ-value and keeps track of the
-- minimal value
lowestNJElement :: [[(String, String, Double)]] -> (String, String, Double)
lowestNJElement list = lowestInList list (totalDist list) ("a", "b", 10000) (fromIntegral (length list))            

lowestInList :: [[(String, String, Double)]] -> [(String, Double)] -> (String, String, Double) -> Double -> (String, String, Double)
lowestInList [] _ currLowest _ = currLowest
lowestInList (x:xs) totalD currLowest listLength = lowestInList xs totalD (lowestInListHelper x totalD currLowest listLength) listLength


lowestInListHelper :: [(String, String, Double)] -> [(String, Double)] -> (String, String, Double) -> Double -> (String, String, Double)
lowestInListHelper [] _ currLowest _ = currLowest
lowestInListHelper (element:xs) totalD currLowest listLength = lowestInListHelper xs totalD returnElement listLength
                        where
                            newDist = if first element /= second element then (listLength - 2) * (third element) - (findDist (first element) totalD) - (findDist (second element) totalD) else 0
                            returnElement = if newDist < (third currLowest) then (first element, second element, newDist) else currLowest

-- Returns distances for all Mols
totalDist :: [[(String, String, Double)]] -> [(String, Double)]
totalDist list = map (\n -> (first (head n), totalDistHelper list n)) list

totalDistHelper :: [[(String, String, Double)]] -> [(String, String, Double)] -> Double
totalDistHelper wholeList l 
                -- If l is the same length as the whole list we dont have
                -- to look in other lists to sum that mol as it contains all combos
                | length wholeList == length l = foldl (\n x -> n + (third x)) 0 l
                -- Otherwise we have to get one distance from all the lists 
                -- before the one we are summing
                | otherwise = (foldl (\n x -> n + (third x)) 0 l) + totalDistSpread wholeList currListIndex (currListIndex - 1) 
                    where
                        currListIndex = (length wholeList) - (length l)

-- If we have list [[(A,A,⋅),(A,B,⋅),(A,C,⋅),(A,D,⋅)], [(B,B,⋅),(B,C,⋅),(B,D,⋅)], [(C,C,⋅),(C,D,⋅)], [(D,D,⋅)]]
-- and are summing the total distance of element D, this function will get all the distances where some x maps to D.
-- So it will sum (A,D) + (B,D) + (C,D)
totalDistSpread :: [[(String, String, Double)]] -> Int -> Int -> Double
totalDistSpread list index prevListIndex
                        | prevListIndex == -1 = 0
                        | otherwise = (third ((list !! (prevListIndex)) !! (index - prevListIndex))) + totalDistSpread list index (prevListIndex - 1)


-- Given a b and whole unit, it adds (a,b) to unit and removes a b
updateString :: String -> String -> [String] -> [String]
updateString a b unit = newRoot : (updateStringHelper (filter (\n -> n /= a && n /= b && n /= ",") unit))
                where
                    newRoot = "(" ++ a ++ "," ++ b ++ ")"

updateStringHelper :: [String] -> [String]
updateStringHelper [] = []
updateStringHelper (x:xs) = "," : x : updateStringHelper xs


updateDist :: [[(String, String, Double)]] -> [String] -> String -> String -> String -> [[(String, String, Double)]]
updateDist dist toPrint root a b = (map (\n -> if (head n) == root then updateDistRoot dist n root a b else updateDistNonRoot dist n (head n)) unit)
        where 
            unit = filter (\n -> n /= []) (tails toPrint)

updateDistRoot :: [[(String, String, Double)]] -> [String] -> String -> String -> String -> [(String, String, Double)]
updateDistRoot _ [] _ _ _ = []
updateDistRoot lookUpL (x:xs) temp a b = (temp, x, (((findDistPair a x lookUpL) + (findDistPair b x lookUpL)) / 2)) : updateDistRoot lookUpL xs temp a b

updateDistNonRoot :: [[(String, String, Double)]] -> [String] -> String -> [(String, String, Double)]
updateDistNonRoot _ [] _ = []
updateDistNonRoot lookUpL (x:xs) temp = (temp, x, findDistPair temp x lookUpL) : updateDistNonRoot lookUpL xs temp

-- Finds the given distance pair. (a, b, dist) or (b, a, dist)
findDistPair :: String -> String -> [[(String, String, Double)]] -> Double
findDistPair _ _ [] = 0
findDistPair a b (x:xs)
                | a == first (head x) || b == first (head x) = findDistPairHelper a b x xs -- If dists start with a/b
                | otherwise = findDistPair a b xs

findDistPairHelper :: String -> String -> [(String, String, Double)] -> [[(String, String, Double)]] -> Double
findDistPairHelper a b [] rest = findDistPair a b rest -- Needed for non-exhaustive pattern 
findDistPairHelper a b (x:xs) rest
                | (a == first x && b == second x) || (b == first x && a == second x) = third x -- If correct pair
                | otherwise = findDistPairHelper a b xs rest -- Search rest of given list

----------------------------------------------------------------------------------------------------------------
----------------------------------------------------- Helpers --------------------------------------------------
-- Coverts list of name and seq to list of MolSeqs
mols :: [String] -> [MolSeq]
mols [] = []
mols (x:xs) = string2seq x (head xs) : mols (tail xs)


-- Given list of MolSeqs returns all distances according to formula
-- Gives list [[(A,A,⋅),(A,B,⋅),(A,C,⋅),(A,D,⋅)], [(B,B,⋅),(B,C,⋅),(B,D,⋅)], [(C,C,⋅),(C,D,⋅)], [(D,D,⋅)]]
seqDistMatrix :: [MolSeq] -> [[(String, String, Double)]]
seqDistMatrix [] = []
seqDistMatrix list = filter (\n -> n /= []) (map (\n -> seqDistMatrixOneRow (head n) n) (tails list))

-- Given one molseq and list of other molseqs this function will
-- map the distance from that one molseq to all the other in the list
seqDistMatrixOneRow :: MolSeq -> [MolSeq] -> [(String, String, Double)]
seqDistMatrixOneRow element list = map (\x -> (seqName element, seqName x, abs(seqDistance element x))) list


findDist :: String -> [(String, Double)] -> Double
findDist s (x:xs)
            | s == fst x = snd x
            | otherwise = findDist s xs 


first :: (String, String, Double) -> String
first (n, _, _) = n


second :: (String, String, Double) -> String
second (_, n, _) = n


third :: (String, String, Double) -> Double
third (_, _, n) = n

-- Given list of mols return a,b,c,d
molNames :: [MolSeq] -> [String]
molNames [] = []
molNames [n] = seqName n : []
molNames (x:xs) = (seqName x) : "," : molNames xs