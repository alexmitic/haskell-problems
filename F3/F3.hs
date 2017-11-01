module Main where
import System.IO
import Data.List
import F2

main = do
    content <- getContents
    length content `seq` return ()
    let molekyler =  mols (words content) -- Splits input into string array and create list of molseqs

    let distances = seqDistMatrix molekyler
    -- A unit represents the currently calculated evo-tree
    let unit = molNames molekyler

    putStr "("
    mapM putStr (findEvoTree distances unit (length molekyler))
    putStr ")"


-- This is the function that calculates the evo-tree
findEvoTree :: [[(String, String, Double)]] -> [String] -> Int -> [String]
findEvoTree distances unit n
            | n < 4 = addCommas unit
            | otherwise = findEvoTree newDistances newUnit (n - 1)
        where
            lowest = lowestNJElement distances

            a = first lowest
            b = second lowest

            -- Update the unit with the new node
            newUnit = addNewNode a b unit
            -- Calculate the values for the new node
            newDistances = updateDist distances newUnit (head newUnit) a b



-- Calcualtes the new value for every distance using neighbor joining and finds the lowest value
-- Recursivly checks the element in all the lists, calculates thier NJ-value and keeps track of the
-- minimal value
lowestNJElement :: [[(String, String, Double)]] -> (String, String, Double)
lowestNJElement distances = lowestInList distances (totalDist distances) ("a", "b", 10000) (fromIntegral (length distances))            

lowestInList :: [[(String, String, Double)]] -> [(String, Double)] -> (String, String, Double) -> Double -> (String, String, Double)
lowestInList [] _ currLowest _ = currLowest
lowestInList (node:xs) totalD currLowest listLength = lowestInList xs totalD (lowestInListHelper node totalD currLowest listLength) listLength


lowestInListHelper :: [(String, String, Double)] -> [(String, Double)] -> (String, String, Double) -> Double -> (String, String, Double)
lowestInListHelper [] _ currLowest _ = currLowest
lowestInListHelper (node:xs) totalD currLowest listLength = lowestInListHelper xs totalD returnElement listLength
                        where
                            newDist = if first node /= second node then (listLength - 2) * (third node) - (findDist (first node) totalD) - (findDist (second node) totalD) else 0
                            returnElement = if newDist < (third currLowest) then (first node, second node, newDist) else currLowest



-- Given a b and whole unit, it adds (a,b) to unit and removes a b
addNewNode :: String -> String -> [String] -> [String]
addNewNode a b unit = newRoot : (filter (\n -> n /= a && n /= b) unit)
                where
                    newRoot = "(" ++ a ++ "," ++ b ++ ")"



updateDist :: [[(String, String, Double)]] -> [String] -> String -> String -> String -> [[(String, String, Double)]]
updateDist dist toPrint root a b = (map (\n -> if (head n) == root then updateDistNewRoot dist n root a b else updateDistNonNewRoot dist n (head n)) unit)
        where 
            unit = filter (\n -> n /= []) (tails toPrint)

-- Calculates the distance from the newly created node to all the other nodes
-- with the given formula
updateDistNewRoot :: [[(String, String, Double)]] -> [String] -> String -> String -> String -> [(String, String, Double)]
updateDistNewRoot _ [] _ _ _ = []
updateDistNewRoot dist (x:xs) newNode a b = (newNode, x, (((findDistPair a x dist) + (findDistPair b x dist)) / 2)) : updateDistNewRoot dist xs newNode a b


-- Calculates the distance from a previously known node to all the other nodes 
updateDistNonNewRoot :: [[(String, String, Double)]] -> [String] -> String -> [(String, String, Double)]
updateDistNonNewRoot _ [] _ = []
updateDistNonNewRoot dist (x:xs) currNode = (currNode, x, findDistPair currNode x dist) : updateDistNonNewRoot dist xs currNode



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
molNames (x:xs) = (seqName x) : molNames xs

-- Prepares the string for printing by addning commas between all elements
addCommas :: [String] -> [String]
addCommas [] = []
addCommas [x] = x : []
addCommas (x:xs) = x : "," : addCommas xs

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