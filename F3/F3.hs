module Main where
import System.IO
import Data.List
import F2

main = do
    content <- getContents
    length content `seq` return ()
    let molekyler =  mols (words content)
    let firstDist = seqDistMatrix molekyler molekyler
    let firstSist = sistMatrix firstDist

    print firstDist
    putStrLn ""
    putStrLn ""
    putStrLn ""
    print firstSist

-- Coverts list of name and seq to list of MolSeqs
mols :: [String] -> [MolSeq]
mols [] = []
mols (x:xs) = string2seq x (head xs) : mols (tail xs)

-- Given list of MolSeqs returns all distances 
-- according to formula
seqDistMatrix :: [MolSeq] -> [MolSeq] -> [[(String, String, Double)]]
seqDistMatrix [] _ = []
seqDistMatrix element list = seqDistMatrixOneRow (head element) list : seqDistMatrix (tail element) list 

seqDistMatrixOneRow :: MolSeq -> [MolSeq] -> [(String, String, Double)]
seqDistMatrixOneRow element list = map (\x -> (seqName element, seqName x, abs(seqDistance element x))) list


-- Given distance list return sist list
sistMatrix :: [[(String, String, Double)]] -> [[(String, String, Double)]]
sistMatrix list = map (\n -> map (\x -> if (first x) /= (second x) then (first x, second x, ((fromIntegral (length list)) - 2)*(third x) - (findDist (first x) totalD) - (findDist (second x) totalD)) else (first x, second x, 0) ) n) list
            where
                totalD = totalDist list


-- Returns distances for all Mols
totalDist :: [[(String, String, Double)]] -> [(String, Double)]
totalDist list = map (\n -> (first (head n), sum (map (\x -> third x) n))) list


-------------------------- Helpers -------------------------------
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
------------------------------------------------------------------