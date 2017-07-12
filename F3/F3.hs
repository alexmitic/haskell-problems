module Main where
import System.IO
import Data.List
import F2

main = do
    content <- getContents
    length content `seq` return ()
    let molekyler =  mols (words content)
    let test = testaaaa molekyler
    let dist = distMa molekyler
    print test
    putStrLn ""
    putStrLn ""
    print dist


-- Coverts list of name and seq to list of MolSeqs
mols :: [String] -> [MolSeq]
mols [] = []
mols (x:xs) = string2seq x (head xs) : mols (tail xs)

-- Given list of MolSeqs returns all distances 
-- according to formula
distMa :: [MolSeq] -> [(String, String, Double)]
distMa [] = []
distMa list = [ if (seqName (head l)) /= seqName y then (seqName (head l), seqName y, (((fromIntegral (length list)) - 2) * abs(seqDistance (head l) y) - (findDist (seqName (head l)) distance) - (findDist (seqName y) distance))) else (seqName (head l), seqName y, 0)| l <- tails list, y <- l]
        where
            distance = totalDist list list

-- Returns distances for all Mols
totalDist :: [MolSeq] -> [MolSeq] -> [(String, Double)]
totalDist [] _ = []
totalDist (x:xs) list = totalDistOneRow x list : totalDist xs list 

totalDistOneRow :: MolSeq -> [MolSeq] -> (String, Double)
totalDistOneRow n list = (seqName n, sum (map (\x -> abs(seqDistance n x)) list))

findDist :: String -> [(String, Double)] -> Double
findDist s (x:xs)
            | s == fst x = snd x
            | otherwise = findDist s xs 


testaaaa :: [MolSeq] -> [(String, String, Double)]
testaaaa [] = []
testaaaa list = [(name (head l), name y, abs(distance (head l) y)) | l <- tails list, y <- l]