module Main where
import System.IO
import Data.List
import F2

main = do
    content <- getContents
    length content `seq` return ()
    let dist =  distMa (mols (words content))
    print dist



mols :: [String] -> [MolSeq]
mols [] = []
mols (x:xs) = string2seq x (head xs) : mols (tail xs)

distMa :: [MolSeq] -> [(String, String, Double)]
distMa [] = []
distMa list = [(seqName (head l), seqName y, abs(seqDistance (head l) y)) | l <- tails list, y <- l]