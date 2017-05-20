module F2 where

data MolSeq = MolSeq String String String deriving (Show)

checkIfDNA :: String -> Bool
checkIfDNA [] = True
checkIfDNA (x:xs)
        | x `elem` "ACGT" = checkIfDNA xs -- If head is DNA check rest
        | otherwise = False -- If not DNA
