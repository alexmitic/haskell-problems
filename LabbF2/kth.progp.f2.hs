module F2 where

data MolSeq = MolSeq String String String deriving (Show)

checkIfDNA :: String -> Bool
checkIfDNA [] = True
checkIfDNA (x:xs)
        | x `elem` "ACGT" = checkIfDNA xs -- If head is DNA check rest
        | otherwise = False -- If not DNA

string2seq :: String -> String -> MolSeq
string2seq n s
        | checkIfDNA s = MolSeq n s "DNA" -- If sequence is DNA
        | otherwise = MolSeq n s "PROTEIN" -- Protein

-- Return name of MolSeq
seqName :: MolSeq -> String
seqName (MolSeq n _ _) = n

-- Return sequence of MolSeq
seqSequence :: MolSeq -> String
seqSequence (MolSeq _ s _) = s

-- Return length of sequence
seqLength :: MolSeq -> Int
seqLength (MolSeq _ s _) = length s

seqDistance :: MolSeq -> MolSeq -> Double
seqDistance (MolSeq _ s1 _) (MolSeq _ s2 _) = fromInteger (length s1)

numDifferent :: String -> String -> Double
numDifferent [] [] = 0
numDifferent (s1h:s1t) (s2h:s2t)
        |s1h == s2h = numDifferent s1t s2t
        |otherwise = 1 + numDifferent s1t s2t
        