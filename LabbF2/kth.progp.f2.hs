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