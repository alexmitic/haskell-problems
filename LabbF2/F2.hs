module F2 where
import Control.Monad
import Data.List

-- Datatype for a Molecule
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

-- Return type of Mol
seqType :: MolSeq -> String
seqType (MolSeq _ _ s) = s

-- Calculate evo-distance
seqDistance :: MolSeq -> MolSeq -> Double
seqDistance m1 m2
        |seqType m1 /= seqType m2 = 1.0 -- Throw error if trying to compare diffrent
        |checkIfDNA (seqSequence m1) = jukesCantor hamming -- DNA use Jukes-Cantor
        |otherwise = poisson hamming -- Protein use Possion
        where 
                hamming = numDifferent (seqSequence m1) (seqSequence m2) / fromIntegral (seqLength m1)

-- Helpers for evo-distance
-- Number of different characters
numDifferent :: String -> String -> Double
numDifferent [] [] = 0
numDifferent (s1h:s1t) (s2h:s2t)
        |s1h == s2h = numDifferent s1t s2t
        |otherwise = 1 + numDifferent s1t s2t

-- Evo-distance with Jukes-Cantor model
jukesCantor :: Double -> Double
jukesCantor hamming 
        |hamming <= 0.74 = -3/4 * log(1 - 4/3 * hamming)
        |otherwise = 3.3
-- Evo-distance with Possion model
poisson :: Double -> Double
poisson hamming
        |hamming <= 0.94 = -19/20 * log(1 - 20 * hamming / 19)
        |otherwise = 3.7


-- Data type for a profile of molecules 
data Profile = Profile [[(Char, Int)]] String Int String deriving (Show)

nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

-- Creates profile from a list of molecules
makeProfileMatrix :: [MolSeq] -> [[(Char, Int)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
  where 
    t = seqType (head sl)
    defaults = 
      if t == "DNA" then
        zip nucleotides (replicate (length nucleotides) 0) -- If list of DNA, 
        -- create a list of tupels with all possible letter in DNA paired with a 0
      else 
        zip aminoacids (replicate (length aminoacids) 0)   -- Same as above but with  PROTEIN
    strs = map seqSequence sl -- List of all the sequences 
    tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
               (transpose strs) -- First transpose the list of sequences, 
               -- which will create a list of lists where the first list contains
               -- all first letters and so on
               -- After that sort all the lists,
               -- group all equal elements in each list
               -- Then create a tuple consisting of the head of each list and the length
    equalFst a b = (fst a) == (fst b)
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)