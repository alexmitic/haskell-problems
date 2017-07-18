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
seqDistance (MolSeq _ s1 t1) (MolSeq _ s2 t2)
        |t1 /= t2 = error "Not same typ!"
        |checkIfDNA s1 = jukesCantor hamming -- DNA use Jukes-Cantor
        |otherwise = poisson hamming -- Protein use Possion
        where 
                hamming = numDifferent s1 s2 / fromIntegral (length s1)

-- Helpers for evo-distance
-- Number of different characters
numDifferent :: String -> String -> Double
numDifferent [] [] = 0
numDifferent (s1h:s1t) (s2h:s2t)
        |s1h == s2h = numDifferent s1t s2t -- If same skip to next
        |otherwise = 1 + numDifferent s1t s2t 

-- Evo-distance with Jukes-Cantor model
jukesCantor :: Double -> Double
jukesCantor hamming 
        |hamming <= 0.74 = (-3/4) * log(1 - 4/3 * hamming)
        |otherwise = 3.3
-- Evo-distance with Possion model
poisson :: Double -> Double
poisson hamming
        |hamming <= 0.94 = (-19/20) * log(1 - 20 * hamming / 19)
        |otherwise = 3.7


-- Data type for a profile of molecules 
data Profile = Profile [[(Char, Double)]] String Int String deriving (Show)

nucleotides = "ACGT"
aminoacids = "ACDEFGHIKLMNPQRSTVWXY"

-- Creates profile from a list of molecules
makeProfileMatrix :: [MolSeq] -> [[(Char, Double)]]
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
    tmp1 = map (map (\x -> ((head x), ((fromIntegral (length x)) / (fromIntegral (length sl))))) . group . sort)
               (transpose strs) -- First transpose the list of sequences, 
               -- which will create a list of lists where the first list contains
               -- all first letters and so on
               -- After that sort all the lists,
               -- group all equal elements in each list
               -- Then create a tuple consisting of the head of each list and the length
    equalFst a b = (fst a) == (fst b)
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)

-- Take in a list of molecules, a name and create a profile from it 
molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile s mols = Profile (makeProfileMatrix mols) (seqType (head mols)) (length mols) s

-- Getter for namn of a profile
profileName :: Profile -> String
profileName (Profile _ _ _ s) = s

-- Return the relativ frequency of a character
profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile p typ numS _) i c 
        | typ == "DNA" = getOccorencesDna (p !! i) c 
        | otherwise = getOccorencesProtein (p !! i) c 

-- Helper for profileFrequency. Gets the second value from a tuple after finding the correct one
-- Really ugly code but for performance
getOccorencesDna :: [(Char, Double)] -> Char -> Double
getOccorencesDna list c
        | c == 'A' = snd (list !! 0)
        | c == 'C' = snd (list !! 1)
        | c == 'G' = snd (list !! 2)
        | otherwise = snd (list !! 3)

getOccorencesProtein :: [(Char, Double)] -> Char -> Double
getOccorencesProtein list c
        | c == 'A' = snd (list !! 0)
        | c == 'C' = snd (list !! 1)
        | c == 'D' = snd (list !! 2)
        | c == 'E' = snd (list !! 3)
        | c == 'F' = snd (list !! 4)
        | c == 'G' = snd (list !! 5)
        | c == 'H' = snd (list !! 6)
        | c == 'I' = snd (list !! 7)
        | c == 'K' = snd (list !! 8)
        | c == 'L' = snd (list !! 9)
        | c == 'M' = snd (list !! 10)
        | c == 'N' = snd (list !! 11)
        | c == 'P' = snd (list !! 12)
        | c == 'Q' = snd (list !! 13)
        | c == 'R' = snd (list !! 14)
        | c == 'S' = snd (list !! 15)
        | c == 'T' = snd (list !! 16)
        | c == 'V' = snd (list !! 17)
        | c == 'W' = snd (list !! 18)
        | c == 'X' = snd (list !! 19)
        | otherwise = snd (list !! 20)

profileDistance :: Profile -> Profile -> Double 
profileDistance (Profile m1 typ1 num1 name1) (Profile m2 typ2 num2 name2)
         | typ1 /= typ2 = error "Not matching types" -- If comparing diffrent matrix
         | otherwise = sum (zipWith (\list1 list2 -> sum (zipWith (\tup1 tup2 -> abs((snd tup1) - (snd tup2))) list1 list2)) m1 m2) 

class Evol object where
        name :: object -> String
        distance :: object -> object -> Double

        distanceMatrix :: [object] -> [(String, String, Double)]  
        distanceMatrix [] = []
        distanceMatrix list = [(name (head l), name y, abs(distance (head l) y)) | l <- tails list, y <- l]

instance Evol MolSeq where
 name = seqName
 distance = seqDistance
 
instance Evol Profile where
 name = profileName
 distance = profileDistance