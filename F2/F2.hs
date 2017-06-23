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
        |seqType m1 /= seqType m2 = error "Not same typ!"
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

-- Take in a list of molecules, a name and create a profile from it 
molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile s mols = Profile (makeProfileMatrix mols) (seqType (head mols)) (length mols) s

-- Getter for namn of a profile
profileName :: Profile -> String
profileName (Profile _ _ _ s) = s

-- Return the relativ frequency of a character
profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile p _ numS _) i c = getOccorences (p !! i) c / fromIntegral numS

-- Helper for profileFrequency. Gets the second value from a tuple after finding the correct one
getOccorences :: [(Char, Int)] -> Char -> Double
getOccorences (x:xs) c
        | fst x == c = fromIntegral (snd x)
        | otherwise = getOccorences xs c


profileDistance :: Profile -> Profile -> Double 
profileDistance (Profile m1 typ1 num1 name1) (Profile m2 typ2 num2 name2)
         | typ1 /= typ2 = error "Not matching types" -- If comparing diffrent matrix
         | otherwise = calculateDist typen lengthWord (Profile m1 typ1 num1 name1) (Profile m2 typ2 num2 name2)
         where
           typen = -- String with all possible characters 
             if typ1 == "DNA" then
                nucleotides
             else
                aminoacids

           lengthWord = length m2 - 1 

calculateDist :: String -> Int -> Profile -> Profile -> Double
calculateDist s n p1 p2 
                | n == (-1) = 0 -- Do from last to first position 
                | otherwise = helperCharcther s n p1 p2 + calculateDist s (n - 1) p1 p2

helperCharcther :: String -> Int -> Profile -> Profile -> Double 
helperCharcther "" _ _ _ = 0 --For all possible letters
helperCharcther (x:xs) n p1 p2 = abs(profileFrequency p1 n x - profileFrequency p2 n x) + helperCharcther xs n p1 p2

class Evol object where
        name :: object -> String
        distance :: object -> object -> Double

        distanceMatrix :: [object] -> [(String, String, Double)]  
        distanceMatrix [] = []
        distanceMatrix n = distanceOneObject n (head n) ++ distanceMatrix (tail n)

        distanceOneObject :: [object] -> object -> [(String, String, Double)]
        distanceOneObject [] _ = []
        distanceOneObject (x:xs) n = (name n, name x, abs(distance x n)) : distanceOneObject xs n
        

instance Evol MolSeq where
 name = seqName
 distance = seqDistance
 
instance Evol Profile where
 name = profileName
 distance = profileDistance