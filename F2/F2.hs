module F2 where
import Control.Monad
import Data.List 

--code written by Oskar Nehlin 9603035214 and Aleksandar Mitic 9607278737

-- Datatype for a Molecule 
data MolSeq = MolSeq String String String deriving (Show)
-- Data type for a profile of molecules 
data Profile = Profile [[(Char, Double)]] String Int String deriving (Show)

--This takes a String of a sequence and returns if it's DNA or not in a boolean
--An edge case is if the input sequence is empty, then the function will return true but we presume the sequences never are empty
checkIfDNA :: String -> Bool
checkIfDNA [] = True 
checkIfDNA (x:xs)
        | elem x "ACGT" = checkIfDNA xs -- If head is DNA check rest
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
                hamming = numDifferent s1 s2 / fromIntegral (length s1) -- hamming is the number of different elements between s1 and s2 divided by the lenght

-- Helpers for evo-distance
-- Returns the number of different characters in the sequences
numDifferent :: String -> String -> Double
numDifferent [] [] = 0
numDifferent (s1h:s1t) (s2h:s2t)
        |s1h == s2h = numDifferent s1t s2t -- If same skip to next
        |otherwise = 1 + numDifferent s1t s2t --otherwise add one to the total number of differences

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
               -- which will create a list of lists where the first list contains all first letters and so on
               -- After that sort all the lists,
               -- group all equal elements in each list meaning it creates a list of lists of equal elements
               -- Then create a tuple consisting of the head of the list of lists (meaning an list of the same letters) and the second element is the lenght of that list divided by the number of sequences
    equalFst a b = (fst a) == (fst b) -- this takes two tuples and returns a boolean if the first element in the tuple is equal
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1) -- this compares the tmp1 list with the defaults (a,0), (t,0) and so on if the letter in the tuple match
                                                                  -- it chooses the calculated tuple in the list l (since otherwise there would be duplicates), if they don't match
                                                                  -- the union will fill res with the default value
                                                                  -- it then sorts the tuples in alphabetical order
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
-- The index valuse are hardcoded for speed. We are aware that there are cleaner soulutions. For example 
-- using the elemIndex function.  
getOccorencesDna :: [(Char, Double)] -> Char -> Double
getOccorencesDna list c
        | c == 'A' = (snd (list !! 0))
        | c == 'C' = (snd (list !! 1))
        | c == 'G' = (snd (list !! 2))
        | otherwise = (snd (list !! 3))

getOccorencesProtein :: [(Char, Double)] -> Char -> Double
getOccorencesProtein list c
        | c == 'A' = (snd (list !! 0))
        | c == 'C' = (snd (list !! 1))
        | c == 'D' = (snd (list !! 2))
        | c == 'E' = (snd (list !! 3))
        | c == 'F' = (snd (list !! 4))
        | c == 'G' = (snd (list !! 5))
        | c == 'H' = (snd (list !! 6))
        | c == 'I' = (snd (list !! 7))
        | c == 'K' = (snd (list !! 8))
        | c == 'L' = (snd (list !! 9))
        | c == 'M' = (snd (list !! 10))
        | c == 'N' = (snd (list !! 11))
        | c == 'P' = (snd (list !! 12))
        | c == 'Q' = (snd (list !! 13))
        | c == 'R' = (snd (list !! 14))
        | c == 'S' = (snd (list !! 15))
        | c == 'T' = (snd (list !! 16))
        | c == 'V' = (snd (list !! 17))
        | c == 'W' = (snd (list !! 18))
        | c == 'X' = (snd (list !! 19))
        | otherwise = (snd (list !! 20))

-- This calculates the evolutionary distance between to profile matrices
-- If the types (DNA/Protein) don't match the function throws an error
-- Otherwise the function takes the absolute difference between each corresponding element in the matrix and sums them up
profileDistance :: Profile -> Profile -> Double 
profileDistance (Profile m1 typ1 num1 name1) (Profile m2 typ2 num2 name2)
         | typ1 /= typ2 = error "Not matching types" -- If comparing diffrent matrix
         | otherwise = sum (zipWith (\col1 col2 -> sum (zipWith (\(a, i) (b, j) -> abs(i - j)) col1 col2)) m1 m2) 


-- type class where Molseq and Profle are instances
class Evol object where
        name :: object -> String
        distance :: object -> object -> Double

        --this creates a distance matrix by giving a list of triplets which represent 
        --all uniqe combination of elements and their distances in the given list
        -- tails list gives us all unique subsets from the given list
        -- for each subset we match and calculate the distance between the first element and all other elements (y) in that subset and against it self.
        distanceMatrix :: [object] -> [(String, String, Double)]  
        distanceMatrix [] = []
        distanceMatrix list = [(name (head subset), name y, distance (head subset) y) | subset <- tails list, y <- subset]

instance Evol MolSeq where
 name = seqName
 distance = seqDistance
 
instance Evol Profile where
 name = profileName
 distance = profileDistance