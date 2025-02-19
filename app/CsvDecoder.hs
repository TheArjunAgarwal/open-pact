{-# LANGUAGE OverloadedStrings #-}

module CsvDecoder where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.List

-- | Defines the 'Profile' data type, representing an individual’s responses.
data Profile = Profile {
    name         :: String,   -- ^ User's name
    email        :: String,   -- ^ User's email
    choiceVector :: [Int]     -- ^ User's preference vector (list of integers)
} deriving (Show)

-- | Specifies how to decode a CSV row into a 'Profile' instance.
instance FromRecord Profile where
    parseRecord v
        | V.length v >= 50 = Profile <$> v .! 0 <*> v .! 1 <*> traverse (v .!) [2..49]
        | otherwise = fail "Invalid CSV format: Expecting at least 50 columns"

-- | Parses a CSV file into a vector of 'Profile' records.
--   Throws an error if parsing fails.
parseCSV :: BL.ByteString -> V.Vector Profile
parseCSV csvData =
    case decode NoHeader csvData of
        Left err       -> error err  -- Parsing failed, crash (could be handled more gracefully)
        Right profiles -> profiles   -- Successfully parsed profiles

-------------------------------------------------------
-- Distance Metrics for Matching Algorithm
-------------------------------------------------------

-- | Power used for exponentiation in the distance calculation.
--   If not a power of 2, calculations may be slow.
powerOfExponentiation :: Int
powerOfExponentiation = 2

-- | Computes the Euclidean-like distance between two choice vectors.
--   Assumes both lists are of the same length.
dist :: [Int] -> [Int] -> Int
dist [] [] = 0  -- Base case: distance between two empty lists is 0
dist [] _ = error "Both lists must have the same length"
dist _ [] = error "Both lists must have the same length"
dist (x:xs) (y:ys) = (x - y) ^ powerOfExponentiation + dist xs ys

-------------------------------------------------------
-- Profile Processing & Matching
-------------------------------------------------------

-- | Extracts email and choiceVector pairs from a vector of profiles.
profileList :: V.Vector Profile -> [(String, [Int])]
profileList x = map (\p -> (email p, choiceVector p)) $ V.toList x

-- | Creates a lookup map of email to name from a vector of profiles.
emailLookup :: V.Vector Profile -> M.Map String String
emailLookup x = M.fromList $ map (\p -> (email p, name p)) $ V.toList x

-- | Generates an unsorted preference list with match scores.
preferenceListWithScoresUnsorted :: V.Vector Profile -> [(String, [(String, Int)])]
preferenceListWithScoresUnsorted x =
    [ (a, [(c, dist b d) | (c, d) <- profileList x, (a, b) /= (c, d)])
    | (a, b) <- profileList x
    ]

-- | Orders the preference list by ascending match score (best matches first).
preferenceOrder :: V.Vector Profile -> [(String, [String])]
preferenceOrder x =
    map (\(person, matches) -> (person, map fst $ sortOn snd matches)) $
    preferenceListWithScoresUnsorted x

-------------------------------------------------------
-- Matching Algorithm: Removing Duplicates & Sorting
-------------------------------------------------------

-- | Helper function to extract the third element from a tuple.
thd :: (a, b, c) -> c
thd (_, _, x) = x

-- | Generates a preference list, ensuring that matches are unique
--   and sorted in descending order of similarity.
preferenceListWithScores :: V.Vector Profile -> [(String, String, Int)]
preferenceListWithScores x =
    let
        -- Flatten the unsorted preference list into (email1, email2, score) tuples
        flattened = [(personA, personB, score) 
                     | (personA, matches) <- preferenceListWithScoresUnsorted x
                     , (personB, score) <- matches]

        -- Normalize pairs so that (a, b) and (b, a) are treated as the same
        normalizeKey (a, b) = if a < b then (a, b) else (b, a)

        -- Deduplicate matches by keeping the maximum score for each unique pair
        dedupedMap = M.fromListWith max [(normalizeKey (a, b), score) | (a, b, score) <- flattened]

        -- Convert the map back to a sorted list (descending order by match score)
    in sortOn (negate . thd) [(a, b, c) | ((a, b), c) <- M.toList dedupedMap]

-------------------------------------------------------
-- Additional Distance Functions
-------------------------------------------------------

-- | Hinge Distance (dot product similarity)
hingeDist :: [Int] -> [Int] -> Int
hingeDist [] [] = 0
hingeDist [] _ = error "Both lists must have the same length"
hingeDist _ [] = error "Both lists must have the same length"
hingeDist (x:xs) (y:ys) = (-1) * floor ((fromIntegral x - 5.5) * (fromIntegral y - 5.5)) + hingeDist xs ys

-- | Infinite Norm (Maximum absolute difference)
infiniteNorm :: [Int] -> [Int] -> Int
infiniteNorm [] [] = 0
infiniteNorm [] _ = error "Both lists must have the same length"
infiniteNorm _ [] = error "Both lists must have the same length"
infiniteNorm (x:xs) (y:ys) = max (abs (x - y)) (infiniteNorm xs ys)

-- | Computes min/max lists for Jaccard Similarity variations.
minMaxLists :: (Ord a) => [a] -> [a] -> ([a], [a])
minMaxLists u v = foldr go ([], []) (zip u v)
  where
    go (x, y) (mins, maxs) = (min x y : mins, max x y : maxs)

-- | Computes the Jaccard similarity.
jaccard :: [Int] -> [Int] -> Float
jaccard a b = norm mins / norm maxs
  where 
    (mins, maxs) = minMaxLists a b
    norm l = fromIntegral (sum (map (^ powerOfExponentiation) l)) ** (1 / fromIntegral powerOfExponentiation)

-- | Jaccard-based distance metric
jaccardDist :: [Int] -> [Int] -> Int
jaccardDist a b = floor (1000 * (1 - weightedJaccard a b))

-- | Yet another Jaccard-based idea: similarity score
metric :: [Int] -> [Int] -> Double
metric [] [] = 0
metric [] _ = error "Both lists must have the same length"
metric _ [] = error "Both lists must have the same length"
metric (x:xs) (y:ys)
    | y == 0    = error "Division by zero"
    | otherwise = if x <= y then (fromIntegral x / fromIntegral y) + metric xs ys 
                  else (fromIntegral y / fromIntegral x) + metric xs ys

-- | Converts metric to a distance measure
metricDist :: [Int] -> [Int] -> Int
metricDist l1 l2 = floor (1000 * (50 - metric l1 l2))
