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

-- | Computes the Euclidean-like distance between two choice vectors.
--   Assumes both lists are of the same length.
dist :: [Int] -> [Int] -> Int
dist = meanAbsoluteDifferenceDist
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
    in sortOn thd [(a, b, c) | ((a, b), c) <- M.toList dedupedMap]

-------------------------------------------------------
-- Additional Distance Functions
-------------------------------------------------------

-- | Computes the Euclidean-like distance between two choice vectors.
-- | Power used for exponentiation in the distance calculation.
powerOfExponentiation :: Int
powerOfExponentiation = 2

--   Assumes both lists are of the same length.
euclideanDist :: [Int] -> [Int] -> Int
euclideanDist xs ys = sum $ zipWith f xs ys where f x y = (x - y) ^ powerOfExponentiation

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
jaccardDist a b = floor (1000 * (1 - jaccard a b))

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

-- | Calculates the Pearson correlation coefficient between two integer vectors.
--   This measures the linear relationship between two datasets.
--   A value of +1 is total positive linear correlation, 0 is no linear correlation,
--   and -1 is total negative linear correlation.
pearsonCorrelation :: [Int] -> [Int] -> Float
pearsonCorrelation xs ys = 
    let
      -- Calculate the mean (average) of each vector.
      xDash = fromIntegral (sum xs) / fromIntegral (length xs)
      yDash = fromIntegral (sum ys) / fromIntegral (length ys)
      
      -- Calculate the deviation of each element from its mean.
      xsDash = map (\x -> fromIntegral x - xDash) xs
      ysDash = map (\y -> fromIntegral y - yDash) ys
      
      -- Numerator: Covariance, the sum of the product of deviations.
      numerator = sum (zipWith (*) xsDash ysDash)
      
      -- Denominators: Related to the standard deviation of each vector.
      denominatorX = sum (map (^ 2) xsDash)
      denominatorY = sum (map (^ 2) ysDash)
      
    in numerator / (sqrt denominatorX * sqrt denominatorY)

-- | The desired output range for the Pearson distance, defining the scale.
pearsonOutputRange :: Int
pearsonOutputRange = 100000000

-- | Converts the Pearson correlation coefficient (a similarity measure from -1.0 to 1.0)
--   into an integer distance score. A higher correlation results in a lower distance.
pearsonDistance :: [Int] -> [Int] -> Int
pearsonDistance xs ys = - round (pearsonCorrelation xs ys * r + r) 
  where r = fromIntegral (pearsonOutputRange `div` 2)


-- | The expected maximum value for a single response, used for scaling.
meanDistInputRange :: Int
meanDistInputRange = 10

-- | The target output range for the final scaled distance.
meanDistOutputRange :: Int
meanDistOutputRange = 1000000

-- | Computes a distance based on the Mean Absolute Difference (MAD).
--   It first calculates the average absolute difference between vector elements.
--   This value is then transformed into a similarity score, scaled to a larger
--   output range, and inverted to function as a final distance metric.
meanAbsoluteDifferenceDist :: [Int] -> [Int] -> Int
meanAbsoluteDifferenceDist xs ys = - round (similarityScore * ratio) where
    -- Calculate the average of the absolute differences between corresponding elements.
    meanAbsDiff = fromIntegral (sum (zipWith (\x y -> abs (x-y)) xs ys)) / fromIntegral (length xs) :: Double
    
    -- Convert the distance (meanAbsDiff) into a similarity score.
    -- A smaller difference results in a higher similarity score.
    similarityScore = fromIntegral meanDistInputRange - meanAbsDiff
    
    -- Define the scaling factor to map the similarity score to the desired output range.
    ratio = fromIntegral meanDistOutputRange / fromIntegral meanDistInputRange