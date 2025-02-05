{-# LANGUAGE OverloadedStrings #-}


module CsvDecoder where
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.List

-- Define the Profile data type
data Profile = Profile {
    name         :: String,
    email        :: String,
    choiceVector :: [Int]
} deriving (Show)

-- Define how to parse a row into a Profile instance
instance FromRecord Profile where
    parseRecord v
        | V.length v >= 50 = Profile <$> v .! 0 <*> v .! 1 <*> traverse (v .!) [2..49]
        | otherwise = fail "Invalid CSV format: Expecting at least 52 columns"

-- Function to parse CSV data (PURE, no IO)
parseCSV :: BL.ByteString -> V.Vector Profile
parseCSV csvData =
    case decode NoHeader csvData of
        Left err       -> error err  -- If parsing fails, crash (or handle it differently)
        Right profiles -> profiles  -- Return parsed profiles



-- p norm (note if the power exponation is not of the form 2^n, the code is very slow)
-- powerOfExponation :: Int
-- powerOfExponation = 8

-- dist :: [Int] -> [Int] -> Int
-- dist [] [] = 0
-- dist [] _ = error "Both lists must have same length"
-- dist _ [] = error "Both lists must have same length"
-- dist (x:xs) (y:ys) = (x-y)^powerOfExponation + dist xs ys


-- Hinge Distence (dot product similarity)
-- dist :: [Int] -> [Int] -> Int
-- dist [] [] = 0
-- dist [] _ = error "Both lists must have same length"
-- dist _ [] = error "Both lists must have same length"
-- dist (x:xs) (y:ys) = (-1) * floor ((fromIntegral x-5)*(fromIntegral y-5)) + dist xs ys

-- Infinite Norm
-- dist :: [Int] -> [Int] -> Int
-- dist [] [] = 0
-- dist [] _ = error "Both lists must have same length"
-- dist _ [] = error "Both lists must have same length"
-- dist (x:xs) (y:ys) = max (x-y) (dist xs ys)

-- Weird Jaccard Similarity
-- minMaxLists :: (Ord a) => [a] -> [a] -> ([a], [a])
-- minMaxLists u v = foldr go ([], []) (zip u v)
--   where
--     go (x, y) (mins, maxs) = (min x y : mins, max x y : maxs)

-- powerOfExponation :: Int
-- powerOfExponation = 8

-- norm :: [Int] -> Float
-- norm l = fromIntegral (sum (map (^ powerOfExponation) l)) ** (1 / fromIntegral powerOfExponation)

-- wjs :: [Int] -> [Int] -> Float
-- wjs a b = norm mins / norm maxs where (mins,maxs) = minMaxLists a b

-- dist :: [Int] -> [Int] -> Int
-- dist a b = floor ( 1000 * (1 - wjs a b))

thd :: (a,b,c) -> c
thd (_,_,x) = x

profileList x = map (\x -> (email x, choiceVector x)) $ V.toList x

emailLookup x = M.fromList $ map (\x -> (email x, name x)) $ V.toList x


preferenceListWithScoresUnsorted x = [(a, [(c, dist b d) | (c,d) <- profileList x, (a,b) /= (c,d)]) | (a,b) <- profileList x ]

preferenceOrder x = map (\(x,y) -> (x, map fst $ sortOn snd y)) $ preferenceListWithScoresUnsorted x

preferenceListWithScores :: V.Vector Profile -> [(String, String, Int)]
preferenceListWithScores x =
    let
        -- Flatten the unsorted preference list
        flattened = [(l, x, y) | (l, ls) <- preferenceListWithScoresUnsorted x, (x, y) <- ls]

        -- Normalize key order so that (a, b) and (b, a) are treated as the same
        normalizeKey (a, b) = if a < b then (a, b) else (b, a)

        -- Use a Map to remove duplicate flipped pairs while keeping the max score
        dedupedMap = M.fromListWith max [(normalizeKey (a, b), c) | (a, b, c) <- flattened]

        -- Extract values and sort them by score in descending order
    in sortOn thd [(a, b, c) | ((a, b), c) <- M.toList dedupedMap]

