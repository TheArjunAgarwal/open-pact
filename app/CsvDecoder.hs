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
        | V.length v >= 52 = Profile <$> v .! 0 <*> v .! 1 <*> traverse (v .!) [2..51]
        | otherwise = fail "Invalid CSV format: Expecting at least 52 columns"

-- Function to parse CSV data (PURE, no IO)
parseCSV :: BL.ByteString -> V.Vector Profile
parseCSV csvData =
    case decode NoHeader csvData of
        Left err       -> error err  -- If parsing fails, crash (or handle it differently)
        Right profiles -> profiles  -- Return parsed profiles

powerOfExponation :: Int
powerOfExponation = 2

dist :: [Int] -> [Int] -> Int
dist [] [] = 0
dist [] _ = error "Both lists must have same length"
dist _ [] = error "Both lists must have same length"
dist (x:xs) (y:ys) = (x-y)^powerOfExponation + dist xs ys

profileList x = map (\x -> (email x, choiceVector x)) $ V.toList x

emailLookup x = M.fromList $ map (\x -> (email x, name x)) $ V.toList x

preferenceListWithScoresUnsorted x = [(a, [(c, dist b d) | (c,d) <- profileList x, (a,b) /= (c,d)]) | (a,b) <- profileList x ]

preferenceOrder x = map (\(x,y) -> (x, map fst $ sortOn snd y)) $ preferenceListWithScoresUnsorted x

