{-# LANGUAGE OverloadedStrings #-}

module CsvMaker where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-------------------------------------------------------
-- Match Data Type and CSV Encoding
-------------------------------------------------------

-- | Defines the 'Match' data type, representing a pairing of two individuals.
data Match = Match {
    name       :: String,  -- ^ The name of the person
    email      :: String,  -- ^ The email of the person
    matchName  :: String,  -- ^ The name of their assigned match
    matchEmail :: String   -- ^ The email of their assigned match
} deriving Show

-- | Defines how a 'Match' is converted into a CSV row.
--   Each row contains four fields: name, email, match name, and match email.
instance ToRecord Match where 
    toRecord (Match name email matchName matchEmail) =
        record [toField name, toField email, toField matchName, toField matchEmail]

-------------------------------------------------------
-- Top Matches Data Type and CSV Encoding
-------------------------------------------------------

-- | Defines the 'Top' data type, representing high-scoring matches.
data Top = Top {
    nameT      :: String,  -- ^ Name of the first person in the match
    matchNameT :: String,  -- ^ Name of the matched person
    score      :: Int      -- ^ Match score (higher is better)
} deriving Show

-- | Defines how a 'Top' match is converted into a CSV row.
--   Each row contains three fields: name, match name, and score.
instance ToRecord Top where
    toRecord (Top nameT matchNameT scoreT) = 
        record [toField nameT, toField matchNameT, toField scoreT]

-------------------------------------------------------
-- Writing CSV Files
-------------------------------------------------------

-- | Writes a list of 'Match' data to a CSV file.
--   Each entry represents a unique match between two individuals.
writeMatchesToCSV :: FilePath -> [Match] -> IO ()
writeMatchesToCSV filePath matches = 
    BL.writeFile filePath (encode matches)

-- | Writes a list of 'Top' match data to a CSV file.
--   Each entry represents a top match with a score.
writeTopToCSV :: FilePath -> [Top] -> IO ()
writeTopToCSV filePath tops = 
    BL.writeFile filePath (encode tops)
