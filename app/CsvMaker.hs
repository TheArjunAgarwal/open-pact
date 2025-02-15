{-# LANGUAGE OverloadedStrings #-}

module CsvMaker where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Define the Match data type
data Match = Match {
    name       :: String, 
    email      :: String,
    matchName  :: String,
    matchEmail :: String

} deriving Show

-- Define how to convert Match into CSV format
instance ToRecord Match where 
    toRecord (Match name email matchName matchEmail) =
        record [toField name, toField email, toField matchName, toField matchEmail]

-- same same but different
data Top = Top {
    nameT :: String,
    matchNameT :: String,
    score :: Int
} deriving Show

instance ToRecord Top where
    toRecord (Top nameT matchNameT scoreT) = record [toField nameT, toField matchNameT, toField scoreT]

-- Function to write a list of Match data to a CSV file
writeMatchesToCSV :: FilePath -> [Match] -> IO ()
writeMatchesToCSV filePath matches = 
    BL.writeFile filePath (encode matches)

writeTopToCSV :: FilePath -> [Top] -> IO ()
writeTopToCSV filePath tops = BL.writeFile filePath (encode tops)