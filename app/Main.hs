module Main where

import CsvDecoder
import Irving
import CsvMaker
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay, forkIO)

-------------------------------------------------------
-- Heartbeat Monitoring
-------------------------------------------------------

-- | Periodically prints a heartbeat message to indicate the program is still running.
heartbeat :: IO ()
heartbeat = do
    putStrLn "Program is still running..."
    hFlush stdout  -- Force flush stdout to ensure the message appears immediately
    threadDelay (60 * 1000000)  -- Wait for 60 seconds (1 minute)
    heartbeat  -- Recursively call itself to continue indefinitely

-------------------------------------------------------
-- Main Program Execution
-------------------------------------------------------

-- | The main entry point for the program.
main :: IO ()
main = do
    _ <- forkIO heartbeat  -- Runs the heartbeat function in a separate thread
    heart  -- Calls the main logic function

-------------------------------------------------------
-- Core Matching Logic
-------------------------------------------------------

-- | Reads the input CSV, processes it through the Irving matching algorithm,
--   and writes the results into output CSV files.
heart :: IO ()
heart = do
    -- Read input CSV file containing participant preferences
    csvData <- BL.readFile "realData.csv"  

    -- Parse the CSV file into a vector of 'Profile' records
    let profiles = parseCSV csvData    

    -- Generate a stable roommate matching using the Irving algorithm
        roomMates = roomies $ preferenceOrder profiles

    -- Create a lookup table for email-to-name mapping
        emailName = emailLookup profiles

    -- Create a list of bidirectional matches (A,B) and (B,A)
        twosidedmatches = roomMates ++ map (\(x,y) -> (y,x)) roomMates

    -- Convert the matches into 'Match' records for CSV output
        matches = map (\(x,y) -> Match (M.findWithDefault "" x emailName) x 
                                        (M.findWithDefault "" y emailName) y) twosidedmatches

    -- Generate preference scores for ranking
        scoreList = preferenceListWithScores profiles

    -- Convert the preference scores into 'Top' records for CSV output
        topList = map (\(x,y,z) -> Top (M.findWithDefault "" x emailName) 
                                       (M.findWithDefault "" y emailName) z) scoreList

    -- Write the generated matches to "matches.csv"
    writeMatchesToCSV "matches.csv" matches

    -- Write the top-ranked pairs to "top.csv"
    writeTopToCSV "top.csv" topList

    -- Indicate successful completion
    putStrLn "CSV file written successfully!"
