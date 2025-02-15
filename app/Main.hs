module Main where
import CsvDecoder
import Irving
import CsvMaker
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import System.IO (readFile, hFlush, stdout)
import Control.Concurrent (threadDelay, forkIO)

-- to check if the heart is still beating
heartbeat :: IO ()
heartbeat = do
    putStrLn "Program is still running..."
    hFlush stdout  -- Force flushing the output buffer
    threadDelay (60 * 1000000)  -- Wait for 60 seconds
    heartbeat


main :: IO ()
main = do
    _ <- forkIO heartbeat  -- Run heartbeat in a separate thread
    -- The heart!
    heart



heart :: IO ()
heart = do
    csvData <- BL.readFile "realData.csv"  -- Read the file (IO operation)
    let profiles = parseCSV csvData    -- Pass data to the pure function
        roomMates = roomies $ preferenceOrder profiles
        emailName = emailLookup profiles
        matches = map (\(x,y) -> Match (M.findWithDefault "" x emailName) x (M.findWithDefault "" y emailName) y ) twosidedmatches
        twosidedmatches = roomMates ++ map (\(x,y) -> (y,x)) roomMates
        scoreList = preferenceListWithScores profiles
        topList = map (\(x,y,z) -> Top (M.findWithDefault "" x emailName) (M.findWithDefault "" y emailName) z) scoreList
    writeMatchesToCSV "matches.csv" matches
    writeTopToCSV "top.csv" topList
    putStrLn "CSV file written successfully!"