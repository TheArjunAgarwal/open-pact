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



score :: (String, String) -> M.Map (String, String) Float -> Float
score (x,y) dataMap = let a = M.findWithDefault (-1.1) (x,y) dataMap in
    if (a == (-1.1)) then M.findWithDefault (-1.0) (y,x) dataMap else a


heart :: IO ()
heart = do
    csvData <- BL.readFile "test.csv"  -- Read the file (IO operation)
    let profiles = parseCSV csvData    -- Pass data to the pure function
        roomMates = roomies $ preferenceOrder profiles
        emailName = emailLookup profiles
        matches = map (\(x,y) -> Match (M.findWithDefault "" x emailName) x (M.findWithDefault "" y emailName) y ) twosidedmatches
        twosidedmatches = roomMates ++ map (\(x,y) -> (y,x)) roomMates
    writeMatchesToCSV "matches.csv" matches
    putStrLn "CSV file written successfully!"
