module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (getStdGen)
import Control.Monad (when)

import Lib
import Parser (parseConfigFile)
import Types

-- =============================================================================
-- ======= Main Entry Point ====================================================
-- =============================================================================

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        putStrLn "Usage: stack run <config-file.star>"
        exitFailure
    
    let configFile = head args
    content <- readFile configFile
    
    case parseConfigFile content of
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
            exitFailure
        Right galaxy -> do
            when (null $ galaxyMissions galaxy) $ do
                putStrLn "Error: No missions found in config file"
                exitFailure
            
            rng <- getStdGen
            runGame galaxy rng