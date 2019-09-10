module Main where

-- base
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

-- bytestring
import qualified Data.ByteString as B

-- zsh-history
import Zsh.History


main :: IO ()
main = do
  [inputFile] <- getArgs
  putStrLn $ "Reading file: " <> inputFile
  input <- B.readFile inputFile
  case parseHistory input of
    Left err -> do
      putStrLn $ "Error: " <> err
      exitFailure
    Right history -> do
      _ <- traverse print history
      exitSuccess
