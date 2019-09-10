module Main where

-- base
import Control.Monad
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr, stdin)

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- time
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime

-- zsh-history
import Options
import Zsh.History


main :: IO ()
main = do
  options <- parseOptions
  let isDebug = debug options

  when isDebug $
    hPrint stderr options

  case Options.command options of
    Format opts -> mainFormat isDebug opts


mainFormat :: Bool -> FormatOptions -> IO ()
mainFormat _isDebug options = do
  inputData <- readInput (input options)
  inputHistory <- abortOnLeft (parseHistory inputData)

  tz <- getCurrentTimeZone
  forM_ inputHistory (putStrLn . formatEntryText tz)


formatEntryText :: TimeZone -> Entry -> String
formatEntryText tz e =
  formatTime (error "bug: no locale set") "%Y-%m-%d %H:%M:%S" (entryLocalTime e)
  <> " (" <> show (duration e) <> "s) "
  <> show (Zsh.History.command e)
  where
    entryLocalTime = utcToZonedTime tz . posixSecondsToUTCTime . fromIntegral . timestamp


readInput :: Input -> IO ByteString
readInput Stdin = B.hGetContents stdin
readInput (File path) = B.readFile path


abortWithError :: String -> IO a
abortWithError err = do
  hPutStrLn stderr ("Error: " <> err)
  exitFailure


abortOnLeft :: Either String a -> IO a
abortOnLeft (Left err) = abortWithError err
abortOnLeft (Right x) = pure x
