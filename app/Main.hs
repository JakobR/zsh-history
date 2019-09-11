module Main where

-- base
import Control.Monad
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr, stdin)

-- text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
-- import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
-- import qualified Data.Text.Lazy.Builder.Int as Builder

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
  printHistory (format options) inputHistory


printHistory :: OutputFormat -> [Entry] -> IO ()
printHistory TextOutputFormat history = do
  tz <- getCurrentTimeZone
  forM_ history (putStrLn . formatEntryText tz)
printHistory ZshOutputFormat history = do
  let rendered = mconcat (renderEntry <$> history)
  Text.Lazy.IO.putStr (Builder.toLazyText rendered)
printHistory JSONOutputFormat _history = do
  abortWithError "not yet implemented"


formatEntryText :: TimeZone -> Entry -> String
formatEntryText tz e =
  formatTime (error "bug: no locale set") "%Y-%m-%d %H:%M:%S" (entryLocalTime e)
  <> " (" <> show (duration e) <> "s) "
  <> show (Zsh.History.command e)
  where
    entryLocalTime = utcToZonedTime tz . posixSecondsToUTCTime . fromIntegral . timestamp


readInput :: Input -> IO Text
readInput Stdin = Text.IO.hGetContents stdin
readInput (File path) = Text.IO.readFile path


abortWithError :: String -> IO a
abortWithError err = do
  hPutStrLn stderr ("Error: " <> err)
  exitFailure


abortOnLeft :: Either String a -> IO a
abortOnLeft (Left err) = abortWithError err
abortOnLeft (Right x) = pure x
