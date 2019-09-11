{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

-- base
import Control.Monad
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr, stdin)

-- text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

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
  let output = cmdFormat options tz inputHistory
  Text.Lazy.IO.putStr (Builder.toLazyText output)


cmdFormat :: Foldable f => FormatOptions -> TimeZone -> f Entry -> Builder
cmdFormat options tz inputHistory =
  renderHistory (format options) tz inputHistory


renderHistory :: Foldable f => OutputFormat -> TimeZone -> f Entry -> Builder
renderHistory TextOutputFormat tz history = foldMap (renderEntryText tz) history
renderHistory ZshOutputFormat  _  history = foldMap renderEntry history
renderHistory JSONOutputFormat _ _history = error "not yet implemented"


renderEntryText :: TimeZone -> Entry -> Builder
renderEntryText tz e =
  Builder.fromString (formatTime (error "bug: no locale set") "%Y-%m-%d %H:%M:%S" (entryLocalTime e))
  <> " (" <> Builder.decimal (duration e) <> "s) "
  <> Builder.fromText (Zsh.History.command e)
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
