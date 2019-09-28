{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

-- base
import Control.Monad
import Data.Function ((&))
import Data.List (group, sortOn)
import System.Exit (exitFailure)
import System.IO (withFile, hPrint, hPutStrLn, stderr, stdin, stdout, Handle, IOMode(..))

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder

-- containers
import Data.Containers.ListUtils (nubOrdOn)

-- text
-- import Data.Text (Text)
-- import qualified Data.Text.IO as Text.IO
-- import qualified Data.Text.Lazy.IO as Text.Lazy.IO
-- import Data.Text.Lazy.Builder (Builder)
-- import qualified Data.Text.Lazy.Builder as Builder
-- import qualified Data.Text.Lazy.Builder.Int as Builder

-- time
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime

-- zsh-history
import Options hiding (command)
import qualified Options
import Zsh.History


-- Lists are good enough for now.
-- We can switch to Vector later if performance turns out to be an issue.
type History = [Entry]


main :: IO ()
main = do
  options <- parseOptions
  let isDebug = debug options

  when isDebug $
    hPrint stderr options

  case Options.command options of
    Format opts -> mainFormat isDebug opts


mainFormat :: Bool -> FormatOptions -> IO ()
mainFormat _isDebug options@FormatOptions{optInputs,optOutput,optSort} = do

  when (length (filter (==Stdin) optInputs) > 1) $
    abortWithError "command line argument '-' is allowed at most once"

  when (length optInputs > 1 && not optSort) $
    hPutStrLn stderr "Warning: multiple inputs without '--sort'. History entries might be out of order."

  inputHistories <- traverse readHistory optInputs

  tz <- getCurrentTimeZone

  let
    rendered = formatHistories options tz inputHistories
    output = Builder.toLazyByteString rendered

  withOutput optOutput $ \handle ->
    BL.hPutStr handle output


formatHistories :: FormatOptions -> TimeZone -> [History] -> Builder
formatHistories FormatOptions{optFormat,optSort,optDedupLite,optDedup} tz hs =
  renderHistory optFormat tz h'
  where
    h' =
      hs
      & concat
      & if' optSort sortHistory
      & if' optDedupLite dedupLiteHistory
      & if' optDedup dedupHistory

    if' :: Bool -> (a -> a) -> (a -> a)
    if' True f = f
    if' False _ = id


-- NOTE: be careful to use a stable sort here! (timestamp resolution is only 1s)
sortHistory :: History -> History
sortHistory = sortOn timestamp


dedupLiteHistory :: History -> History
dedupLiteHistory = map head . group


dedupHistory :: History -> History
dedupHistory = reverse . nubOrdOn command . reverse


renderHistory :: Foldable f => OutputFormat -> TimeZone -> f Entry -> Builder
renderHistory TextOutputFormat tz history = foldMap (renderEntryText tz) history
renderHistory ZshOutputFormat  _  history = foldMap renderEntry history
renderHistory JSONOutputFormat _ _history = error "not yet implemented"


renderEntryText :: TimeZone -> Entry -> Builder
renderEntryText tz e =
  Builder.stringUtf8 (formatTime (error "bug: no locale set") "%Y-%m-%d %H:%M:%S" (entryLocalTime e))
  <> " (" <> Builder.word64Dec (duration e) <> "s) "
  <> Builder.stringUtf8 (show $ Zsh.History.command e)
  <> "\n"
  where
    entryLocalTime = utcToZonedTime tz . posixSecondsToUTCTime . fromIntegral . timestamp


readHistory :: Input -> IO History
readHistory input = readInput input >>= abortOnLeft . parseHistory


readInput :: Input -> IO ByteString
readInput Stdin = B.hGetContents stdin
readInput (InFile path) = B.readFile path


withOutput :: Output -> (Handle -> IO a) -> IO a
withOutput Stdout action = action stdout
withOutput (OutFile path) action = withFile path WriteMode action


abortWithError :: String -> IO a
abortWithError err = do
  hPutStrLn stderr ("Error: " <> err)
  exitFailure


abortOnLeft :: Either String a -> IO a
abortOnLeft (Left err) = abortWithError err
abortOnLeft (Right x) = pure x
