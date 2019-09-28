{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options where

-- base
import Data.List (intercalate)

-- optparse-applicative
import Options.Applicative hiding (command)
import qualified Options.Applicative

data Options = Options
  { command :: !Command
  , debug :: !Bool
  }
  deriving Show

data Command
  = Format !FormatOptions
  deriving Show

-- TODO: Add option "--filter-regex"? might be useful because grep doesn't properly work with multiline commands
data FormatOptions = FormatOptions
  { optInputs :: ![Input]
  , optFormat :: !OutputFormat
  , optOutput :: !Output
  , optSort :: !Bool
  , optDedup :: !Bool
  , optDedupLite :: !Bool
  }
  deriving Show

data Input
  = Stdin
  | InFile !FilePath
  deriving (Eq, Show)

data Output
  = Stdout
  | OutFile !FilePath
  deriving (Eq, Show)

data OutputFormat
  = TextOutputFormat
  | ZshOutputFormat
  | JSONOutputFormat
  deriving (Eq, Show, Bounded, Enum)


readInputOptionValue :: ReadM Input
readInputOptionValue = do
  path <- str
  case path of
    "-" -> pure Stdin
    _ -> pure (InFile path)


readOutputFormatOptionValue :: String -> Maybe OutputFormat
readOutputFormatOptionValue "text" = Just TextOutputFormat
readOutputFormatOptionValue "zsh"  = Just ZshOutputFormat
readOutputFormatOptionValue "json" = Just JSONOutputFormat
readOutputFormatOptionValue _ = Nothing


showOutputFormatOptionValue :: OutputFormat -> String
showOutputFormatOptionValue TextOutputFormat = "text"
showOutputFormatOptionValue ZshOutputFormat  = "zsh"
showOutputFormatOptionValue JSONOutputFormat = "json"


formatOptionsParser :: Parser FormatOptions
formatOptionsParser = do
  optFormat <-
    option (maybeReader readOutputFormatOptionValue) $
    short 'f'
    <> long "format"
    <> value ZshOutputFormat
    <> showDefaultWith showOutputFormatOptionValue
    <> help ("The output format. (values: "
             <> intercalate ", " (showOutputFormatOptionValue <$> [minBound..maxBound])
             <> ")")
  optOutput <-
    option (OutFile <$> str) $
    short 'o'
    <> long "output"
    <> value Stdout
    <> showDefaultWith (const "stdout")
    <> help "Write output to file"
  optSort <-
    switch $
    short 's'
    <> long "sort"
    <> help "Sort commands by timestamp (this should be a no-op for well-formed history files)."
  optDedup <-
    switch $
    short 'd'
    <> long "dedup"
    <> help "Remove duplicate commands, keeping the latest ones."
  optDedupLite <-
    switch $
    long "dedup-lite"
    <> help ("Remove duplicate consecutive entries "
             <> "(i.e., where all of timestamp, duration, and command are the same).")
  optInputs <-
    fmap (\xs -> if null xs then [Stdin] else xs) $
    many $
    argument readInputOptionValue $
    metavar "INPUT"
    <> help "Input file, or \"-\" to read from stdin instead (default: \"-\")"
    <> action "file"
  pure FormatOptions{..}


formatOptionsParserInfo :: ParserInfo FormatOptions
formatOptionsParserInfo =
  info (formatOptionsParser <**> helper) . mconcat $
  [ progDesc ("Print history file entries. "
              <> "Can merge multiple histories by passing multiple file arguments "
              <> "(merging is best done with '--sort' and '--dedup-lite').")
  ]


commandParser :: Parser Command
commandParser =
  subparser
  . mconcat
  . map (uncurry Options.Applicative.command) $
  [ ("format", Format <$> formatOptionsParserInfo)
  ]


optionsParser :: Parser Options
optionsParser = do
  debug <-
    switch $
    long "debug"
    <> hidden
    <> help "Print debug information on stderr"
  command <- commandParser
  pure Options{..}


optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper) . mconcat $
  [ header "zsh-history"
  , progDesc "Tools to manipulate zsh history files"
  , fullDesc
  ]


parseOptions :: IO Options
parseOptions =
  customExecParser (prefs showHelpOnError) optionsParserInfo
