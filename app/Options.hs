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
  -- | Merge !MergeOptions
  deriving Show

-- TODO: Add option "--filter-regex"? might be useful because grep doesn't properly work with multiline commands
data FormatOptions = FormatOptions
  { optInput :: !Input
  , optFormat :: !OutputFormat
  , optSort :: !Bool
  , optDedup :: !Bool
  }
  deriving Show

data Input
  = Stdin
  | File !FilePath
  deriving Show

data OutputFormat
  = TextOutputFormat
  | ZshOutputFormat
  | JSONOutputFormat
  deriving (Show, Bounded, Enum)


readInputOptionValue :: ReadM Input
readInputOptionValue = do
  path <- str
  case path of
    "-" -> pure Stdin
    _ -> pure (File path)


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
    <> value TextOutputFormat
    <> showDefaultWith showOutputFormatOptionValue
    <> help ("The output format. (values: "
             <> intercalate ", " (showOutputFormatOptionValue <$> [minBound..maxBound])
             <> ")")
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
  optInput <-
    argument readInputOptionValue $
    metavar "INPUT"
    <> value Stdin
    <> showDefaultWith (const "\"-\"")
    <> help "Input file, or \"-\" to read from stdin instead"
    <> action "file"
  pure FormatOptions{..}


formatOptionsParserInfo :: ParserInfo FormatOptions
formatOptionsParserInfo =
  info (formatOptionsParser <**> helper) . mconcat $
  [ progDesc "Print history file entries" ]


commandParser :: Parser Command
commandParser =
  subparser
  . mconcat
  . map (uncurry Options.Applicative.command) $
  [ ("format", Format <$> formatOptionsParserInfo)
  -- , ("merge", Merge <$> mergeOptionsParserInfo)
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
