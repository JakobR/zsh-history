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

-- TODO: Add option "--sort" (simply sort by timestamp)
-- TODO: Add option "--dedup" (so we don't actually need a separate dedup command)
--       Keeps latest of duplicate entry (according to order in the files;
--       with --sort according to timestamp order, i.e., sort is performed before dedup)
-- TODO: Add option "--filter-regex"? might be useful because grep doesn't properly work with multiline commands
data FormatOptions = FormatOptions
  { input :: !Input
  , format :: !OutputFormat
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
  format <-
    option (maybeReader readOutputFormatOptionValue) $
    short 'f'
    <> long "format"
    <> value TextOutputFormat
    <> showDefaultWith showOutputFormatOptionValue
    <> help ("The output format. (values: "
             <> intercalate ", " (showOutputFormatOptionValue <$> [minBound..maxBound])
             <> ")")
  input <-
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
    long "--debug"
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
