module Zsh.History
  ( Entry(..)
  , Command
  -- * Parsing
  , entryP
  , historyP
  , parseHistory
  -- * Formatting
  , formatEntry
  ) where

-- base
import Data.Word

-- attoparsec
import Data.Attoparsec.ByteString.Char8

-- bytestring
import Data.ByteString (ByteString)


data Entry = Entry
  { timestamp :: !Word64
  , duration :: !Word64
  , command :: !Command
  }
  deriving Show


type Command = ByteString



{-

* zsh history file format

Example:

: 1568116039:0;cd code/zsh-history
: 1568116554:0;ll
: 1568116568:0;echo hello\\
this\\
is a multiline\\
command.
: 1568116575:0;cat .zsh_history

This is the extended history format of zsh,
see http://zsh.sourceforge.net/Doc/Release/Options.html#History.

Each entry has three values:
- Beginning timestamp in seconds since the epoch
- Duration in seconds
- Command

Notes:
- Line continuations in multi-line commands are indicated by double
  backslashes ("\\") before the newline character.
- If a command ends in double backslashes, zsh will disambiguate by
  adding a space between the double backslashes and the newline before
  writing the command to the history file

-}


commandP :: Parser Command
commandP = scan 0 f <* char '\n'
  where
    -- | 'n' counts the number of backlashes immediately before the current character.
    f :: Int -> Char -> Maybe Int
    f n '\\' = Just $! n + 1
    f n '\n'
      -- newline means continuation if preceded by double backslash, otherwise end of command
      | n >= 2    = Just 0
      | otherwise = Nothing
    f _ _ = Just 0


entryP :: Parser Entry
entryP = do
  _ <- char ':'
  _ <- char ' '
  t <- decimal
  _ <- char ':'
  d <- decimal
  _ <- char ';'
  c <- commandP
  return Entry{ timestamp = t
              , duration = d
              , command = c
              }


historyP :: Parser [Entry]
historyP = many' entryP


parseHistory :: ByteString -> Either String [Entry]
parseHistory = parseOnly (historyP <* endOfInput)



formatEntry :: Entry -> ByteString
formatEntry = error "TODO"
