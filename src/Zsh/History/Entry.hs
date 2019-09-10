module Zsh.History.Entry
  ( Entry(..)
  , Command
  ) where

-- base
import Data.Int

-- bytestring
import Data.ByteString (ByteString)


data Entry = Entry
  { timestamp :: !Int64
  , duration :: !Int64
  , command :: !Command
  }
  deriving Show

type Command = ByteString
