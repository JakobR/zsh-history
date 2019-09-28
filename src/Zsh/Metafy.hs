module Zsh.Metafy
  ( unmetafy
  , Metafied(..)
  ) where

-- base
import Data.Bits
import Data.Word

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B


-- | Meta together with the character following Meta denotes the
-- character which is the exclusive or of 32 and the character
-- following Meta.  This is used to represent characters which
-- otherwise have special meaning for zsh.  These are the characters
-- for which the imeta() test is true: the null character, and the
-- characters from Meta to Marker.
--
-- Source: zsh source tree, file @Src/zsh.h@.
meta :: Word8
meta = 0x83

-- marker :: Word8
-- marker = 0xA2


newtype Metafied a = Metafied { getMetafied :: a }
  deriving (Eq, Ord, Show)


-- | See function @unmetafy@ in zsh source tree, file @Src/utils.c@.
unmetafy :: Metafied ByteString -> ByteString
unmetafy (Metafied bs) = fst $ B.unfoldrN bslen unmetafyAt 0
  where
    bslen = B.length bs

    unmetafyAt :: Int -> Maybe (Word8, Int)
    unmetafyAt i
      | i >= bslen =
          Nothing
      | c == meta =
          if i+1 < bslen
          then
            let d = B.index bs (i+1) `xor` 32
            in Just (d, i+2)
          else
            -- The last character is Meta. This should never be the case
            -- in valid metafied strings, so just drop this lone Meta.
            Nothing
      | otherwise =
          Just (c, i+1)
      where
        c = B.index bs i
