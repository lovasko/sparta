module Text.Sparta.Types
( Column(..)
, Table(..)
, Token(..)
) where

import Data.List
import qualified Data.Sequence as S
import qualified Data.Text as T

data Token
  = Plain T.Text -- ^ simple plaintext
  | Asterisk     -- ^ any number of wildcard characters
  | Question Int -- ^ exact number of wildcard characters
  deriving (Show)

-- | Table column.
data Column = Column
              (S.Seq [Token]) -- ^ data
              Integer         -- ^ complexity rating

-- | Data table.
type Table = [(Int, Column)]

-- | Pretty-printing of the Column type.
instance Show Column where
  show (Column cells comp) =
    unwords [ "Column"
            , "comp", show comp
            , "len", show $ S.length cells ]

