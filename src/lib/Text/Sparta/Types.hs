module Text.Sparta.Types
( Column
, Query
, Table
, Token(..)
) where

import qualified Data.Sequence as S
import qualified Data.Text as T

-- | Table column defined as a sequence of rows, each row represented
-- as a list of pattern tokens.
type Column = S.Seq [Token]

-- | Search query that consists of column number and a plain text.
type Query = [(Int, T.Text)]

-- | Table cell content patterns.
data Token
  = Plain T.Text -- ^ simple plain text
  | Asterisk     -- ^ any number of wild-card characters
  | Question Int -- ^ exact number of wild-card characters

-- | Table defined as a list of numbered columns.
type Table = [(Int, Column)]

