module Types
( Column(..)
, Table(..)
) where

import Data.List

import qualified Data.Sequence as S

import Token

-- | Table column.
data Column = Column
              (S.Seq [Token]) -- ^ data
              Integer         -- ^ complexity rating

-- | Data table.
data Table = Table [Column]

-- | Pretty-printing of the Table type.
instance Show Table where
  show (Table cols) = concat $ intersperse "\n" (["Table"] ++ map show cols)

-- | Pretty-printing of the Column type.
instance Show Column where
  show (Column cells comp) =
    unwords [ "Column"
            , "comp", show comp
            , "len", show $ S.length cells ]

