module Build
( build
) where

import qualified Data.Sequence as S
import qualified Data.Text as T

import Token
import Types

-- | Build a table from textual data.
build :: [[T.Text]]  -- ^ cells
      -> Maybe Table -- ^ table
build []   = Nothing
build [[]] = Nothing
build cells
  | same (map length cells) = build'
  | otherwise               = Nothing
  where
    build'    = Just $ Table $ foldr rowAppend columns cells
    rowAppend = flip $ zipWith columnAppend
    colcnt    = length $ head cells
    columns   = replicate colcnt (Column S.empty 0)

-- | Parse cell content and append it to a column.
columnAppend :: Column -- ^ old column
             -> T.Text -- ^ cell content
             -> Column -- ^ new column
columnAppend (Column cells comp) text = Column newCells newComp
  where
    tokens   = tokenize text
    newComp  = comp + complexity tokens
    newCells = cells S.|> tokens

-- | Rate the complexity of a set of tokens. The higher the rate, the more
-- complex the tokens are. Constants used in this function are rather
-- arbitrary and should be reasoned about in the future.
complexity :: [Token] -- ^ tokens
           -> Integer -- ^ complexity
complexity [] = 0
complexity (t:ts) = rate t + complexity ts
  where
    rate (Plain _)    = 0
    rate (Question _) = 1
    rate Asterisk     = 5

-- | Find out if all elements of a list are the same.
same :: (Eq a)
     => [a]  -- ^ list
     -> Bool -- ^ decision
same []       = True
same [_]      = True
same (x:y:zs) = x == y && same (y:zs)

