module Text.Sparta.Build
( build
) where

import Data.List
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Text as T

import Text.Sparta.Token
import Text.Sparta.Types

-- | Build a table from textual data.
build :: [[T.Text]]  -- ^ cells
      -> Maybe Table -- ^ table
build []   = Nothing
build [[]] = Nothing
build cells
  | same (map length cells) = Just cols3
  | otherwise               = Nothing
  where
    cols1 = replicate (length $ head cells) S.empty
    cols2 = foldr (zipWith columnAppend) cols1 cells
    cols3 = compSorted $ zip [1..] cols2

-- | Return columns sorted by the complexity of each.
compSorted :: [(Int, Column)] -- ^ columns
           -> [(Int, Column)] -- ^ sorted columns
compSorted = sortBy (\c1 c2 -> compare (columnRate c1) (columnRate c2))
  where columnRate = F.sum . fmap rate . snd

-- | Parse cell content and append it to a column.
columnAppend :: T.Text -- ^ cell content
             -> Column -- ^ old column
             -> Column -- ^ new column
columnAppend text col = col S.|> tokenize text

-- | Rate the complexity of a set of tokens. The higher the rate, the more
-- complex the tokens are. Constants used in this function are rather
-- arbitrary and should be reasoned about in the future.
rate :: [Token] -- ^ tokens
     -> Integer -- ^ complexity
rate = foldr (\t s -> s + complexity t) 0
  where
    complexity (Plain _)    = 0
    complexity (Question _) = 1
    complexity Asterisk     = 5

-- | Find out if all elements of a list are the same.
same :: (Eq a)
     => [a]  -- ^ list
     -> Bool -- ^ decision
same xs = and $ zipWith (==) xs (tail xs)

