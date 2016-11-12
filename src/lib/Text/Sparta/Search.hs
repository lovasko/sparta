module Text.Sparta.Search
( search
) where

import Data.List
import Data.Maybe
import Safe
import qualified Data.Sequence as S
import qualified Data.Text as T

import Text.Sparta.Token
import Text.Sparta.Types

-- | Search in the table given a list of query keys.
search :: Table          -- ^ table
       -> Query          -- ^ query
       -> Maybe [T.Text] -- ^ result
search _    []              = Nothing
search cols query
  | queryInvalid cols query = Nothing
  | otherwise               = fmap (selectRow cols) (sieve (pairs cols query))

-- | Examine only certain indices of the column.
secondSieve :: (T.Text, Column) -- ^ key & column
            -> [Int]            -- ^ old indices
            -> [Int]            -- ^ new indices
secondSieve (text, col) = filter (match text . S.index col)

-- | Examine all indices of the column.
firstSieve :: (T.Text, Column) -- ^ key & column
           -> [Int]            -- ^ indices
firstSieve (text, col) = S.findIndicesL (match text) col

-- | Combine the first and second sieves over a list of pairings.
sieve :: [(T.Text, Column)] -- ^ keys & columns
      -> Maybe Int          -- ^ possible row number
sieve []     = Nothing
sieve (x:xs) = headMay $ foldr secondSieve (firstSieve x) xs

-- | Create a column-wise join between the query and the table columns.
pairs :: Table -- ^ table
      -> Query -- ^ query
      -> [(T.Text, Column)]
pairs cols query = mapMaybe findPair cols
  where
    findPair (n, col) = fmap (flip (,) col . snd) (find ((== n) . fst) query)

-- | Select a row from the table.
selectRow :: Table    -- ^ table
          -> Int      -- ^ row number
          -> [T.Text] -- ^ row content
selectRow cols n = map (textify . flip S.index n . snd) cols

-- | Determine whether the query is invalid. This happens if a query
-- contains indices outside of the table range or if one column number
-- is used more than once.
queryInvalid :: Table -- ^ table
             -> Query -- ^ query
             -> Bool  -- ^ decision
queryInvalid cols query = outOfRange || duplicates
  where
    outOfRange = any (\i -> i < 0 || i > length cols) indices
    duplicates = length indices /= length (nub indices)
    indices    = map fst query

-- | Match a list of tokens against a plain text.
match :: T.Text  -- ^ plain text
      -> [Token] -- ^ tokens
      -> Bool    -- ^ decision
match text []               = T.null text
match text (Plain plain:ts)
  | T.null text             = False
  | T.isPrefixOf plain text = match (T.drop (T.length plain) text) ts
  | otherwise               = False
match text (Question n:ts)
  | T.length text < n       = False
  | otherwise               = match (T.drop n text) ts
match text us@(Asterisk:ts)
  | T.null text             = match text ts
  | otherwise               = match (T.tail text) us || match text us

