module Text.Sparta.Search
( search
) where

import Safe
import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text as T

import Text.Sparta.Token
import Text.Sparta.Types

-- | Examine only certain indices of the column.
secondSieve :: (T.Text, Column) -- ^ column & key
            -> [Int]            -- ^ old indices
            -> [Int]            -- ^ new indices
secondSieve (text, col) = filter (\i -> match (S.index col i) text)

-- | Examine all indices of the column.
firstSieve :: (T.Text, Column) -- ^ key & column
           -> [Int]            -- ^ indices
firstSieve (text, col) = S.findIndicesL (flip match text) col

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

-- | Search in the table given a list of query keys.
search :: Query          -- ^ query
       -> Table          -- ^ table
       -> Maybe [T.Text] -- ^ result
search []    _     = Nothing
search query cols
  | queryInvalid query cols = Nothing
  | otherwise               = fmap (selectRow cols) (sieve (pairs cols query))

-- | Determine whether the query is invalid. This happens if a query
-- contains indices outside of the table range or if one column number
-- is used more than once.
queryInvalid :: Query -- ^ query
             -> Table -- ^ table
             -> Bool  -- ^ decision
queryInvalid query cols = outOfRange || duplicates
  where
    outOfRange = any (\i -> i < 0 || i > length cols) indices
    duplicates = length indices /= length (nub indices)
    indices    = map fst query

-- | Match a list of tokens against a plain text.
match :: [Token] -- ^ tokens
      -> T.Text  -- ^ plain text
      -> Bool    -- ^ decision
match [] text = T.null text

match (Plain plain:ts) text
  | T.null text             = False
  | T.isPrefixOf plain text = match ts (T.drop (T.length plain) text)
  | otherwise               = False

match (Question n:ts) text
  | T.length text < n = False
  | otherwise         = match ts (T.drop n text)

match (Asterisk:ts) text
  | T.null text = match ts text
  | otherwise   = match (Asterisk:ts) (T.drop 1 text) ||
                  match ts            text

