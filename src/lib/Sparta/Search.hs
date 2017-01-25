{- |
Module      : Sparta.Search
Description : Search algorithm
Copyright   : (c) 2016-2017 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Implementation of the search algorithm that performs the pairing of
columns and query keys and returns a list of all matching rows. It makes
use of the fact that the columns sorted ascendingly by complexity and
therefore evalues the query in that order.
-}

module Sparta.Search
( search
) where

import Data.List (find, nub)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S
import qualified Data.Text as T

import Sparta.Token
import Sparta.Types


-- | Search in the table given a list of query keys.
search :: Table                    -- ^ table
       -> Query                    -- ^ query
       -> Either String [[T.Text]] -- ^ results
search _    [] = Left "Empty query"
search table query
  | outOfRange = Left "Query indices out of range"
  | duplicates = Left "Query indices contain duplicates"
  | otherwise  = Right $ map selectRow (sieve (pairs table query))
  where
    outOfRange  = any (\i -> i < 0 || i > length table) indices
    duplicates  = length indices /= length (nub indices)
    indices     = map fst query
    selectRow n = map (textify . flip S.index n . snd) table

-- | Match each key over corresponding column and filter out the numbers
-- of rows that match keys in all columns.
sieve :: [(T.Text, Column)] -- ^ keys & columns
      -> [Int]              -- ^ possible row number
sieve []     = []
sieve (x:xs) = foldr secondSieve (firstSieve x) xs
  where
    firstSieve  (text, col) = S.findIndicesL (match text) col
    secondSieve (text, col) = filter (match text . S.index col)

-- | Create a column-wise join between the query and the table columns.
pairs :: Table              -- ^ table
      -> Query              -- ^ query
      -> [(T.Text, Column)] -- ^ keys & columns
pairs table query = mapMaybe findPair table
  where
    findPair (n, col) = fmap (createPair col) (findMatch n)
    createPair col    = flip (,) col . snd
    findMatch n       = find ((== n) . fst) query

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
  | otherwise               = match (T.tail text) us || match text ts
