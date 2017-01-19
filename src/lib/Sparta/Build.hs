{- |
Module      : Sparta.Build
Description : Data structure initialization
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Implementation of the table building algorithm that creates the table
columns and orders them by their respective complexities. The complexity
function favors cells without any wild-cards, as they are most likely to
filter out most entries.
-}

{-# LANGUAGE OverloadedStrings #-}

module Sparta.Build
( build
) where

import Data.List
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Text as T

import Sparta.Token
import Sparta.Types


-- | Build a table from textual data.
build :: [[T.Text]]          -- ^ cells
      -> Either T.Text Table -- ^ error message | table
build []   = Left "No rows provided"
build [[]] = Right []
build cells
  | same (map length cells) = Right cols3
  | otherwise               = Left "Non-identical row sizes"
  where
    cols1   = replicate (length $ head cells) S.empty -- create empty columns
    cols2   = foldr (zipWith append) cols1 cells      -- append cells
    cols3   = rateSort $ zip [1..] cols2              -- sort by complexity
    same xs = and $ zipWith (==) xs (tail xs)
    append  = flip (S.|>) . tokenize

-- | Return columns sorted by the complexity of each. The higher the rate,
-- the more broader the domain set of the tokens is. Constants used in
-- this function are rather arbitrary and should be reasoned about in the
-- future.
rateSort :: [(Int, Column)] -- ^ columns
         -> [(Int, Column)] -- ^ sorted columns
rateSort = sortBy (\c1 c2 -> compare (columnRate c1) (columnRate c2))
  where
    columnRate        = F.sum . fmap tokensRate . snd
    tokensRate        = sum . map rate
    rate (Plain _)    = 0 :: Integer
    rate (Question _) = 1 :: Integer
    rate Asterisk     = 5 :: Integer
