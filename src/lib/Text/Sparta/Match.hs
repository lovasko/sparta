module Sparta.Match
( match
) where

import qualified Data.Text as T

import Token

-- | Match a list of tokens against a plain text.
match :: [Token] -- ^ tokens
      -> T.Text  -- ^ plain text
      -> Bool    -- ^ decision
match [] text
  | T.null text = True
  | otherwise   = False

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

