{- |
Module      : Text.Sparta.Token
Description : Matching pattern tokenization
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Implementation of the search pattern parsing and tokenization along with
utility functions to simplify the rules and render back into a text.
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Sparta.Token
( textify
, tokenize
) where

import Data.List.Split
import Data.Monoid
import qualified Data.Text as T

import Text.Sparta.Types

-- | Merge adjacent compatible tokens into one.
simplify :: [Token] -- ^ old tokens
         -> [Token] -- ^ new tokens
simplify (Plain a    : Plain b    : ts) = simplify (Plain (a <> b) : ts)
simplify (Question n : Question m : ts) = simplify (Question (n + m) : ts)
simplify (Asterisk   : Asterisk   : ts) = simplify (Asterisk : ts)
simplify (t                       : ts) = t : simplify ts
simplify []                             = []

-- | Convert a list of tokens into their textual representation.
textify :: [Token] -- ^ tokens
        -> T.Text  -- ^ text
textify [] = T.empty
textify (Plain text:ts) = text              <> textify ts
textify (Question n:ts) = T.replicate n "?" <> textify ts
textify (Asterisk  :ts) = "*"               <> textify ts

-- | Process text parts into tokens, while implementing the backslash
-- escaping rules.
process :: [T.Text] -- ^ text parts
        -> [Token]  -- ^ tokens
process []      = []
process [text]
  | text == "*" = [Asterisk]
  | text == "?" = [Question 1]
  | otherwise   = [Plain text]
process (x:y:ts)
  | x == "*"               = Asterisk   : process (y:ts)
  | x == "?"               = Question 1 : process (y:ts)
  | x == "\\" && y == "*"  = Plain "*"  : process ts
  | x == "\\" && y == "?"  = Plain "?"  : process ts
  | x == "\\" && y == "\\" = Plain "\\" : process ts
  | otherwise              = Plain x    : process (y:ts)

-- | Chop up text into tokens.
tokenize :: T.Text  -- ^ text
         -> [Token] -- ^ tokens
tokenize text = simplify $ process $ map T.pack $ filter (not . null) parts
  where parts = (split . oneOf) ['?', '*', '\\'] (T.unpack text)

