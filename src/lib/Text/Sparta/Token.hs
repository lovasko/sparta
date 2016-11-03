{-# LANGUAGE OverloadedStrings #-}

module Token
( Token(..)
, tokenize
) where

import Data.List.Split
import Data.Monoid
import qualified Data.Text as T

-- | Table cell contents.
data Token
  = Plain T.Text -- ^ simple plaintext
  | Asterisk     -- ^ any number of wildcard characters
  | Question Int -- ^ exact number of wildcard characters
  deriving (Show)

-- | Merge adjacent compatible tokens into one.
simplify :: [Token] -- ^ old tokens
         -> [Token] -- ^ new tokens
simplify (Plain a    : Plain b    : ts) = simplify (Plain (a <> b) : ts)
simplify (Question n : Question m : ts) = simplify (Question (n + m) : ts)
simplify (Asterisk   : Asterisk   : ts) = simplify (Asterisk : ts)
simplify (t                       : ts) = t : simplify ts
simplify []                             = []

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
