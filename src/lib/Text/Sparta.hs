{- |
Module      : Text.Sparta
Description : Sparse table database
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Sparse table is a reverse database where the table contains search
patterns along with data stored in rows, which can be queried by
plain-text keys. The search algorithm is stable, meaning that the search
results are returned in the same order as they appear in the original
table. This feature makes the database a great fit for storing data that
contain lots of general rules with a few important exceptions, e.g.
configuration management.

This module serves as a front-end import that only re-exports the public
API of the project.
-}

module Text.Sparta
( Query  -- *
, Table  -- *
, build  -- [[T.Text]] -> Either T.Text Table
, search -- Table -> Query -> Either T.Text [[T.Text]]
) where

import Text.Sparta.Build
import Text.Sparta.Search
import Text.Sparta.Types

