module Text.Sparta
( Query  -- *
, Table  -- *
, build  -- [[T.Text]] -> Either T.Text Table
, search -- Table -> Query -> Either T.Text [[T.Text]]
) where

import Text.Sparta.Build
import Text.Sparta.Search
import Text.Sparta.Types

