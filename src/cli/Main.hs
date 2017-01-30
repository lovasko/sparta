{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Monoid
import System.Exit
import Text.Comma
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Options.Applicative as O
import qualified Sparta as S

import Options


-- | Create named columns. In the case that there are no table headers
-- provided, strings "1", "2", ... are used.
columnNames :: Maybe [T.Text]   -- ^ table headers
            -> Int              -- ^ column count
            -> [(T.Text,  Int)] -- ^ column header dictionary
columnNames hdrs len = zip keys [1..len]
  where keys = fromMaybe (map (T.pack . show) [1..len]) hdrs

-- | Split the full query string into the column and value parts based on the
-- position of the '=' character. The first occurrence of '=' is used as the
-- breaking point.
splitQuery :: T.Text           -- ^ query
           -> (T.Text, T.Text) -- ^ column & value
splitQuery query
  | length parts >= 2 = (head parts, T.intercalate "=" (tail parts))
  | length parts == 1 = (head parts, T.empty)
  | otherwise         = (T.empty, T.empty)
  where parts = T.split (== '=') query

-- | Parse a single query argument.
parseQuery :: [(T.Text, Int)]             -- ^ column header dictionary
           -> T.Text                      -- ^ query
           -> Either String (Int, T.Text) -- ^ error message | query
parseQuery dict query
  | T.null query         = Left "No query specified"
  | T.all (/= '=') query = Left "Delimiter '=' missing"
  | T.null col           = Left "No column specified"
  | T.null val           = Left "No value specified"
  | isNothing idx        = Left $ "No such column '" ++ T.unpack col ++ "'"
  | otherwise            = Right (fromJust idx, val)
  where
    (col, val) = splitQuery query
    idx        = lookup col dict

-- | Perform a table search based on the command-line options.
search :: Options                   -- ^ command-line options
       -> (Maybe [T.Text], S.Table) -- ^ table
       -> Either String [[T.Text]]  -- ^ error | result
search opts (headers, table) = sequence queries >>= S.search table
  where
    queries = map (parseQuery columns) (optQueries opts)
    columns = columnNames headers (length table)

-- | Build the query table based on the command-line options.
build :: Options                                 -- ^ command-line options
      -> [[T.Text]]                              -- ^ table
      -> Either String (Maybe [T.Text], S.Table) -- ^ error | headers & table
build opts table
  | optNoHeader opts  = fmap ((,) Nothing) (S.build table)
  | length table >= 2 = fmap ((,) (Just $ head table)) (S.build $ tail table)
  | length table == 1 = Left "Table contains only headers"
  | otherwise         = Left "Table has no headers"

-- | Sparse table query engine.
main :: IO ()
main = do
  opts  <- O.execParser Options.parser
  input <- maybe T.getContents T.readFile (optFile opts)
  case comma input >>= build opts >>= search opts of
    Left err  -> T.putStrLn ("ERROR: " <> T.pack err) >> exitFailure
    Right res -> T.putStr   (uncomma res)             >> exitSuccess
