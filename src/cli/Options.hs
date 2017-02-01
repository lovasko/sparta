module Options
( Options(..)
, parser
) where

import Options.Applicative
import qualified Data.Text as T

-- | Command-line options.
data Options = Options
  { optNoHeader :: Bool
  , optQueries  :: [T.Text]
  , optFile     :: Maybe String }

-- | Table data file option.
parseFile :: Parser (Maybe String) -- ^ parser
parseFile = optional $ strArgument (metavar "FILE")

-- | Query option.
parseQueries :: Parser [T.Text] -- ^ parser
parseQueries = fmap (map T.pack) $ many $ strOption
   $ short   'q'
  <> long    "query"
  <> metavar "COLUMN:QUERY"
  <> help    "Query for the table data"

-- | No CSV header line option.
parseNoHeader :: Parser Bool -- ^ parser
parseNoHeader = switch
   $ short   'n'
  <> long    "no-header"
  <> help    "Header-less input table (column names are 1, 2, ...)"

-- | Command-line user interface.
optionsParser :: Parser Options -- ^ parser
optionsParser = Options
  <$> parseNoHeader
  <*> parseQueries
  <*> parseFile

-- | Parser of the command-line options.
parser :: ParserInfo Options -- ^ parser
parser = info (helper <*> optionsParser) (header desc <> fullDesc)
  where desc = "sparta - sparse table query engine operating on CSV files"
