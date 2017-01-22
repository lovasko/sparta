module Options
( Options(..)
, parser
) where

import Options.Applicative
import qualified Data.Text as T

-- | Command-line options.
data Options = Options
  { optFile     :: Maybe String
  , optQueries  :: [T.Text]
  , optNoHeader :: Bool }

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
  <> help    "Input table does not contain header line"

-- | Command-line user interface.
optionsParser :: Parser Options -- ^ parser
optionsParser = Options
  <$> parseFile
  <*> parseQueries
  <*> parseNoHeader

-- | Parser of the command-line options.
parser :: ParserInfo Options -- ^ parser
parser = info (helper <*> optionsParser) (header desc <> fullDesc)
  where desc = "sparta - sparse table query engine"
