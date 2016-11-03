 - dostaneme zoznam stlpcov a texty
 - zoberie sa prvy a urobi sa zoznam integerov
 - to su tie pre ktore to matchovalo = toto je O(n)
 - vybrat len tie indexy ktore boli menovane a matchovat len tie
 - a toto dalej opakovat na tom dalsom stlpci atd
 - na konci mame zoznam indexov pre ktore to plati
 - 

 - assign number to each column, starting with 1
 - probably store that in a tuple (zip [1..] columns)
 - sortnut toto podla complexity tych columnov
 - toto vsetko este v builde

 - zobrat najkomplexnejsi column
 - je v query liste?

 - 
module Search
( search
) where

import Data.Word
import qualified Data.Sequence as S

matchingIndices :: Column
                -> T.Text
								-> [Word64]
matchingIndices (Column seq _) = 

columnSelect :: Column
             -> [Word64]
						 -> [

tableSelect :: Table
            -> [Int]
						-> (

type Query = [(Int, T.Text)]

search :: Query
       -> Table
       -> Maybe [T.Text]
search []      _     = []
search [q]     table = 


-- takze pride zoznam ze: [(cislo stlpca, text)]
-- | 
search :: [(Int, T.Text)] -- ^ query
       -> Table           -- ^ data table
			 -> Maybe [T.Text]  -- ^ result
search []      _     = []
search [query] table = tableSelect 
search queries (Table cols) 
  | any (>= (map fst queries)

