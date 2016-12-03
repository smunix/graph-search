module Main ( main
            , pathGraph
            , path
            ) where

import qualified Data.Map as M
import qualified Hfm as H

-- a graph
gph = M.fromList [(1, [2, 3, 4]), (2, [4]), (3, [5]), (4, [8]), (5, [7, 8]), (8, [9])]

path g (s, d) algo = H.search (algo g) (== d) s

pathGraph :: (H.SearchFn a) => (Int, Int) -> (H.Graph -> H.SearchGraph a) -> Maybe H.Path
pathGraph = path gph

say h v = putStrLn $ h ++ " " ++ show v

main :: IO ()
main = do
  say "pathGraph (1, 9) H.bfs" $ maybe ([], 0) id (pathGraph (1, 9) H.bfs)
  say "pathGraph (1, 9) H.dfs" $ maybe ([], 0) id (pathGraph (1, 9) H.dfs)
  say "pathGraph (1, 9) H.bfs" $ pathGraph (1, 9) H.bfs
  say "pathGraph (1, 9) H.dfs" $ pathGraph (1, 9) H.dfs
  return ()
