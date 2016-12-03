{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Hfm ( search
) where

import Data.Map as M

type Node = Int
type Move = Node
type Path = ([Move], Node)
type Frontier = [Path]
type Graph = M.Map Node [Node]
type Search = (Node -> Bool) -> [Node] -> Frontier -> Maybe Path
  
-- class of searchable actions
class IsSearch a where
  getSearch :: a -> Search

instance IsSearch Search where
  getSearch = id
    
instance SearchFn Search
  
class (IsSearch a) => SearchFn a where
  search :: (Node -> Bool) -> a -> Node -> Maybe Path
  search checkF act n = (getSearch act) checkF [] [([], n)]

-- neighbors of a node in a graph
neighbors :: Graph -> Node -> [Node]
neighbors grph n = undefined

newSearch :: Graph -> (Frontier -> Graph -> Frontier) -> Search
newSearch grph succF = auxSearchF
  where
    auxSearchF :: Search
    auxSearchF checkF ns [] = Nothing
    auxSearchF checkF ns (ftr@(mvs, n):ftrs)
      | checkF n     = Just (mvs, n)
      | n `elem` ns = auxSearchF checkF (n:ns) ftrs
      | otherwise     = auxSearchF checkF (n:ns) (succF ftrs grph)
      
-- bfs action type
newtype BFS = BFS { unBFS :: Search } deriving (IsSearch, SearchFn)
-- create a new bfs action
newBFS :: Graph -> BFS
newBFS grph = BFS . newSearch grph $ succF
  where
    succF :: Frontier -> Graph -> Frontier
    succF (ftr@(mvs, fst):ftrs) grph = ftrs ++ [((mvs++[fst]), newFst) | newFst <- neighbors grph fst]

-- dfs action type
newtype DFS = DFS { unDFS :: Search } deriving (IsSearch, SearchFn)
-- create a new dfs action
newDFS :: Graph -> DFS
newDFS grph = DFS . newSearch grph $ succF
  where
    succF :: Frontier -> Graph -> Frontier
    succF (ftr@(mvs, fst):ftrs) grph = ftrs ++ [((mvs++[fst]), newFst) | newFst <- neighbors grph fst]
  
