{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Hfm ( search
           , getSearch
) where

import Data.Map as M

type State = Int
type Path = ([Move], State)
type Frontier = [Path]
type Move = State
type Graph = M.Map State [State]

type Search = (State -> Bool) -> [State] -> Frontier -> Maybe Path

neighbors :: Graph -> State -> [State]
neighbors grph st = undefined

newSearch :: Graph -> (Frontier -> Graph -> Frontier) -> Search
newSearch grph succF = auxSearchF
  where
    auxSearchF :: Search
    auxSearchF checkF sts [] = Nothing
    auxSearchF checkF sts (ftr@(mvs, st):ftrs)
      | checkF st     = Just (mvs, st)
      | st `elem` sts = auxSearchF checkF (st:sts) ftrs
      | otherwise     = auxSearchF checkF (st:sts) (succF ftrs grph)
      
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
    
-- class of searchable actions
class IsSearch a where
  getSearch :: a -> Search

instance IsSearch Search where
  getSearch = id
    
instance SearchFn Search
  
class (IsSearch a) => SearchFn a where
  search :: (State -> Bool) -> a -> State -> Maybe Path
  search checkF act st = (getSearch act) checkF [] [([], st)]
  
main :: IO()
main = putStrLn "Hellow World"
