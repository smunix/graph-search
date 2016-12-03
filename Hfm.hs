{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}

module Hfm ( search
           , bfs
           , dfs
           , Path
           , BFS
           , Graph
           , SearchGraph
           , SearchFn
) where

import Data.Map as M

type Path = ([Int], Int)
type Frontier = [Path]
type Graph = M.Map Int [Int]
type Search = (Int -> Bool) -> [Int] -> Frontier -> Maybe (Path)

-- a bit of GADTs :-)
data SearchGraph sf where
  SearchGraph :: sf -> Graph -> SearchGraph sf
  
class IsNode n where
  
instance Show (SearchGraph sf) where
  show (SearchGraph sf grph) = show grph

instance (IsSearch sf) => IsSearch (SearchGraph sf) where
  getSearch (SearchGraph sf grph) = getSearch sf

instance (SearchFn sf) => SearchFn (SearchGraph sf)

-- class of searchable algorithms
class IsSearch a where
  getSearch :: a -> Search
  
class (IsSearch a) => SearchFn a where
  search :: a -> (Int -> Bool) -> Int -> Maybe (Path)
  search a checkF n = (getSearch a) checkF [] [([], n)]


instance IsSearch (Search) where
  getSearch = id
  
instance SearchFn (Search)

-- neighbors of a node in a graph
neighbors :: Graph -> Int -> [Int]
neighbors grph n = M.findWithDefault [] n grph

newSearch :: Graph -> (Path -> Frontier -> Graph -> Frontier) -> Search
newSearch grph succF = auxSearchF
  where
    auxSearchF :: Search
    auxSearchF checkF ns [] = Nothing
    auxSearchF checkF ns (ftr@(mvs, n):ftrs)
      | checkF n      = Just (mvs, n)
      | n `elem` ns   = auxSearchF checkF (ns) ftrs
      | otherwise     = auxSearchF checkF (n:ns) (succF ftr ftrs grph)
      
-- bfs action type
newtype BFS = BFS { unBFS :: Search } deriving (IsSearch, SearchFn)
-- create a new bfs action
newBFS :: Graph -> SearchGraph BFS
newBFS grph = SearchGraph (BFS . newSearch grph $ succF) grph
  where
    succF :: Path -> Frontier -> Graph -> Frontier
    succF path@(ns, n) ftrs g = ftrs ++ [((ns++[n]), n8br) | n8br <- neighbors g n]

-- dfs action type
newtype DFS = DFS { unDFS :: Search } deriving (IsSearch, SearchFn)
-- create a new dfs action
newDFS :: Graph -> SearchGraph DFS
newDFS grph = SearchGraph (DFS . newSearch grph $ succF) grph
  where
    succF :: Path -> Frontier -> Graph -> Frontier
    succF path@(ns, n) ftrs g = [((ns++[n]), n8br) | n8br <- neighbors g n] ++ ftrs
    
dfs = newDFS
bfs = newBFS
  
