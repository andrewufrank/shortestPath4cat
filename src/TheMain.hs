-----------------------------------------------------------------------------
--
-- Module      :   a test  
------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main     where      -- must have Main (main) or Main where

 
-- import           Lib.DirTree
-- import           Lib.OpenClass

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe ( fromMaybe )
dijkstra
    :: (Ord cost , Ord node)
    => ((cost , node) -> [(cost , node)]) -- ^ Where we can go from a node and the cost of that
    -> node                               -- ^ Where we want to get to
    -> (cost , node)                      -- ^ The start position
    -> Maybe (cost , node)                -- ^ Maybe the answer. Maybe it doesn't exist
dijkstra next target start = search mempty (Set.singleton start)
    where
        search visited toBeVisited = case Set.minView toBeVisited of
            Nothing -> Nothing
            Just ((cost , vertex) , withoutVertex)
                | vertex == target            -> Just (cost , vertex)
                | vertex `Set.member` visited -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = Set.insert vertex visited
                    withNext = foldr Set.insert withoutVertex $ next (cost , vertex)

graph =
    Map.fromList
        [ ('a' , [(1 , 'b') , (5 , 'c')])
        , ('b' , [(2 , 'c')])
        , ('c' , [(1 , 'a') , (5 , 'b')])
        ]

-- Output:
-- Just (3,'c')
main1 = print $ dijkstra step 'c' (0 , 'a')
    where
        step :: (Int , Char) -> [(Int , Char)]
        step (cost , node) =
            [ (cost + edgeCost , child)
            | (edgeCost , child) <- fromMaybe [] $ Map.lookup node graph
            ]

main :: IO ()
main =  do  -- with tests in other modules
    main1
    -- dirMain
    -- openMain
    return ()

