{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Control.Applicative
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.List

type Reel = [Int]
-- a position is a list of the locations of a number in each reel
type Positions = [Int]
-- map of positions
type PositionMap = Map Int Positions
-- numbers in reel are from 1..Max
type Max = Int

main :: IO ()
main = do
  mx <- readLn :: IO Max
  reels <- fmap (fmap readReel . take 3 . lines) getContents
  print $ answer mx reels
  where
    readReel s = read <$> splitOn " " s
    answer mx xs = mostCommonPositionCount $ getPositionMap mx xs

-- get count of most common position
mostCommonPositionCount :: PositionMap -> Int
mostCommonPositionCount ans = maximum count
 where
    count = fmap snd (Map.toList countMap)
    countMap = Map.fromListWith (+) tupleList
    tupleList = fmap pairOne (Map.toList ans)
    pairOne (_,y) = (y,1)

getPositionMap :: Max -> [Reel] -> PositionMap
getPositionMap mx xs = Map.map (rebasePositions mx) mapIndex
  where
    mapIndex = Data.List.foldl' index Map.empty xs
    index m is = Map.unionWith (++) m (Map.fromList $ pairs is)
    pairs ps = zip ps (fmap (: []) [1..])

-- this is the key, we rebase the positions so that they represent rotations of the reel
rebasePositions :: Max -> Positions -> Positions
rebasePositions mx xs@(x:_) = fmap (rebase . normalize) xs
  where
        normalize y = y - x + 1
        rebase y = (y + mx) `mod` mx
rebasePositions _ _ = error "can't rebase empty list"
