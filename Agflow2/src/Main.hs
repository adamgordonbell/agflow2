{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Control.Applicative
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  input <- fmap (fmap strToInt . take n . lines) getContents
  print $ answer n input

strToInt :: String -> [Int]
strToInt s = read <$> splitOn " " s

answer :: Int -> [[Int]] -> Int
answer mx xs = count $ countMap $ buildMap mx xs

count :: Map [Int] Int -> Int
count ans = maximum $ fmap snd (Map.toList ans)

countMap :: Map Int [Int] -> Map [Int] Int
countMap ans = Map.fromListWith (+) tupleList
 where
    tupleList = fmap pairOne (Map.toList ans)
    pairOne (_,y) = (y,1)

buildMap ::Int -> [[Int]] -> Map Int [Int]
buildMap mx xs = Map.map (rebaseNumbers mx) mapIndex
  where mapIndex = Data.List.foldl' index Map.empty xs

index :: Map Int [Int] -> [Int] -> Map Int [Int]
index  m xs = Map.unionWith (++) m (Map.fromList $ pairs xs)

pairs :: [Int] -> [(Int,[Int])]
pairs xs = zip xs (fmap (: []) [1..])

rebaseNumbers :: Int -> [Int] -> [Int]
rebaseNumbers mx xs@(x:_) = fmap (rebase . normalize) xs
  where
        normalize y = y - x + 1
        rebase y = (y + mx) `mod` mx
rebaseNumbers _ _ = error "can't rebase empty list"
