{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.List

main = do
  n <- readLn :: IO Int
  input <- fmap (take n . lines) getContents
  print $ strLToIntL n input

strToInt :: String -> [Int]
strToInt s = read <$> splitOn " " s

strLToIntL :: Int -> [String] -> Int
strLToIntL max xs = answer max  (fmap strToInt xs)

answer :: Int -> [[Int]] -> Int
answer max xs = count $ countMap $ buildMap max xs

count :: Map [Int] Int -> Int
count map = maximum $ fmap snd (Map.toList map)

countMap :: Map Int [Int] -> Map [Int] Int
countMap ans = Map.fromListWith (+) $ fmap (\(x,y) -> (y,1)) (Map.toList ans)

buildMap ::Int -> [[Int]] -> Map Int [Int]
buildMap max xs = Map.map (rebaseNumbers max) $ Data.List.foldl' (flip index) Map.empty xs

index :: [Int] -> Map Int [Int] -> Map Int [Int]
index xs  m = Map.unionWith (++) m (Map.fromList $ pairs xs)

pairs :: [Int] -> [(Int,[Int])]
pairs xs = zip xs (fmap (\x -> [x]) [1..])

rebaseNumbers :: Int -> [Int] -> [Int]
rebaseNumbers max xs@(x:_) = fmap (\y -> (y + max) `mod` max) shift
  where shift = fmap (\y -> y - x + 1) xs
