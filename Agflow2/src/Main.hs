{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative
import Data.List.Split
import Control.Monad
import Debug.Trace
import Data.List
import Data.Ord
import Data.Maybe
import GHC.Exts
import qualified Data.Sequence
import qualified Data.Foldable
import qualified Data.Vector as V

main = do
  n <- readLn :: IO Int
  input <- fmap (take n . lines) getContents
  print . strLToIntL $ input

strToInt :: String -> [Int]
strToInt s = fmap read $ splitOn " " s

--strLToIntL :: [String] -> Int
strLToIntL xs = answer $ map strToInt xs
    
--answer :: [[Int]] -> Int
answer (first:second:third:[]) = count
    where
        sp = reorderings second
        tp = reorderings third
        best = ((zipWith checkRowMatches) <$> (checkerConvert sp first) <*> (checkerConvert tp first))
        -- this is the answer
        count = foldr1 max $ (length . catMaybes <$> best) ++ [1]
        
reorderings :: [Int] -> [[Int]]
reorderings !xs = take len $! map (take len) . tails . cycle $ xs
  where len = length xs
            
check :: Int -> Int -> Maybe Int        
check x y 
    | x == y = Just x
    | otherwise = Nothing
    
checkRowMatches :: Maybe Int -> Maybe Int -> Maybe Int        
checkRowMatches Nothing _ = Nothing
checkRowMatches _  Nothing = Nothing
checkRowMatches (Just x) (Just y) = case (x ==y) of 
                            True -> Just x
                            _ -> Nothing
          
          
sorter x =  reverse1 . sortWith (length . catMaybes) $ x        
        where reverse1 = Data.Foldable.toList . Data.Sequence.reverse . Data.Sequence.fromList 

checkerConvert :: [[Int]] -> [Int] -> [[Maybe Int]]
checkerConvert r f = go
    where r1 = V.fromList `V.map` V.fromList r
          f1 =  V.fromList f
          go = unmap $ checker1 r1 f1
          unmap x =  V.toList <$>  V.toList x 

checker1 :: V.Vector (V.Vector Int) -> V.Vector Int -> V.Vector (V.Vector (Maybe Int))
checker1 r f = sorted
 where  
        checkerResults = (V.zipWith check f) `V.map` r
        sorted = filterRs checkerResults
        filterRs rs =  V.filter (lenFilter) rs
        lenFilter x = length (catMaybes (V.toList x)) > 1       