module Main where

import Control.Applicative
import Data.List.Split
import Control.Monad
import Debug.Trace
import Data.List
import Data.Ord
import Data.Maybe
import GHC.Exts

main = do
  n <- readLn :: IO Int
  input <- fmap (take n . lines) getContents
  print . strLToIntL $ input

strToInt :: String -> [Int]
strToInt s = fmap read $ splitOn " " s

strLToIntL :: [String] -> Int
strLToIntL xs = answer $ map strToInt xs
    
answer :: [[Int]] -> Int
answer (first:second:third:[]) = count
    where
        sp = reorderings secondRow
        tp = reorderings thridRow
        best = (zipWith check1) <$> (checker sp) <*> (checker tp)
        count = head $ (length . catMaybes <$> best) ++ [1]
        
reorderings :: [Int] -> [[Int]]
reorderings xs = take len . map (take len) . tails . cycle $ xs
  where len = length xs
            
check :: Int -> Int -> Maybe Int        
check x y 
    | x == y = Just x
    | otherwise = Nothing
    
check1 :: Maybe Int -> Maybe Int -> Maybe Int        
check1 Nothing _ = Nothing
check1 _  Nothing = Nothing
check1 (Just x) (Just y) = case (x ==y) of 
                            True -> Just x
                            _ -> Nothing
          
          
sorter =  reverse . sortWith (length . catMaybes)            

checker r = takeWhile (lenFilter) sorted
 where  checkerResults = (zipWith check firstRow) <$> r
        sorted = sorter checkerResults
        lenFilter x = length (catMaybes x) > 1
                            
firstRow, secondRow, thridRow :: [Int]
firstRow = [1, 5, 4, 3, 2]
secondRow = [1, 3, 2, 4, 5]
thridRow = [2, 1, 5, 4, 3]

sp = reorderings secondRow
tp = reorderings thridRow