module Main where

import Control.Applicative
import Data.List.Split
import Control.Monad
import Debug.Trace
import Data.List
import Data.List.LCS.HuntSzymanski
import Data.Ord
import Data.List.Ordered
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
answer x1@(x:xs) = foldr1 max $ (length . lis) <$> x1 

-- foldr1 min $ (length . lcs x) <$> xs

doublelist :: [[Int]] -> [[Int]]
doublelist [] = []
doublelist (x:xs) = (x++x):doublelist xs

t :: [[Int]] -> [[[Int]]]
t rs = reorderings <$> rs

reorderings :: [Int] -> [[Int]]
reorderings xs = take len . map (take len) . tails . cycle $ xs
  where len = length xs
  
longestSubstring ::(Eq a, Ord a) => [a] -> [a] -> [a]
longestSubstring first second = head $ reverse $ sortBy (comparing length) comparisons
  where comparisons = concatMap (\x -> map (sharedPrefix x) $ tails second) (tails first)

  
sharedPrefix :: (Eq a, Ord a) => [a] -> [a] -> [a]
sharedPrefix (a:as) (b:bs)
  | a==b = a:sharedPrefix as bs
  | otherwise = []
sharedPrefix _ _ = []

lis :: Ord a => [a] -> [a]
lis = maximumBy (comparing length) . map Data.List.Ordered.nub  . filter Data.List.Ordered.isSorted . subsequences                 
--    longest                    <-- unique <-- increasing    <-- all      
 
windowed :: Int -> [a] -> [[a]]
windowed size ls = 
	(case ls of 
		[] -> []
		x:xs -> 
			if length ls >= size then 
				(take size ls) : windowed size xs
			else windowed size xs)
            
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
                            

checker r = takeWhile (lenFilter) sorted
 where  checkerResults = (zipWith check firstRow) <$> r
        sorted = reverse . sortWith (length . catMaybes) $ checkerResults
        lenFilter x = length (catMaybes x) > 1
                            
firstRow, secondRow, thridRow :: [Int]
firstRow = [1, 5, 4, 3, 2]
secondRow = [1, 3, 2, 4, 5]
thridRow = [2, 1, 5, 4, 3]

sp = reorderings secondRow
tp = reorderings thridRow