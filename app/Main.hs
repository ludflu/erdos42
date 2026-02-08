{-# LANGUAGE BlockArguments #-}
module Main where

import Data.List (tails, nub)
import qualified Data.Set as S
import System.Random (randomRIO)

goodExample = [1,2,5,7]

badExample = [1,2,3]

uniquePairs :: [a] -> [(a, a)]
uniquePairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

doubles :: [a] -> [(a,a)]
doubles l = [(x,x) | x <- l]

pairWise :: [a] -> [(a,a)]
pairWise l = uniquePairs l ++ doubles l

-- all pairwise sums are distinct
siddon :: [Int] -> Bool
siddon nbrs = let pairs = pairWise nbrs
                  psums = map (uncurry (+)) pairs
               in length psums == length (nub psums)

differenceSet :: (Num a, Eq a, Ord a) => [a] -> S.Set a
differenceSet s = let dlist = [ a - b
                                | a <- s
                                , b <- s
                                , a /= b 
                                ]
                   in S.fromList (0 : dlist)


-- Function to get 'n' unique random numbers from the range [lo..hi]
getUniqueRandoms :: Int -> Int -> Int -> IO [Int]
getUniqueRandoms n lo hi = go n S.empty []
  where
    go 0 _ acc = return acc
    go remaining seen acc = do
      r <- randomRIO (lo, hi)
      if S.member r seen
        then go remaining seen acc
        else go (remaining - 1) (S.insert r seen) (r : acc)

isSidon = siddon goodExample
notSidon = siddon badExample

maybeSidon :: Int -> Int -> IO (Maybe [Int])
maybeSidon size upper = do rs <- getUniqueRandoms size 0 upper
                           let result = if siddon rs then Just rs else Nothing
                           return result

tryMaybeSidon :: Int -> Int -> Int -> IO (Maybe [Int])
tryMaybeSidon _ _ 0 = return Nothing
tryMaybeSidon size upper maxTries = do
  result <- maybeSidon size upper
  case result of
    Just _  -> return result
    Nothing -> tryMaybeSidon size upper (maxTries - 1)

main :: IO ()
main = do
  result <- tryMaybeSidon 4 100 100
  case result of
    Just xs -> putStrLn $ "Found Sidon set: " ++ show xs
    Nothing -> putStrLn "Failed to find Sidon set after 100 tries"
