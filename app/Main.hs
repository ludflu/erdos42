{-# LANGUAGE BlockArguments #-}
module Main where

import Data.List (tails, nub)
import qualified Data.Set as S
import System.Random (randomRIO)

uniquePairs :: [a] -> [(a, a)]
uniquePairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

doubles :: [a] -> [(a,a)]
doubles l = [(x,x) | x <- l]

pairWise :: [a] -> [(a,a)]
pairWise l = uniquePairs l ++ doubles l

-- all pairwise sums are distinct
sidon :: [Int] -> Bool
sidon nbrs = let pairs = pairWise nbrs
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

disjointDifferenceSidonSet :: [Int] -> [Int] -> Bool
disjointDifferenceSidonSet a b = let sidon_a = sidon a
                                     sidon_b = sidon b
                                     diff_a = differenceSet a
                                     diff_b = differenceSet b
                                     zeroSet = S.singleton 0
                                  in sidon_a && sidon_b && S.intersection diff_a diff_b == zeroSet


maybeSidon :: Int -> Int -> IO (Maybe [Int])
maybeSidon size upper = do rs <- getUniqueRandoms size 0 upper
                           let result = if sidon rs then Just rs else Nothing
                           return result

tryMaybeSidon :: Int -> Int -> Int -> IO (Maybe [Int])
tryMaybeSidon _ _ 0 = return Nothing
tryMaybeSidon size upper maxTries = do
  result <- maybeSidon size upper
  case result of
    Just _  -> return result
    Nothing -> tryMaybeSidon size upper (maxTries - 1)

example :: [Integer]
example = [7,83,67]


findDisjointSidonSets :: [Int] -> Int -> Int -> Int -> IO (Maybe [Int])
findDisjointSidonSets sidona size upper maxTries = do
  sidonb <- tryMaybeSidon size upper maxTries
  case sidonb of
    Just b -> if disjointDifferenceSidonSet sidona b then 
                return $ Just b 
              else 
                findDisjointSidonSets sidona size upper (maxTries - 1)

    Nothing -> findDisjointSidonSets sidona size upper (maxTries - 1)


main :: IO ()
main = do
  sidona <- tryMaybeSidon 4 100 100
  case sidona of
    Just a -> do putStrLn $ "Found Sidon set: " ++ show a
                 b <- findDisjointSidonSets a 4 100 100
                 putStrLn $ "Found disjoiint Sidon set: " ++ show b
    Nothing -> putStrLn "Failed to find Sidon set after 100 tries"
