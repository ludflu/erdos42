module Main where

import Sidon


main :: IO ()
main = do
  sidona <- tryMaybeSidon 4 100 100
  case sidona of
    Just a -> do putStrLn $ "Found Sidon set: " ++ show a
                 b <- findDisjointSidonSets a 4 100 100
                 putStrLn $ "Found disjoiint Sidon set: " ++ show b
    Nothing -> putStrLn "Failed to find Sidon set after 100 tries"
