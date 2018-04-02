module Main where

import Parser
import Blocks

main :: IO ()
main = do
    xs <- parse . lines <$> readFile "sample.py"
    putStrLn . unlines $ concatMap layout xs
