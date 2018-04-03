{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Macro
import MacroPython
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    source <- TIO.readFile "sample.py"
    case expand macroPython source of
        Left err  -> putStrLn err
        Right src -> TIO.putStr src
