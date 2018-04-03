{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import PythonExt
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    source <- TIO.readFile "sample.py"
    case parse (blocks macro) "sample.py" source of
        Left err -> print err
        Right bs -> do
            mapM_ print bs
            TIO.putStr . T.concat $ layout "" bs
