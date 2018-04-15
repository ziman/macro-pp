module Main where

import System.Environment
import qualified Data.Text.IO as TIO

import Text.MacroPP.Parser
import Text.MacroPP.Macro

import PythonPP

main :: IO ()
main = do
    fname <- getArgs >>= \case
        [fname] -> return fname
        _ -> error "usage: python-pp pp_FILE.py"

    source <- TIO.readFile fname
    case expand macroPython source of
        Left err  -> error err
        Right src -> TIO.putStr src
