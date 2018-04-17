module Main where

import System.Environment
import qualified Data.Text.IO as TIO

import Text.MacroPP.Parser
import Text.MacroPP.Macro

import Macro
import Parser        (parser)
import PPrinter      (fmt)
import Util.PPrinter (render)

macro :: Text.MacroPP.Macro.Macro Macro.Macro
macro = Text.MacroPP.Macro.Macro
    { macroParser   = parser
    , macroPPrinter = render "# " . fmt
    }

main :: IO ()
main = do
    fname <- getArgs >>= \case
        [fname] -> return fname
        _ -> error "usage: python-pp pp_FILE.py"

    source <- TIO.readFile fname
    case expand macro source of
        Left err  -> error err
        Right src -> TIO.putStr src
