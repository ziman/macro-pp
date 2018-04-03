module PythonExt
    ( Macro(..)
    , macro
    ) where

import Parser
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

data Macro
    = MacroTest Int
    deriving Show

macroTest :: Parser Macro
macroTest = do
    kwd "macro("
    i <- read <$> many1 digit
    kwd ")"
    pure $ MacroTest i

macro :: Parser Macro
macro = macroTest
