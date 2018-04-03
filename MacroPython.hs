{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module MacroPython
    ( MacroPython(..)
    , macroPython
    ) where

import Parser
import Macro

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Monoid
import qualified Data.Text as T

data MacroPython
    = MacroTest Int
    deriving Show

parserMacroTest :: Parser MacroPython
parserMacroTest = do
    kwd "macro("
    i <- read <$> many1 digit
    kwd ")"
    pure $ MacroTest i

parser :: Parser MacroPython
parser = parserMacroTest

toLines :: MacroPython -> [T.Text]
toLines = \case
    MacroTest i -> ["## macro test " <> T.pack (show i) <> " ##"]

macroPython :: Macro MacroPython
macroPython = Macro
    { macroParser  = parser
    , macroToLines = toLines
    }
