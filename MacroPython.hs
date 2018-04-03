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
    = Enum String [String]
    deriving Show

parserEnum :: Parser MacroPython
parserEnum = do
    kwd "@macro(enum)"
    kwd "class"
    enumName <- ident
    kwd ":"
    cases <- ident `manyTill` atom "_end_"
    pure $ Enum enumName cases

parser :: Parser MacroPython
parser = parserEnum

toLines :: MacroPython -> [T.Text]
toLines = \case
    Enum n cs -> ("# enum " <> T.pack n) : map (\c ->  "#  -> " <> T.pack c) cs

macroPython :: Macro MacroPython
macroPython = Macro
    { macroParser  = parser
    , macroToLines = toLines
    }
