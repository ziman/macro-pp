{-# LANGUAGE OverloadedStrings #-}

module PythonExt
    ( Macro(..)
    , macro
    ) where

import Parser
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Monoid
import qualified Data.Text as T

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

macroToLines :: Macro -> [T.Text]
macroToLines = error "not implemented"

indentation :: T.Text -> T.Text
indentation = fst . T.span (== ' ')

layout :: T.Text -> [Block Macro] -> [T.Text]
layout ind [] = []
layout ind (Verbatim t : bs) = t : layout (indentation $ T.reverse t) bs
layout ind (Macro m : bs) = foldr ((++) . fmtLine) (layout ind bs) (macroToLines m)
  where
    fmtLine line = [ind, line, "\n"]
