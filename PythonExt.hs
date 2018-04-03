{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module PythonExt
    ( Macro(..)
    , macro
    , layout
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
macroToLines = \case
    MacroTest i -> ["## macro test " <> T.pack (show i) <> " ##"]

indentation :: T.Text -> T.Text
indentation = fst . T.span (== ' ')

layout :: T.Text -> [Block Macro] -> [T.Text]
layout ind [] = []
layout ind (Verbatim t : bs) = t : layout (indentation $ T.reverse t) bs
layout ind (Macro m : bs) = macroToLines m `before` layout ind bs
  where
    [] `before` ls = ls
    [x] `before` ls = [ind, x] ++ ls
    (x:xs) `before` ls = [ind, x, "\n"] ++ (xs `before` ls)
