{-# LANGUAGE OverloadedStrings #-}

module Macro
    ( Macro(..)
    , expand
    ) where

import Parser
import Data.Text as T

data Macro m = Macro
    { macroParser :: Parser m
    , macroToLines :: m -> [T.Text]
    }

indentation :: T.Text -> T.Text
indentation = fst . T.span (== ' ')

layout :: Macro m -> T.Text -> [Block m] -> [T.Text]
layout macro ind [] = []
layout macro ind (Verbatim t : bs) = t : layout macro (indentation $ T.reverse t) bs
layout macro ind (MacroBlock m : bs) = macroToLines macro m `before` layout macro ind bs
  where
    [] `before` ls = ls
    [x] `before` ls = [ind, x] ++ ls
    (x:xs) `before` ls = [ind, x, "\n"] ++ (xs `before` ls)

expand :: Macro m -> T.Text -> Either String T.Text
expand macro t
    = case parse (blocks $ macroParser macro) "input" t of
        Left err -> Left (show err)
        Right bs -> Right $ T.concat (layout macro "" bs)
