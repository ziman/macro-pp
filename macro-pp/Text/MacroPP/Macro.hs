module Text.MacroPP.Macro
    ( Macro(..)
    , expand
    ) where

import Data.Char
import Data.Text (Text)
import Data.Text as T

import Text.MacroPP.Parser

data Macro m = Macro
    { macroParser :: Parser m
    , macroPPrinter :: m -> Text
    }

indentation :: Text -> Text
indentation = fst . T.span (`elem` [' ', '\t'])

layout :: Macro m -> Text -> [Block m] -> [Text]
layout macro ind [] = []
layout macro ind (Verbatim t : bs) = t : layout macro (indentation $ T.reverse t) bs
layout macro ind (MacroBlock m : bs) = macroPPrinter macro m : layout macro ind bs
  where
    [] `before` ls = ls
    [x] `before` ls = [ind, x] ++ ls
    (x:xs) `before` ls = [ind, x, "\n"] ++ (xs `before` ls)

expand :: Macro m -> Text -> Either String Text
expand macro t
    = case parse (blocks $ macroParser macro) "input" t of
        Left err -> Left (show err)
        Right bs -> Right $ T.concat (layout macro "" bs)
