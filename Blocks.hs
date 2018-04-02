module Blocks
    ( Block
    )
    where

data Block = Block String [Block] deriving (Eq, Ord, Show)

indentation :: String -> (Int, String)
indentation s = (length whitespace, content)
  where
    (whitespace, content) = span (== ' ') s

indentedAbove :: Int -> (Int, String) -> Bool
indentedAbove ind (_, "") = True
indentedAbove ind (i, _ ) = i > ind

parseBlocks :: [(Int, String)] -> [Block]
parseBlocks [] = []
parseBlocks ((ind, line) : lines) = 
    Block line (parseBlocks subLines) : parseBlocks rest
  where
    (subLines, rest) = span (indentedAbove ind) lines

layout :: Block -> [String]
layout (Block s subs) = s : map indent (concatMap layout subs)

indent :: String -> String
indent = ("    " ++)

parse :: [String] -> [Block]
parse = parseBlocks . map indentation
