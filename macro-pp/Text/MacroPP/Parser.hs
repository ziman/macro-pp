module Text.MacroPP.Parser
    ( Parser
    , Block(..)
    , atom, kwd, ident
    , parens, sqbrackets
    , blocks
    , Text.Parsec.parse
    )
    where

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Text as T

type Parser = Parsec T.Text ()
data Token m = TChar Char | TMacro m deriving Show
data Block m = Verbatim T.Text | MacroBlock m deriving Show

ignore :: Parser a -> Parser ()
ignore = (*> pure ())

blocks :: Parser m -> Parser [Block m]
blocks = fmap aggregate . many . Parser.token

atom :: String -> Parser ()
atom = ignore . try . string

kwd :: String -> Parser ()
kwd s = atom s *> spaces

parens :: Parser a -> Parser a
parens p = kwd "(" *> p <* kwd ")"

sqbrackets :: Parser a -> Parser a
sqbrackets p = kwd "[" *> p <* kwd "]"

ident :: Parser String
ident = many1 identChar <* spaces

identChar :: Parser Char
identChar = satisfy (\c -> isAlpha c || isNumber c || c `elem` "_")

spanChars :: [Token m] -> ([Char], [Token m])
spanChars (TChar c : rest) = case spanChars rest of
    (cs, ts) -> (c:cs, ts)
spanChars rest = ([], rest)

aggregate :: [Token m] -> [Block m]
aggregate ts = case spanChars ts of
    ([], ts) -> aggregate' ts
    (cs, ts) -> Verbatim (T.pack cs) : aggregate' ts
  where
    aggregate' [] = []
    aggregate' (TMacro m : rest) = MacroBlock m : aggregate rest
    aggregate' ts = error "impossible!"

    isChar (TChar _) = True
    isChar _ = False

token :: Parser m -> Parser (Token m)
token macro
    =   TMacro <$> macro
    <|> TChar <$> anyChar
