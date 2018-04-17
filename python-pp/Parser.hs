module Parser (parser) where

import Text.Parsec
import Text.MacroPP.Parser
import Macro

ftype :: Parser Type
ftype = do
    hd <- ident
    args <- option [] $ sqbrackets (ftype `sepBy` kwd ",")
    pure $ Type hd args

field :: Parser Field
field = do
    name <- ident
    kwd "="
    ty <- ftype
    pure $ Field name ty

ctor :: Parser Ctor
ctor = do
    name <- ident
    fields <- option [] $ parens (field `sepBy` kwd ",")
    pure $ Ctor name fields

parserEnum :: Parser Macro
parserEnum = do
    kwd "@enum(deriving=Codec)"
    kwd "class"
    name <- ident
    kwd ":"
    ctors <- ctor `manyTill` atom "pass"
    pure $ Enum name ctors

parser :: Parser Macro
parser = parserEnum
