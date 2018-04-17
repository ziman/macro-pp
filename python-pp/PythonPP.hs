module PythonPP
    ( MacroPython(..)
    , macroPython
    ) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.List
import Data.Monoid
import qualified Data.Text as T

import Text.MacroPP.Macro
import Text.MacroPP.Parser as Parser

import PPrinter as PP

data Type = Type String [Type] deriving Show  -- name, args

data Field = Field
    { fieldName :: String
    , fieldType :: Type
    }
    deriving Show

data Ctor = Ctor
    { ctorName   :: String
    , ctorFields :: [Field]
    }
    deriving Show

data MacroPython
    = Enum String [Ctor]
    deriving Show

ftype :: Parser Type
ftype = do
    head <- ident
    args <- option [] $ sqbrackets (ftype `sepBy` kwd ",")
    pure $ Type head args

field :: Parser Field
field = do
    name <- ident
    kwd "="
    ty <- ftype
    pure $ Field name ty

ctor :: Parser Ctor
ctor = do
    name <- ident
    fields <- option [] $ Parser.parens (field `sepBy` kwd ",")
    pure $ Ctor name fields

parserEnum :: Parser MacroPython
parserEnum = do
    kwd "@enum(deriving=Codec)"
    kwd "class"
    name <- ident
    kwd ":"
    ctors <- ctor `manyTill` atom "pass"
    pure $ Enum name ctors

parser :: Parser MacroPython
parser = parserEnum

indent :: Doc -> Doc
indent = nest 4

fmt :: MacroPython -> Doc
fmt (Enum n ctors)
    = vcat (map fmtCtor $ zip [0..] ctors)
      $+$ fmtEnum n ctors
      $+$ fmtCodec n ctors
  where
    fmtType :: Type -> Doc
    fmtType (Type head [])
        = text (T.pack head)
    fmtType (Type head args)
        = text (T.pack head)
            <> brackets (hsep . punctuate comma $ map fmtType args)

    fmtField :: Field -> Doc
    fmtField (Field n ty) = text (T.pack n <> " : ") <> fmtType ty

    fmtCtor :: (Int, Ctor) -> Doc
    fmtCtor (tag, Ctor name fields) =
        text "class " <> text (T.pack name) <> text "(NamedTuple):"
        $+$ indent (
            vcat (map fmtField fields)
            $+$ text "tag : int = " <> int tag
          )
        $+$ blankLine

    fmtEnum :: String -> [Ctor] -> Doc
    fmtEnum n ctors =
        text (T.pack n) <+> text "=" <+> text "Union" <> brackets (
            blankLine
            $+$ indent (vcat [
                    text (T.pack cn) <> text ","
                    | Ctor cn _fields <- ctors
                ])
        )

    fmtCtorC :: (Int, Ctor) -> Doc
    fmtCtorC (tag, Ctor n fields)
        = text (T.pack n)
        <+> colon
        <+> PP.parens (hsep [
                typeCodec ty <> comma
                | Field _n ty <- fields
        ])
        <> comma

    fmtCodec :: String -> [Ctor] -> Doc
    fmtCodec n ctors =
        text (T.pack n) <> text "C"
        <+> text "="
        <+> text "enumC"
        <>  PP.parens (quoteSingle (text $ T.pack n)
            <> comma <+> lbrace
            $+$ indent (
                    vcat (map fmtCtorC $ zip [0..] ctors)
                )
            $+$ rbrace
        )

typeCodec :: Type -> Doc
typeCodec (Type f [])
    = text $ codecName f
typeCodec (Type f xs)
    = (text $ codecName f)
    <> PP.parens (hsep . punctuate comma $ map typeCodec xs)

codecName :: String -> T.Text
codecName "List" = "listC"
codecName n = T.pack n <> "C"

macroPython :: Macro MacroPython
macroPython = Macro
    { macroParser   = parser
    , macroPPrinter = render "# " . fmt
    }
