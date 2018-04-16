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

import PPrinter

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

indent :: [T.Text] -> [T.Text]
indent = map ("    " <>)

toLines :: MacroPython -> [T.Text]
toLines (Enum n ctors)
    = concatMap fmtCtor (zip [0..] ctors)
      ++ fmtEnum n ctors
      ++ fmtCodec n ctors
  where
    fmtType (Type head []) = head
    fmtType (Type head args) = head ++ "[" ++ intercalate ", " (map fmtType args) ++ "]"

    fmtField (Field n ty) = T.pack n <> " : " <> T.pack (fmtType ty)

    fmtCtor :: (Int, Ctor) -> [T.Text]
    fmtCtor (tag, Ctor name fields) =
        ("class " <> T.pack name <> "(NamedTuple):")
        : indent (
            map fmtField fields
            ++ [ "tag : int = " <> T.pack (show tag)
               , ""
               ]
          )

    fmtEnum n ctors =
        (T.pack n <> " = Union[")
        : indent [
                T.pack cn <> ","
                | Ctor cn _fields <- ctors
            ]
        ++ ["]", ""]

    fmtCtorC :: (Int, Ctor) -> T.Text
    fmtCtorC (tag, Ctor n fields)
        = T.pack n
            <> ": ("
            <> T.unwords [
                typeCodec ty <> ","
                | Field n ty <- fields]
            <> "),"

    fmtCodec n ctors =
        (T.pack n <> "C = enumC('" <> T.pack n <> "', {")
        : indent (
            map fmtCtorC $ zip [0..] ctors
          )
        ++ ["})"]

typeCodec :: Type -> T.Text
typeCodec (Type f []) = codecName f
typeCodec (Type f xs) = T.concat
    [ codecName f
    , "("
    , T.intercalate ", " (map typeCodec xs)
    , ")"
    ]

codecName :: String -> T.Text
codecName "List" = "listC"
codecName n = T.pack n <> "C"

macroPython :: Macro MacroPython
macroPython = Macro
    { macroParser   = parser
    , macroPPrinter = T.concat . toLines
    }
