module PPrinter (fmt) where

import Data.List
import Data.Monoid
import qualified Data.Text as T

import Util.PPrinter
import Macro

indent :: Doc -> Doc
indent = nest 4

fmt :: Macro -> Doc
fmt (Enum n ctors)
    = vcat (map fmtCtor $ zip [0..] ctors)
      $+$ fmtEnum n ctors
      $+$ blankLine
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
            $+$ blankLine
        )

    fmtCtorC :: (Int, Ctor) -> Doc
    fmtCtorC (tag, Ctor n fields)
        = text (T.pack n)
        <+> colon
        <+> parens (hsep [
                typeCodec ty <> comma
                | Field _n ty <- fields
        ])
        <> comma

    fmtCodec :: String -> [Ctor] -> Doc
    fmtCodec n ctors =
        text (T.pack n) <> text "C"
        <+> text "="
        <+> text "enumC"
        <>  parens (quoteSingle (text $ T.pack n)
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
    <> parens (hsep . punctuate comma $ map typeCodec xs)

codecName :: String -> T.Text
codecName "List" = "listC"
codecName n = T.pack n <> "C"

