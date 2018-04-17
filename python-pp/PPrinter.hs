-- This module is similar to Text.PrettyPrint.
-- It has a slightly different indentation behaviour
-- and it also supports code comments with (<?).
module PPrinter
    ( Doc
    , int, text
    , comma, colon
    , lparen, rparen, lbracket, rbracket, lbrace, rbrace
    , (<+>), ($+$), ($$)
    , (<?)
    , nest
    , parens, brackets
    , quoteSingle, quoteDouble
    , render
    , vcat, vsep, hsep
    , punctuate
    , size, width
    , blankLine
    )
    where

import Data.Text      (Text)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Semigroup (Semigroup(..))
import Data.Monoid    (Monoid(..))

type Line = (Text, Text)  -- text, comment
newtype Doc = Doc [Line]
instance Show Doc where
    show = T.unpack . render "# "

infixr 6 <+>
infixr 5 $$, $+$
infixl 1 <?

int :: Int -> Doc
int i = text . T.pack $ show i

text :: Text -> Doc
text s = Doc [(s, "")]

comma, colon :: Doc
comma    = text ","
colon    = text ":"

lparen, rparen, lbracket, rbracket, lbrace, rbrace :: Doc
lparen   = text "("
rparen   = text ")"
lbracket = text "["
rbracket = text "]"
lbrace   = text "{"
rbrace   = text "}"

quoteSingle :: Doc -> Doc
quoteSingle x = text "'" <> x <> text "'"

quoteDouble :: Doc -> Doc
quoteDouble x = text "\"" <> x <> text "\""

instance Semigroup Doc where
    Doc xs <> Doc ys = Doc $ meld "" xs ys

instance Monoid Doc where
    mempty  = Doc []
    mappend = (<>)

(<+>) :: Doc -> Doc -> Doc
Doc xs <+> Doc ys = Doc $ meld " " xs ys

($+$) :: Doc -> Doc -> Doc
Doc xs $+$ Doc ys = Doc $ xs <> ys

($$) :: Doc -> Doc -> Doc
($$) = ($+$)

-- | Add a comment to the first line of the Doc.
(<?) :: Doc -> Text -> Doc
Doc [] <? comment = Doc [("", comment)]
Doc ((t,c) : ls) <? comment = Doc $ (t, merge comment c) : ls
  where
    merge "" y  = y
    merge x  "" = x
    merge x  y  = x <> " (" <> y <> ")"

meld :: Text -> [Line] -> [Line] -> [Line]
meld _sep [] ys = ys
meld _sep xs [] = xs
meld sep [(x,xc)] ((y,yc) : ys) = (x <> sep <> y, merge xc yc) : ys
  where
    merge ""  y' = y'
    merge x'  "" = x'
    merge x'  y' = x' <> ", " <> y'
meld sep (x : xs) ys = x : meld sep xs ys

nest :: Int -> Doc -> Doc
nest n (Doc xs) = Doc [(T.replicate n " " <> t, c) | (t, c) <- xs]

parens :: Doc -> Doc
parens d = lparen <> d <> rparen

brackets :: Doc -> Doc
brackets d = lbracket <> d <> rbracket

render :: Text -> Doc -> Text
render cmtStr (Doc xs) = T.unlines $ map (renderLine cmtStr) xs

renderLine :: Text -> (Text, Text) -> Text
renderLine _cmtStr ("", "") = ""
renderLine  cmtStr ("", comment) = cmtStr <> " " <> comment
renderLine _cmtStr (content, "") = content
renderLine  cmtStr (content, comment) = content <> "  " <> cmtStr <> " " <> comment

vcat :: [Doc] -> Doc
vcat = foldr ($+$) mempty

hsep :: [Doc] -> Doc
hsep = foldr (<+>) mempty

vsep :: [Doc] -> Doc
vsep = vcat . L.intersperse blankLine

blankLine :: Doc
blankLine = text ""

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []  = []
punctuate _ [x] = [x]
punctuate sep (x : xs) = (x <> sep) : punctuate sep xs

size :: Doc -> Int
size (Doc xs) = sum [T.length t | (t, _cmt) <- xs]

width :: Doc -> Int
width (Doc xs) = maximum [T.length t | (t, _cmt) <- xs]
