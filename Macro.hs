module Macro
    ( Macro(..)
    ) where

import Parser

class Macro m where
    parseMacro :: Parser m

