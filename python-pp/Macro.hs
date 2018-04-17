module Macro where

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

data Macro
    = Enum String [Ctor]
    deriving Show
