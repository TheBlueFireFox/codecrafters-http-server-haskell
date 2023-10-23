module Config (Config(..), argParse) where

newtype Config = Config
    { directory :: Maybe String
    }
    deriving (Show)

argParse :: [String] -> Config
argParse args = Config{directory = parseDirectory args}

parseDirectory :: [String] -> Maybe String
parseDirectory [] = Nothing
parseDirectory ("--directory" : dir : _) = Just dir
parseDirectory (_ : n) = parseDirectory n

