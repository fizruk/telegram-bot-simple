{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Telegram.Bot.API.Internal.Utils where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), GToJSON, GFromJSON, genericToJSON, genericParseJSON, Zero)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Options(..), defaultOptions, Parser)
import Data.Char (isUpper, toUpper, toLower)
import Data.List (intercalate)
import GHC.Generics
import Language.Haskell.TH

deriveJSON' :: Name -> Q [Dec]
deriveJSON' name = deriveJSON (jsonOptions (nameBase name)) name

gtoJSON :: forall a d f. (Generic a, GToJSON Zero (Rep a), Rep a ~ D1 d f, Datatype d)
  => a -> Value
gtoJSON = genericToJSON (jsonOptions (datatypeName (Proxy3 :: Proxy3 d f a)))

gparseJSON :: forall a d f. (Generic a, GFromJSON Zero (Rep a), Rep a ~ D1 d f, Datatype d)
  => Value -> Parser a
gparseJSON = genericParseJSON (jsonOptions (datatypeName (Proxy3 :: Proxy3 d f a)))


genericSomeToJSON :: (Generic a, GSomeJSON (Rep a)) => a -> Value
genericSomeToJSON = gsomeToJSON . from

genericSomeParseJSON :: (Generic a, GSomeJSON (Rep a)) => Value -> Parser a
genericSomeParseJSON = fmap to . gsomeParseJSON

data Proxy3 d f a = Proxy3

jsonOptions :: String -> Options
jsonOptions tname = defaultOptions
  { fieldLabelModifier     = snakeFieldModifier tname
  , constructorTagModifier = snakeFieldModifier tname
  , omitNothingFields      = True
  }

snakeFieldModifier :: String -> String -> String
snakeFieldModifier xs ys = wordsToSnake (stripCommonPrefixWords xs ys)

camelWords :: String -> [String]
camelWords "" = []
camelWords s
  = case us of
      (_:_:_) -> us : camelWords restLs
      _       -> (us ++ ls) : camelWords rest
  where
   (us, restLs) = span  isUpper s
   (ls, rest)   = break isUpper restLs

stripCommonPrefix :: Eq a => [a] -> [a] -> [a]
stripCommonPrefix (x:xs) (y:ys) | x == y = stripCommonPrefix xs ys
stripCommonPrefix _ ys = ys

wordsToCamel :: [String] -> String
wordsToCamel [] = ""
wordsToCamel (w:ws) = map toLower w ++ concatMap capitalise ws

wordsToSnake :: [String] -> String
wordsToSnake = intercalate "_" . map (map toLower)

capitalise :: String -> String
capitalise (c:s) = toUpper c : s
capitalise "" = ""

stripCommonPrefixWords :: String -> String -> [String]
stripCommonPrefixWords xs ys =
  stripCommonPrefix (camelWords xs) (camelWords (capitalise ys))


class GSomeJSON f where
  gsomeToJSON :: f p -> Value
  gsomeParseJSON :: Value -> Parser (f p)

instance GSomeJSON f => GSomeJSON (D1 d f) where
  gsomeToJSON (M1 x) = gsomeToJSON x
  gsomeParseJSON js = M1 <$> gsomeParseJSON js

instance (ToJSON a, FromJSON a) => GSomeJSON (C1 c (S1 s (K1 i a))) where
  gsomeToJSON (M1 (M1 (K1 x))) = toJSON x
  gsomeParseJSON js = (M1 . M1 . K1) <$> parseJSON js

instance (GSomeJSON f, GSomeJSON g) => GSomeJSON (f :+: g) where
  gsomeToJSON (L1 x) = gsomeToJSON x
  gsomeToJSON (R1 y) = gsomeToJSON y

  gsomeParseJSON js
      = L1 <$> gsomeParseJSON js
    <|> R1 <$> gsomeParseJSON js
