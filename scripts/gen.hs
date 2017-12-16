module Main where

import Data.Char (toUpper)
import Data.List.Split (splitOn)

snakeToCamel :: String -> String
snakeToCamel = concatMap capitalize . splitOn "_"

capitalize :: String -> String
capitalize (c:s) = toUpper c : s
capitalize "" = ""

toHaskellType :: String -> String -> String -> String
toHaskellType tname ft desc =
  case take 8 desc of
    "Optional" -> "Maybe " ++ toHaskellType' tname ft
    _ -> toHaskellType' tname ft

toHaskellType' :: String -> String -> String
toHaskellType' tname ft = case words ft of
  ["Integer"] -> "Int32"
  ["String"] -> "Text"
  ["Boolean"] -> "Bool"
  ["True"] -> "Bool"
  [t] -> t
  ["Array", "of", t] -> "[" ++ t ++ "]"
  ["Array", "of", "Array", "of", t] -> "[[" ++ t ++ "]]"
  (_:"or":_) -> "Some" ++ capitalize tname
  _ -> error ("unknown type: " ++ ft)

stripOptional :: String -> String
stripOptional s =
  case take 10 s of
    "Optional. " -> drop 10 s
    _ -> s

magic :: String -> String
magic "" = ""
magic s = case splitOn "\t" s of
  [field, fieldType, description] ->
    snakeToCamel field
    ++ " :: "
    ++ toHaskellType field fieldType description
    ++ " -- ^ "
    ++ stripOptional description
  [field, fieldType, isRequired, description] ->
    snakeToCamel field
    ++ " :: "
    ++ toHaskellType field fieldType isRequired
    ++ " -- ^ "
    ++ description
  _ -> error ("invalid input: " ++ s)
  where

main :: IO ()
main = do
  ls <- lines <$> getContents
  mapM_ (putStrLn . magic) ls
