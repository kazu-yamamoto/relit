{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Main where

import RegexLiteral
import Text.Regex.Posix

target1 :: String
target1 = "\\foo\\bar.c"

regexp1 :: Regex
regexp1 = makeRegex ("\\\\(foo)\\\\(bar\\.c)" :: String)

regexp1' :: Regex
regexp1' = [$re|\\(foo)\\(bar\.c)|]

target2 :: String
target2 = "(1+2)/(3+4)"

regexp2 :: String
regexp2 = "\\([^)]+\\)"

regexp2' :: String
regexp2' = [$re|\([^)]+\)|]

main :: IO ()
main = do
  print $ regexp1  `matchAllText` target1
  print $ regexp1' `matchAllText` target1
  print $ (target2 =~ regexp2  :: (Int,Int))
  print $ (target2 =~ regexp2' :: (Int,Int))
