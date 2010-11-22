{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-orphans #-}

module Text.Regex.Literal where

import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Regex.Posix

re :: QuasiQuoter
re = QuasiQuoter parseExprExp parseExprPat

parseExprExp :: String -> ExpQ
parseExprExp x = return . LitE . StringL $ x

parseExprPat :: String -> PatQ
parseExprPat = undefined

instance IsString Regex where
  fromString s = make s
    where
      make :: String -> Regex
      make = makeRegex
