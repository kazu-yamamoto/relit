{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

{-|
  Literal notation for regular expression.

  Importing this module with the 'QuasiQuotes' and 'OverloadedStrings'
  extensions making possible to directly specify reqular expression
  literal. This means that awkward backslashes are not necessary.

  Sample code:

>    { -# LANGUAGE QuasiQuotes, OverloadedStrings #- }
>    -- Due to Haddock limitation, spaces are inserted after and before "}".
>    -- Remove them if you copy this.
>
>    import Text.Regex.Literal
>    import Text.Regex.Posix
>
>    -- Regular expression as the String literal
>    -- regexp :: Regex
>    -- regexp = makeRegex ("\\\\(foo)\\\\(bar\\.c)" :: String)
>    
>    -- Regular expression as the regular expression literal
>    regexp :: Regex
>    regexp = [$re|\\(foo)\\(bar\.c)|]
-}
module Text.Regex.Literal (re) where

import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Regex.Base

{-|
  A 'QuasiQuoter' function to implement regular expression literal.
-}
re :: QuasiQuoter
re = QuasiQuoter parseExprExp parseExprPat

parseExprExp :: String -> ExpQ
parseExprExp x = return . LitE . StringL $ x

parseExprPat :: String -> PatQ
parseExprPat = undefined

instance (RegexMaker regex compOpt execOpt String) => IsString regex where
  fromString s = make s
    where
      make :: (RegexMaker regex compOpt execOpt String) => String -> regex
      make = makeRegex
