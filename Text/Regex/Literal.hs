{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, CPP #-}
{-# OPTIONS -fno-warn-orphans #-}

{-|
  Literal notation for regular expression.

  Importing this module with the 'QuasiQuotes' and 'OverloadedStrings'
  extensions making possible to directly specify reqular expression
  literal. This means that awkward backslashes are not necessary.

  You can copy a regular expression in other languages and paste it
  to your Haskell program.

  Sample code:

>    { -# LANGUAGE QuasiQuotes, OverloadedStrings, CPP #- }
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
>    -- Due to Haddock limitation, spaces are inserted between "-"s.
>    -- Remove them if you copy this.
>    #if _ _GLASGOW_HASKELL_ _ >= 700
>    regexp = [re|\\(foo)\\(bar\.c)|]
>    #else
>    regexp = [$re|\\(foo)\\(bar\.c)|]
>    #endif
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
#if __GLASGOW_HASKELL__ >= 700
re = QuasiQuoter parseExprExp parseExprPat parseExprType parseExprDec
#else
re = QuasiQuoter parseExprExp parseExprPat
#endif

parseExprExp :: String -> ExpQ
parseExprExp x = return . LitE . StringL $ x

parseExprPat :: String -> PatQ
parseExprPat = undefined

#if __GLASGOW_HASKELL__ >= 700
parseExprType :: String -> TypeQ
parseExprType = undefined

parseExprDec :: String -> Q [Dec]
parseExprDec = undefined
#endif

instance (RegexMaker regex compOpt execOpt String) => IsString regex where
  fromString s = make s
    where
      make :: (RegexMaker regex compOpt execOpt String) => String -> regex
      make = makeRegex
