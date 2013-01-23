We need some conversion functions

> module Util where

> import qualified Data.ByteString.Char8 as BS

For example, from GHC's FastString to String

> import FastString

> fastStringToString :: FastString -> String
> --fastStringToString (FastString a b c bs d) = BS.unpack a
> fastStringToString = show
