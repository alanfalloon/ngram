{
module WordSplit where
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "basic-bytestring"

$alpha = ['0-9a-zA-Z_\-]

tokens :-

  $white+                             ;
  $alpha+                             { \s -> B.map toLower s }
  \<[~\>]+\>                          { \s -> s }
  \"[~\"]+\"                          { \s -> s }
  .                                   { \s -> s }

{
wordSplit b = alexScanTokens b
}
