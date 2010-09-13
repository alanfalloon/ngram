{
module WordSplit where
}

%wrapper "basic-bytestring"

$alpha = ['0-9a-zA-Z_]

tokens :-

  $white+    ;
  $alpha+    { \s -> s }
  .          { \s -> s }

{
sentences b = alexScanTokens b
}
