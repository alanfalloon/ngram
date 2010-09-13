module Main where

import WordSplit
import Data.Set
import qualified Data.ByteString.Lazy.Char8 as B

sentenceEnds :: Set B.ByteString
sentenceEnds = fromList [ B.pack ".", B.pack "!", B.pack "?", B.pack ";", B.pack ":" ]

main = do
  contents <- B.getContents
  print (sentences contents)
