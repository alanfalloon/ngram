module Main where

import WordSplit
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

dot = B.pack "."

sentenceEnds :: S.Set B.ByteString
sentenceEnds = S.fromList [ dot, B.pack "!", B.pack "?", B.pack ";", B.pack ":" ]

isEnd :: B.ByteString -> Bool
isEnd b = b `S.member` sentenceEnds

sentences :: [B.ByteString] -> [[B.ByteString]]
sentences [] = []
sentences w = words : sentences rest'
    where
      (words, rest) = break isEnd w
      rest' = dropWhile isEnd rest

main = do
  contents <- B.getContents
  mapM_ (print . map B.unpack) (sentences $ wordSplit contents)
