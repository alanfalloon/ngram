module Main where

import WordSplit
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

dot = B.pack "."
comma = B.pack ","

sentenceEnds :: S.Set B.ByteString
sentenceEnds = S.fromList [ dot
                          , B.pack "!"
                          , B.pack "?"
                          , B.pack ":"
                          ]

clauseEnds :: S.Set B.ByteString
clauseEnds = S.fromList [ comma
                        , B.pack ";"
                        , B.pack "("
                        , B.pack ")"
                        ]

isEnd :: B.ByteString -> Bool
isEnd b = b `S.member` sentenceEnds

sentences :: [B.ByteString] -> [[B.ByteString]]
sentences [] = []
sentences w = words : sentences rest'
    where
      (words, rest) = break isEnd w
      rest' = dropWhile isEnd rest

-- | Convert the sentences to a normal form suitable for trigram
-- extraction. The sentence will start with 2 dots and end with a
-- single dot. This ensures that the start and end conditions for the
-- sentences are known. Clauses (such as parenthetical asides, or
-- dependant clauses) are separated using 2 commas so that their state
-- is separated.
normalizeSentence :: [B.ByteString] -> [B.ByteString]
normalizeSentence s = [dot,dot] ++ concatMap normalizeClauses s ++ [dot]
    where
      normalizeClauses w | w `S.member` clauseEnds = [comma,comma]
                         | otherwise               = [w]

main = do
  contents <- B.getContents
  mapM_ (print . map B.unpack) (map normalizeSentence $ sentences $ wordSplit contents)
