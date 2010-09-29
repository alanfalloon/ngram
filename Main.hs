module Main where

import WordSplit
import ERandom
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import System.Random

n :: Int
n = 3

dot = B.pack "."
comma = B.pack ","

startState = replicate (n-1) dot

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
-- extraction. The sentence will start with n-1 dots and end with a
-- single dot. This ensures that the start and end conditions for the
-- sentences are known.
normalizeSentence :: [B.ByteString] -> [B.ByteString]
normalizeSentence s = startState ++ s ++ [dot]

-- | extract the n-grams from the list
ngrams :: [a] -> [[a]]
ngrams x | length x >= n = take n x : ngrams (tail x)
         | otherwise     = []

type Ngrams = M.Map Ngram Freq
type Freq = M.Map B.ByteString Int
type Ngram = [B.ByteString]
insert :: Ngram -> Ngrams -> Ngrams
insert ng t = M.alter add k t
    where
      k = take (n-1) ng
      [v] = drop (n-1) ng
      add :: Maybe Freq -> Maybe Freq
      add (Just m) = Just (M.insertWith (+) v 1 m)
      add Nothing  = Just (M.singleton v 1)

getChoicesMap m0 state = fromMaybe M.empty $ M.lookup state m0
getChoices m0 state = uncurry (flip zip) $ unzip $ M.toList $ getChoicesMap m0 state

main = do
  contents <- B.getContents
  let t :: [[Ngram]]
      t = map (ngrams . normalizeSentence) $ sentences $ wordSplit contents
      trigrams = foldl (flip insert) M.empty $ concat t
  w <- runERandomIO (randomSentence trigrams)
  print $ unwords $ map B.unpack w

randomSentence :: RandomGen g => Ngrams -> ERandomM g [B.ByteString]
randomSentence ngrams = randS startState
    where
      randS state = do
        e <- entropyM
        if e > 128 && hasDot then return [dot] else moreWords
        where
          choices' = getChoices ngrams state
          choices = if null choices' then [(1,dot)] else choices'
          choicesMap = getChoicesMap ngrams state
          hasDot = dot `M.member` choicesMap
          moreWords = do
                      c <- eRandomEltM choices
                      rest <- randS (tail state ++ [c])
                      return (c:rest)
                      
  
