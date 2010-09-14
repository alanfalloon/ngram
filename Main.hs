module Main where

import WordSplit
import ERandom
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import System.Random

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

-- | extract the triples from the list
triples :: [a] -> [(a,a,a)]
triples (h1:r@(h2:h3:_)) = (h1,h2,h3) : triples r
triples _ = []

type Trigrams = M.Map B.ByteString (M.Map B.ByteString (M.Map B.ByteString Int))
type Triple = (B.ByteString,B.ByteString,B.ByteString)
insert :: Triple -> Trigrams -> Trigrams
insert (w1,w2,w3) t = fromJust $ addW w1 (addW w2 (addW w3 incC)) $ Just t
    where
      addW w f v = Just $ M.alter f w $ fromMaybe M.empty v
      incC v = Just $ 1 + fromMaybe 0 v

getChoicesMap m0 w1 w2 = fromMaybe M.empty $ do
                           m1 <- M.lookup w1 m0
                           M.lookup w2 m1
getChoices m0 w1 w2 = uncurry (flip zip) $ unzip $ M.toList $ getChoicesMap m0 w1 w2

main = do
  contents <- B.getContents
  let t :: [[Triple]]
      t = map (triples . normalizeSentence) $ sentences $ wordSplit contents
      trigrams = foldl (flip insert) M.empty $ concat t
  w <- runERandomIO (randomSentence trigrams)
  print $ unwords $ map B.unpack w

randomSentence :: RandomGen g => Trigrams -> ERandomM g [B.ByteString]
randomSentence trigrams = randS dot dot
    where
      randS w1 w2 = do
        e <- entropyM
        if e > 128 && hasDot then return [dot] else moreWords
        where
          choices' = getChoices trigrams w1 w2
          choices = if null choices' then [(1,dot)] else choices'
          choicesMap = getChoicesMap trigrams w1 w2
          hasDot = dot `M.member` choicesMap
          moreWords = do
                      c <- eRandomEltM choices
                      rest <- randS w2 c
                      return (c:rest)
                      
  
