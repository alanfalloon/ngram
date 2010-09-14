module ERandom where

import Control.Monad.State
import Data.Array.IArray
import System.Random

type Entropy = Double

data RandomGen g => ERandom g = ER {
      entropy :: Entropy,
      gen :: g
    }

type ERandomM g a = State (ERandom g) a

runERandom :: RandomGen g => g -> ERandomM g a -> (a,g)
runERandom g m = (a,g')
    where
      (a, ER { gen=g' } ) = runState m (ER 0 g)

runERandomIO :: ERandomM StdGen a -> IO a
runERandomIO m = do
  g <- getStdGen
  let (a, g') = runERandom g m
  setStdGen g'
  return a

entropyM :: RandomGen g => ERandomM g Entropy
entropyM = gets entropy

-- | Calculate the entropy of a linear distribution of N choices
bitsLinear :: Integral a => a -> Entropy
bitsLinear n = logBase 2 (fromIntegral n)

-- | Calculate the entropy of a choice with a given probability
bitsProb :: Double -> Entropy
bitsProb p = - logBase 2 p

-- | Get a random number in a range
eRandomRM :: (Ix a, Random a, RandomGen g) => (a,a) -> ERandomM g a
eRandomRM (lo,hi) = do
  ER e g <- get
  let (r,g') = randomR (lo,hi) g
      e'     = bitsLinear (rangeSize (lo,hi))
  put (ER (e+e') g')
  return r

-- | Get a random element from an occurrence list
eRandomEltM :: RandomGen g => [(Int,b)] -> ERandomM g b
eRandomEltM countsElts = do
  ER e g <- get
  let (r,g') = randomR (1,total) g
      (c,v)  = getChoice r countsElts
      e'     = bitsProb (fromIntegral c / fromIntegral total)
  put (ER (e+e') g')
  return v
    where
      (counts, _) = unzip countsElts
      total = sum counts
      getChoice n [] = error $ "no elements?"
      getChoice n (e@(c,_):rest) | n <= c    = e
                                 | otherwise = getChoice (n-c) rest
