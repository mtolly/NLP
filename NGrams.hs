module NGrams where

-- Requires packages: base, multiset, random
-- The function printEntropies prints the cross entropy and perplexity for each
-- of the five ngram models (1gram, 2gram, 3gram, interpol. with 1:1:1, and
-- interpol. with 2:2:1).

import Control.Applicative ((<$>))
import qualified Data.MultiSet as MS
import Data.List

type Sentence = [Token]
data Token = Word String | End
  deriving (Eq, Ord, Show, Read)
type Probability = Double
type Bag = MS.MultiSet

readSentences :: FilePath -> IO [Sentence]
readSentences fp = map getTokens . lines <$> readFile fp
  where getTokens ws = map Word (words ws) ++ [End]

getGrams :: FilePath -> IO (Bag Unigram, Bag Bigram, Bag Trigram)
getGrams fp = do
  sents <- readSentences fp
  return ( MS.fromList $ concatMap getUnigrams sents
         , MS.fromList $ concatMap getBigrams sents
         , MS.fromList $ concatMap getTrigrams sents )

-- Bigram and trigram definitions

data Unigram
  -- | The start of a sentence.
  = Unigram0
  -- | Any token, including the end of a sentence.
  | Unigram1 Token
  deriving (Eq, Ord, Show, Read)

getUnigrams :: Sentence -> [Unigram]
getUnigrams toks = Unigram0 : map Unigram1 toks

data Bigram
  -- | The start of a sentence.
  = Bigram0
  -- | One token that starts a sentence.
  | Bigram1 Token
  -- | Any two consecutive tokens.
  | Bigram2 Token Token
  deriving (Eq, Ord, Show, Read)

getBigrams :: Sentence -> [Bigram]
getBigrams [] = [Bigram0]
getBigrams xs@(x:_) = Bigram0 : Bigram1 x : get2 xs where
  get2 (x:xs@(y:_)) = Bigram2 x y : get2 xs
  get2 _ = []

data Trigram
  -- | The start of a sentence.
  = Trigram0
  -- | One token that starts a sentence.
  | Trigram1 Token
  -- | Two tokens that start a sentence.
  | Trigram2 Token Token
  -- | Any three consecutive tokens.
  | Trigram3 Token Token Token
  deriving (Eq, Ord, Show, Read)

getTrigrams :: Sentence -> [Trigram]
getTrigrams [] = [Trigram0]
getTrigrams xs@(x:_) = Trigram0 : Trigram1 x : get2 xs where
  get2 xs@(x:y:_) = Trigram2 x y : get3 xs
  get2 _ = []
  get3 (x:xs@(y:z:_)) = Trigram3 x y z : get3 xs
  get3 _ = []

-- Probabilities

toDbl :: Int -> Double
toDbl = fromIntegral

unigramProb :: Bag Unigram -> Unigram -> Probability
unigramProb bag1 gram1 = (countN + d) / (countN1 + sizeV * d) where
  countN = toDbl $ MS.occur gram1 bag1
  d = 0.00001
  sizeV = 9197
  countN1 = toDbl $ MS.size bag1

-- | Returns the probability of a bigram, given the sets of unigrams and bigrams.
bigramProb :: Bag Unigram -> Bag Bigram -> Bigram -> Probability
bigramProb bag1 bag2 gram2 = (countN + d) / (countN1 + sizeV * d) where
  countN = toDbl $ MS.occur gram2 bag2
  d = 0.00001
  sizeV = 9197
  countN1 = toDbl $ MS.occur gram1 bag1
  gram1 = case gram2 of
    Bigram0 -> Unigram0
    Bigram1 _ -> Unigram0
    Bigram2 x _ -> Unigram1 x

-- | Returns the probability of a trigram, given the sets of bigrams and trigrams.
trigramProb :: Bag Bigram -> Bag Trigram -> Trigram -> Probability
trigramProb bag2 bag3 gram3 = (countN + d) / (countN1 + sizeV * d) where
  countN = toDbl $ MS.occur gram3 bag3
  d = 0.00001
  sizeV = 9197
  countN1 = toDbl $ MS.occur gram2 bag2
  gram2 = case gram3 of
    Trigram0 -> Bigram0
    Trigram1 _ -> Bigram0
    Trigram2 x _ -> Bigram1 x
    Trigram3 x y _ -> Bigram2 x y

interpolateProb :: Bag Unigram -> Bag Bigram -> Bag Trigram ->
  Double -> Double -> Double -> Trigram -> Probability
interpolateProb bag1 bag2 bag3 l1 l2 l3 gram3 = l1*pUni + l2*pBi + l3*pTri where
  pUni = unigramProb bag1 gram1
  pBi = bigramProb bag1 bag2 gram2
  pTri = trigramProb bag2 bag3 gram3
  gram1 = case gram2 of
    Bigram0 -> Unigram0
    Bigram1 x -> Unigram1 x
    Bigram2 _ x -> Unigram1 x
  gram2 = case gram3 of
    Trigram0 -> Bigram0
    Trigram1 x -> Bigram1 x
    Trigram2 x y -> Bigram2 x y
    Trigram3 _ x y -> Bigram2 x y

-- Cross entropy and perplexity

-- | Cross entropy, given a probability function and the list of test sentences.
crossEntropy :: (Ord a) => (a -> Probability) -> Bag a -> Double
crossEntropy p bag = negate $
  (1 / bagSize) * (sum $ MS.toList $ MS.map (logb2 . p) bag)
    where logb2 n = log n / log 2
          bagSize = fromIntegral $ MS.size bag

-- | Given cross entropy, computes perplexity.
perplexity :: Double -> Double
perplexity ce = 2 ** ce

-- | Prints all the cross-entropy and perplexity values.
printEntropies :: IO ()
printEntropies = do
  (train1, train2, train3) <- getGrams "orwell-train.txt"
  (test1, test2, test3) <- getGrams "orwell-test.txt"
  let unigramCE = crossEntropy (unigramProb train1) test1
      bigramCE = crossEntropy (bigramProb train1 train2) test2
      trigramCE = crossEntropy (trigramProb train2 train3) test3
      interpolate1CE = crossEntropy (interpolateProb train1 train2 train3 (1/3) (1/3) (1/3)) test3
      interpolate2CE = crossEntropy (interpolateProb train1 train2 train3 (2/5) (2/5) (1/5)) test3
  putStrLn $ "Unigram cross entropy: " ++ show unigramCE
  putStrLn $ "Unigram perplexity: " ++ show (perplexity unigramCE)
  putStrLn $ "Bigram cross entropy: " ++ show bigramCE
  putStrLn $ "Bigram perplexity: " ++ show (perplexity bigramCE)
  putStrLn $ "Trigram cross entropy: " ++ show trigramCE
  putStrLn $ "Trigram perplexity: " ++ show (perplexity trigramCE)
  putStrLn $ "Interpolated (1:1:1) cross entropy: " ++ show interpolate1CE
  putStrLn $ "Interpolated (1:1:1) perplexity: " ++ show (perplexity interpolate1CE)
  putStrLn $ "Interpolated (2:2:1) cross entropy: " ++ show interpolate2CE
  putStrLn $ "Interpolated (2:2:1) perplexity: " ++ show (perplexity interpolate2CE)
