module SentenceLength where

-- Requires packages: base, multiset, gnuplot, gamma
-- The functions pUnigrams, pMultinomial, pNegBinomial, and pTrainCounts
-- generate plots. The function printEntropies prints cross-entropy and
-- perplexity values for the three sentence length models.

import Control.Applicative ((<$>))
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Graphics.Gnuplot.Simple
import Math.Gamma (gamma)

type Sentence = [Token]
-- | A Token is either a normal word, or the end token.
data Token = Word String | End
  deriving (Eq, Ord, Show, Read)

-- | The length of a sentence.
type Length = Int
-- | A probability should be in the range [0, 1).
type Probability = Double

-- | Reads all the sentences, as lists of tokens, from a file.
readSentences :: FilePath -> IO [Sentence]
readSentences fp = map getTokens . lines <$> readFile fp
  where getTokens ws = map Word (words ws) ++ [End]

-- Unigrams

-- | All the data needed to make unigram calculations.
data Unigrams = Unigrams
  { uniCounts :: MS.MultiSet Token
  , uniSmoothing :: Double
  , uniVocabSize :: Int }
  deriving (Eq, Ord, Show, Read)

-- | Given a token and a unigrams database, return a [0,1] probability.
unigramProb :: Unigrams -> Token -> Probability
unigramProb uni tok = (c1 + d) / (c0 + v * d) where
  c1 = fromIntegral $ MS.occur tok $ uniCounts uni
  d = uniSmoothing uni
  c0 = fromIntegral $ MS.size $ uniCounts uni
  v = fromIntegral $ uniVocabSize uni

-- | Make the unigrams database from the Orwell training data.
makeUnigrams :: IO Unigrams
makeUnigrams = do
  toks <- concat <$> readSentences "orwell-train.txt"
  return $ Unigrams
    { uniCounts = MS.fromList toks
    , uniSmoothing = 0.00001
    , uniVocabSize = 9197 }

-- Question 3

-- | Probability of a sentence length, according to the unigram model.
probLenUnigram :: Unigrams -> Length -> Probability
probLenUnigram uni len = ((1 - endProb) ^ len) * endProb
  where endProb = unigramProb uni End

-- | Probability of a sentence length, according to the multinomial model.
probLenMultinomial :: MS.MultiSet Length -> Length -> Probability
probLenMultinomial lengths len = countL / n where
  countL = fromIntegral $ MS.occur len lengths
  n = fromIntegral $ MS.size lengths

-- | Probability of a sentence length, according to the negative binomial model.
probLenGamma :: Length -> Probability
probLenGamma l = let
  l' = fromIntegral l
  p = 0.8682
  r = 2.6878
  in (gamma (r + l') / (gamma r * gamma (l' + 1))) * (p ^ l) * ((1-p) ** r)

-- | Cross entropy, given a probability function and the list of test sentences.
crossEntropy :: (Ord a) => (a -> Probability) -> MS.MultiSet a -> Double
crossEntropy p bag = negate $
  (1 / bagSize) * (sum $ MS.toList $ MS.map (logb2 . p) bag)
    where logb2 n = log n / log 2
          bagSize = fromIntegral $ MS.size bag

-- | Given cross entropy, computes perplexity.
perplexity :: Double -> Double
perplexity ce = 2 ** ce

-- | Counts the lengths of the training sentences.
bagOfLengths :: IO (MS.MultiSet Length)
bagOfLengths = MS.fromList . map length <$> readSentences "orwell-train.txt"

-- | Plots sentence lengths 1 to 100 frequencies according to the unigram model.
pUnigrams :: IO ()
pUnigrams = do
  uni <- makeUnigrams
  plotList [] [(n, probLenUnigram uni n) | n <- [1..100]]

-- | Plots sentence length frequencies according to the multinomial model.
pMultinomial :: IO ()
pMultinomial = do
  lengths <- bagOfLengths
  plotList [] [(len, probLenMultinomial lengths len) | len <- [1..100]]

-- | Plots sentence length frequencies according to the negative binomial model.
pNegBinomial :: IO ()
pNegBinomial = plotList [] [(len, probLenGamma len) | len <- [1..100]]

-- | Plots sentence length counts from the training data.
pTrainCounts :: IO ()
pTrainCounts = do
  lengths <- map length <$> readSentences "orwell-train.txt"
  plotList [] $ MS.toOccurList $ MS.fromList lengths

-- | Prints all the cross-entropy and perplexity values.
printEntropies :: IO ()
printEntropies = do
  uni <- makeUnigrams
  bag <- bagOfLengths
  let unigramCE = crossEntropy (probLenUnigram uni) bag
      multinomialCE = crossEntropy (probLenMultinomial bag) bag
      negBinomialCE = crossEntropy probLenGamma bag
  putStrLn $ "Unigram cross entropy: " ++ show unigramCE
  putStrLn $ "Unigram perplexity: " ++ show (perplexity unigramCE)
  putStrLn $ "Multinomial cross entropy: " ++ show multinomialCE
  putStrLn $ "Multinomial perplexity: " ++ show (perplexity multinomialCE)
  putStrLn $ "Neg. binomial cross entropy: " ++ show negBinomialCE
  putStrLn $ "Neg. binomial perplexity: " ++ show (perplexity negBinomialCE)
