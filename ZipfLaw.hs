module ZipfLaw where

-- Requires packages: base, gnuplot, multiset
-- The functions pData, pDataZoom, and pDataLog plot training data.
-- The functions pZipf, pZipfZoom, and pZipfLog plot the Zipf power law.

import Control.Applicative ((<$>))
import Graphics.Gnuplot.Simple
import qualified Data.MultiSet as MSet
import Data.List (sort)
import Control.Arrow ((***))

readWords :: FilePath -> IO [String]
readWords fp = words <$> readFile fp

type Frequency = Double
type Rank = Int

getRanks :: [String] -> [(Rank, Frequency)]
getRanks ws = zip [1..] $ map (\c -> fromIntegral c / total) counts where
  counts = reverse $ sort $ map snd $ MSet.toOccurList $ MSet.fromList ws
  total = fromIntegral $ sum counts

orwellPoints :: IO [(Rank, Frequency)]
orwellPoints = getRanks <$> readWords "orwell-train.txt"

-- just a convenience for type inference
toDouble :: Int -> Double
toDouble = fromIntegral

-- | The data points for the real Zipf power law.
zipfPoints :: [(Rank, Frequency)]
zipfPoints = do
  rank <- [1..8459]
  let freq = 1 / (toDouble rank * normValue)
  return (rank, freq)
normValue = sum [ 1 / toDouble n | n <- [1..8459] ]

-- | All the plots for Question 1.
pData, pZipf, pDataZoom, pZipfZoom, pDataLog, pZipfLog :: IO ()
-- Plot all the points
pData = orwellPoints >>= plotList []
pZipf = plotList [] zipfPoints
-- Drop the first 499 pairs (ranks 1 to 499) and plot the rest
pDataZoom = orwellPoints >>= plotList [] . drop 499
pZipfZoom = plotList [] $ drop 499 zipfPoints
-- Take the logs of all ranks and frequencies, and plot those
pDataLog = orwellPoints >>= plotList [] . map ((log . toDouble) *** log)
pZipfLog = plotList [] $ map ((log . toDouble) *** log) zipfPoints
