{-# LANGUAGE NoOverloadedStrings #-}
-- | Startup benchmark: generate a 10,000-transaction journal (3 currencies,
-- monthly P directives, 5 years), then time loading, resolving settings and
-- rendering the Month view at 140 columns. Exits 1 if it takes >= 1.0 s.
module Main (main) where

import Cal.Cli (mkSettings, parseCli)
import Cal.Journal (loadJournal)
import Cal.Model.Common (mkEnv)
import Cal.Print (renderPrint)
import Cal.Types (Terminal (..))
import Control.Exception (finally)
import Control.Monad (when)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Hledger.Cli.CliOptions (CliOpts (..))
import Hledger.Data.Types (jtxns)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)
import System.IO (hClose, hPutStr, openTempFile)
import Text.Printf (printf)

threshold :: Double
threshold = 1.0

-- | Five years of monthly price directives, then 5,000 income/expense pairs
-- (10,000 transactions) in USD, EUR and NGN.
journalText :: String
journalText = unlines (header ++ prices ++ concatMap pair [0 .. 4999 :: Int])
  where
    header = ["D 1,000.00 USD", ""]
    months :: [(Int, Int)]
    months = [(y, m) | y <- [2022 .. 2026], m <- [1 .. 12], (y, m) <= (2026, 10)]
    prices =
      concat
        [ [ printf "P %04d-%02d-01 EUR %.4f USD" y m (1.05 + fromIntegral (i `mod` 7) * 0.01 :: Double)
          , printf "P %04d-%02d-01 NGN %.5f USD" y m (0.0015 - fromIntegral (i `mod` 5) * 0.00001 :: Double)
          ]
        | (i, (y, m)) <- zip [0 :: Int ..] months
        ]
        ++ [""]
    ndays = length months * 28
    ccy i = ["USD", "EUR", "NGN"] !! (i `mod` 3)
    dateOf i =
      let k = (i * 7) `mod` ndays
          (y, m) = months !! (k `div` 28)
       in printf "%04d-%02d-%02d" y m (k `mod` 28 + 1 :: Int) :: String
    pair i =
      [ printf "%s Employer %d" (dateOf i) i
      , printf "    assets:bank:%s    %d %s" (ccy i) (100 + i `mod` 900) (ccy i)
      , printf "    income:salary" :: String
      , ""
      , printf "%s Shop %d" (dateOf (i + 3)) i
      , printf "    expenses:food    %d %s" (10 + i `mod` 90) (ccy (i + 1))
      , printf "    assets:bank:%s" (ccy (i + 1)) :: String
      , ""
      ]

main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp "hledger-cal-bench.journal"
  hPutStr h journalText >> hClose h
  secs <- run path `finally` removeFile path
  printf "startup: %.3f s (limit %.1f s)\n" secs threshold
  when (secs >= threshold) exitFailure
  where
    run path = do
      opts <- parseCli ["-f", path, "-M"]
      t0 <- getCurrentTime
      loaded <- loadJournal (inputopts_ opts) (file_ opts)
      j <- either fail pure loaded
      let term = Terminal False True (Just 140) False True
      s <- either fail pure (mkSettings (fromGregorian 2026 11 6) term opts j)
      out <- either (fail . T.unpack) pure (renderPrint (mkEnv j s) 140)
      T.length out `seq` pure ()
      t1 <- getCurrentTime
      printf "rendered %d chars, %d txns\n" (T.length out) (length (jtxns j))
      pure (realToFrac (diffUTCTime t1 t0) :: Double)
