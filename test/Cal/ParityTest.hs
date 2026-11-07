{-# LANGUAGE OverloadedStrings #-}
module Cal.ParityTest where

import Cal.Journal (loadJournal)
import Cal.Model.Common (Money)
import Cal.Model.Shared
import Cal.Period (subSpans)
import Cal.Types
import Cal.Valuation (baseQty)
import Data.Text (Text)
import Hledger hiding (Interval (..), Status)
import Hledger.Cli (argsToCliOpts)
import Hledger.Cli.CliOptions (CliOpts (..))
import Support
import Data.Time.Calendar (fromGregorian)

-- | hledger's per-month column totals (Jan–Nov) for a balance query.
hledgerColumns :: String -> [String] -> IO [MixedAmount]
hledgerColumns q v = do
  opts <- argsToCliOpts
    ( ["bal", q, "-M", "-b", "2026-01-01", "-e", "2026-12-01", "-f", designPath
      , "--infer-market-prices", "--today", "2026-11-06"] ++ v ) []
  r <- loadJournal (inputopts_ opts) (file_ opts)
  j <- either (assertFailure . ("journal failed to load: " ++)) pure r
  pure (prrAmounts (prTotals (multiBalanceReport (reportspec_ opts) j)))

-- | The model's Jan–Nov month rows for the same year.
modelRows :: [String] -> IO [MonthRow]
modelRows v' = do
  e <- envFor (["-Y", "-p", "2026"] ++ v')
  let year = Span (fromGregorian 2026 1 1) (fromGregorian 2027 1 1)
  pure (take 11 (fst (monthRows e (subSpans IMonth year))))

parity :: String -> [String] -> [String] -> Text -> (MonthRow -> Money) -> (Quantity -> Quantity) -> IO ()
parity q v v' base field adjust = do
  cols <- hledgerColumns q v
  rows <- modelRows v'
  length cols @?= 11
  map (adjust . baseQty base) cols @?= map (baseQty base . field) rows

unit_expensesNaira :: IO ()
unit_expensesNaira = parity "type:X" ["--value=end,₦"] [] "₦" mrExpenses id

unit_incomeNaira :: IO ()
unit_incomeNaira = parity "type:R" ["--value=end,₦"] [] "₦" mrIncome negate

unit_expensesDollar :: IO ()
unit_expensesDollar = parity "type:X" ["--value=end,$"] ["-X", "$"] "$" mrExpenses id
