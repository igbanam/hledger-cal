-- | Journal loading and posting classification on top of hledger-lib.
module Cal.Journal
  ( loadJournal
  , postingKind
  , isForecastTxn
  , postingLocation
  , journalDataRange
  , firstSeenCommodity
  ) where

import Cal.Types (Kind (..))
import Control.Exception (SomeException, displayException, try)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Hledger

-- | Read journal files with hledger's own reader. hledger throws (rather than
-- returns Left) for a missing file, so exceptions are folded into the Left.
loadJournal :: InputOpts -> [FilePath] -> IO (Either String Journal)
loadJournal iopts files = do
  r <- try (runExceptT (readJournalFiles iopts files))
  pure $ case r of
    Left e -> Left (displayException (e :: SomeException))
    Right x -> x

-- | Revenue accounts are income, Expense accounts are expenses, all else is ignored.
postingKind :: Journal -> Posting -> Maybe Kind
postingKind j p = case journalAccountType j (paccount p) of
  Just Revenue -> Just KIncome
  Just Expense -> Just KExpense
  _ -> Nothing

-- | True for periodic/auto-generated (forecast) transactions.
isForecastTxn :: Transaction -> Bool
isForecastTxn t = any ((`elem` ["_generated-transaction", "generated-transaction"]) . fst) (ttags t)

-- | The posting's own @location@ tag, else its transaction's.
postingLocation :: Posting -> Maybe Text
postingLocation p = listToMaybe [v | ("location", v) <- ptags p ++ maybe [] ttags (ptransaction p)]

-- | (first transaction date or today, max of today and last transaction date).
journalDataRange :: Day -> Journal -> (Day, Day)
journalDataRange today j = case map tdate (jtxns j) of
  [] -> (today, today)
  ds -> (minimum ds, max today (maximum ds))

-- | Commodity of the first posting amount in parse order.
firstSeenCommodity :: Journal -> Maybe CommoditySymbol
firstSeenCommodity j = listToMaybe
  [ acommodity a
  | t <- jtxns j, p <- tpostings t, a <- amounts (pamount p), not (null (show (acommodity a))) ]
