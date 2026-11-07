-- | The register pane: the selected day's postings with a running total.
module Cal.Model.Register
  ( RegisterRow (..)
  , registerRows
  ) where

import Cal.Model.Common
import Cal.Types
import Cal.Valuation
import Data.List (mapAccumL)
import Data.Text (Text)
import Hledger hiding (Interval (..), Status)

data RegisterRow = RegisterRow
  { rgDate :: !(Maybe Day), rgForecast :: !Bool, rgDescription :: !(Maybe Text)
  , rgAccount :: !AccountName, rgNative :: !MixedAmount, rgRate :: !(Maybe Quantity)
  , rgValue, rgRunning :: !Money }

-- | The day's income/expense postings (query applied), valued with the span
-- as the valuation period. Values are signed (income +, expense -); the
-- running total is cumulative; the date shows on the first row only and the
-- description on a transaction's first row.
registerRows :: Env -> Span -> Day -> [RegisterRow]
registerRows env span' day = snd (mapAccumL step (0, Nothing, True) vs)
  where
    vs = filter ((== day) . vpDate) (periodPostings env span')
    step (run, lastTxn, firstRow) v =
      let t = vpTxn v
          key = (tindex t, tdate t, tdescription t)
          signed = if vpKind v == KIncome then vpValue v else negate (vpValue v)
          run' = run + signed
          row = RegisterRow
            { rgDate = if firstRow then Just day else Nothing
            , rgForecast = vpForecast v
            , rgDescription = if lastTxn == Just key then Nothing else Just (tdescription t)
            , rgAccount = paccount (vpPosting v), rgNative = vpNative v, rgRate = vpRate v
            , rgValue = signed, rgRunning = run' }
      in ((run', Just key, False), row)
