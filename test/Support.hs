module Support
  ( fixtureToday
  , designPath
  , edgePath
  , loadFixture
  ) where

import Cal.Journal (loadJournal)
import Data.Time.Calendar (Day, fromGregorian)
import Hledger.Data.Types (Journal)
import Hledger.Read.InputOptions (definputopts)
import Test.Tasty.HUnit (assertFailure)

fixtureToday :: Day
fixtureToday = fromGregorian 2026 11 6

designPath, edgePath :: FilePath
designPath = "test/fixtures/design.journal"
edgePath = "test/fixtures/edge.journal"

loadFixture :: FilePath -> IO Journal
loadFixture path = do
  r <- loadJournal definputopts [path]
  either (assertFailure . ("fixture failed to load: " ++)) pure r
