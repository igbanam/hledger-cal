module Support
  ( fixtureToday
  , designPath
  , edgePath
  , loadFixture
  , testTerminal
  , settingsFor
  , envFor
  ) where

import Cal.Cli (mkSettings, parseCli)
import Cal.Journal (loadJournal)
import Cal.Model.Common (Env, mkEnv)
import Cal.Types (Settings, Terminal (..))
import Data.Time.Calendar (Day, fromGregorian)
import Hledger.Data.Types (Journal)
import Hledger.Cli.CliOptions (CliOpts (..))
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

-- | A non-TTY, UTF-8, truecolor terminal, 140 columns wide, NO_COLOR unset.
testTerminal :: Terminal
testTerminal = Terminal False True (Just 140) False True

-- | Parse the args (adding @-f design.journal@ unless a file is given),
-- load the journal with the parsed input options and resolve settings.
settingsFor :: [String] -> IO (Journal, Settings)
settingsFor args0 = do
  let args = if "-f" `elem` args0 then args0 else args0 ++ ["-f", designPath]
  opts <- parseCli args
  r <- loadJournal (inputopts_ opts) (file_ opts)
  j <- either (assertFailure . ("journal failed to load: " ++)) pure r
  either (assertFailure . ("settings failed: " ++)) (pure . (,) j) (mkSettings fixtureToday testTerminal opts j)

envFor :: [String] -> IO Env
envFor args = uncurry mkEnv <$> settingsFor args
