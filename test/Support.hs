module Support
  ( fixtureToday
  , designPath
  , edgePath
  , loadFixture
  , testTerminal
  , settingsFor
  , envFor
  , cellStyles
  , cursorCells
  ) where

import qualified Data.Text as T
import Cal.Doc (Block (..), Line (..), Seg (..), Style (..), textW)
import Cal.Theme (Role (RCursorBorder))

import Cal.Cli (loadInputOpts, mkSettings, parseCli)
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
  r <- loadJournal (loadInputOpts fixtureToday opts) (file_ opts)
  j <- either (assertFailure . ("journal failed to load: " ++)) pure r
  either (assertFailure . ("settings failed: " ++)) (pure . (,) j) (mkSettings fixtureToday testTerminal opts j)

envFor :: [String] -> IO Env
envFor args = uncurry mkEnv <$> settingsFor args

-- | Every cell of a block with its style, line by line (a wide character
-- takes one entry).
cellStyles :: Block -> [[(Char, Style)]]
cellStyles b = [ concat [ [ (c, s) | c <- T.unpack t ] | Seg s t <- ss ] | Line ss <- bLines b ]

-- | (line, column, character) of every cell in the cursor-border role.
cursorCells :: Block -> [(Int, Int, Char)]
cursorCells b =
  [ (i, col, c)
  | (i, Line ss) <- zip [0 ..] (bLines b)
  , (col, c, s) <- cols 0 [ (c, s) | Seg s t <- ss, c <- T.unpack t ]
  , stRole s == RCursorBorder ]
  where
    cols _ [] = []
    cols n ((c, s) : rest) = (n, c, s) : cols (n + textW (T.singleton c)) rest
