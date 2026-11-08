module Cal.Types
  ( Interval (..), Face (..), RateAt (..), BaseSource (..), Valuation (..)
  , Kind (..), Status (..), Span (..), ColorMode (..), GlyphSet (..)
  , Output (..), Terminal (..), Cursor (..), Settings (..), defaultCursor
  , CommoditySymbol, Query, Day
  ) where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Hledger.Data.Types (CommoditySymbol)
import Hledger.Query (Query)

data Interval = IYear | IQuarter | IMonth | IWeek
  deriving (Eq, Ord, Show, Enum, Bounded)

data Face = FaceYear | FaceQuarter | FaceMonth | FaceWeek | FaceCommodities
  deriving (Eq, Ord, Show, Enum, Bounded)

data RateAt = AtPeriodEnd | AtTxnDate | AtToday
  deriving (Eq, Show, Enum, Bounded)

data BaseSource = BaseFromX | BaseFromD | BaseFirstSeen
  deriving (Eq, Show)

data Valuation = Native | ValuedIn !CommoditySymbol
  deriving (Eq, Show)

data Kind = KIncome | KExpense
  deriving (Eq, Show)

data Status = Closed | Open | Future
  deriving (Eq, Show)

-- | Half-open span [start, end).
data Span = Span { spStart :: !Day, spEnd :: !Day }
  deriving (Eq, Show)

data ColorMode = ColorNone | Color256 | ColorTrue
  deriving (Eq, Show)

data GlyphSet = GlyphsUtf8 | GlyphsAscii
  deriving (Eq, Show)

-- | Output mode; 'OutPrint' carries the print width.
data Output = OutTui | OutPrint !Int
  deriving (Eq, Show)

data Terminal = Terminal
  { tIsTty :: !Bool, tUtf8 :: !Bool, tColumns :: !(Maybe Int)
  , tNoColorEnv :: !Bool, tTrueColor :: !Bool }
  deriving (Eq, Show)

-- | 'cuCommodity' is the selected Commodities row; Nothing until the user
-- moves it, which selects the first non-base row (design F6, review M8).
data Cursor = Cursor { cuWeekTxn :: !Int, cuCommodity :: !(Maybe Int) }
  deriving (Eq, Show)

data Settings = Settings
  { sFace :: !Face, sCommInterval :: !Interval, sFocus :: !Day
  , sDataRange :: !(Day, Day)  -- ^ (first posting date, max today lastPostingDate); navigation clamp
  , sValuation :: !Valuation, sRateAt :: !RateAt
  , sDefaultBase :: !CommoditySymbol, sBaseSource :: !BaseSource
  , sQueryArgs :: ![Text], sQuery :: !Query
    -- ^ the args query combined with 'sFlagQuery'
  , sFlagQuery :: !Query    -- ^ status (-U -P -C) and -R terms from the flags
  , sFlagArgs :: ![Text]    -- ^ those flags, as echoed in the command line
  , sToday :: !Day, sStaleDays :: !Integer, sForecast :: !Bool, sInferPrices :: !Bool
  , sJournalFile :: !FilePath, sOutput :: !Output, sGlyphSet :: !GlyphSet, sColor :: !ColorMode }
  deriving (Eq, Show)

defaultCursor :: Cursor
defaultCursor = Cursor 0 Nothing
