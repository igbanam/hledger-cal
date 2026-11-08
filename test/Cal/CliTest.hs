module Cal.CliTest where

import Cal.Cli
import Cal.Print (renderPrint)
import Cal.Model.Common (Kpis (..), kpisFor)
import Cal.Types
import Cal.Valuation (baseQty)
import Hledger (Quantity, Status (..), toRegexCI')
import Hledger.Query (Query (..))
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Support (designPath, edgePath, envFor, fixtureToday, settingsFor, testTerminal)
import Test.Tasty.HUnit

face :: [String] -> IO Face
face args = sFace . snd <$> settingsFor args

cmd :: [String] -> IO Text
cmd args = commandLine . snd <$> settingsFor args

unit_defaultFace :: Assertion
unit_defaultFace = do
  s <- snd <$> settingsFor []
  (sFace s, sFocus s, sDefaultBase s, sBaseSource s) @?= (FaceMonth, fixtureToday, "₦", BaseFromD)

unit_faces :: Assertion
unit_faces =
  mapM face [["-Y"], ["-Q"], ["-M"], ["-W"], ["--commodities"]]
    >>= (@?= [FaceYear, FaceQuarter, FaceMonth, FaceWeek, FaceCommodities])

unit_commInterval :: Assertion
unit_commInterval = do s <- snd <$> settingsFor ["--commodities", "-Y"]; sCommInterval s @?= IYear

unit_period :: Assertion
unit_period = do s <- snd <$> settingsFor ["-M", "-p", "2026-10"]; sFocus s @?= fromGregorian 2026 10 1

unit_valueX :: Assertion
unit_valueX = do
  s <- snd <$> settingsFor ["-X", "$"]
  (sValuation s, sBaseSource s) @?= (ValuedIn "$", BaseFromX)

unit_valueThen :: Assertion
unit_valueThen = do s <- snd <$> settingsFor ["--value=then"]; sRateAt s @?= AtTxnDate

unit_valueNow :: Assertion
unit_valueNow = do
  s <- snd <$> settingsFor ["--value=now,$"]
  (sRateAt s, sValuation s) @?= (AtToday, ValuedIn "$")

unit_native :: Assertion
unit_native = do s <- snd <$> settingsFor ["--native"]; sValuation s @?= Native

unit_nativeConflict :: Assertion
unit_nativeConflict = do
  (j, _) <- settingsFor []
  o <- parseCli ["--native", "-X", "$", "-f", designPath]
  mkSettings fixtureToday testTerminal o j @?= Left "--native can't be combined with -X, -V or --value"

unit_inferDefault :: Assertion
unit_inferDefault = do s <- snd <$> settingsFor []; sInferPrices s @?= True

unit_noInfer :: Assertion
unit_noInfer = do s <- snd <$> settingsFor ["--no-infer-market-prices"]; sInferPrices s @?= False

unit_staleDays :: Assertion
unit_staleDays = do s <- snd <$> settingsFor ["--stale-days", "10"]; sStaleDays s @?= 10

unit_printWidth :: Assertion
unit_printWidth = do s <- snd <$> settingsFor ["--print", "--width", "100"]; sOutput s @?= OutPrint 100

unit_firstSeenBase :: Assertion
unit_firstSeenBase = do
  s <- snd <$> settingsFor ["-f", edgePath]
  (sDefaultBase s, sBaseSource s) @?= ("₦", BaseFirstSeen)

unit_query :: Assertion
unit_query = do s <- snd <$> settingsFor ["acct:expenses"]; sQueryArgs s @?= ["acct:expenses"]

unit_cmdMonth :: Assertion
unit_cmdMonth = cmd ["-M", "-p", "2026-10"] >>= (@?= "hledger cal -M -p 2026-10 -f design.journal")

unit_cmdWeek :: Assertion
unit_cmdWeek = cmd ["-W", "-p", "2026-10-28"] >>= (@?= "hledger cal -W -p 2026-10-26 -f design.journal")

unit_cmdQuarter :: Assertion
unit_cmdQuarter = cmd ["-Q"] >>= (@?= "hledger cal -Q -p 2026 -f design.journal")

unit_cmdCommQ :: Assertion
unit_cmdCommQ = cmd ["--commodities", "-Q"] >>= (@?= "hledger cal --commodities -Q -p 2026q4 -f design.journal")

unit_cmdUsdThen :: Assertion
unit_cmdUsdThen =
  cmd ["-M", "-X", "$", "--value=then", "desc:Salary Oct"]
    >>= (@?= "hledger cal -M -p 2026-11 -X $ --value=then -f design.journal 'desc:Salary Oct'")

statusPath :: FilePath
statusPath = "test/fixtures/status.journal"

-- | October expenses with the given flags, as hledger bal expenses gives them.
statusExpenses :: [String] -> IO Quantity
statusExpenses flags = do
  e <- envFor (["-M", "-p", "2026-10", "-f", statusPath] ++ flags)
  pure (baseQty "₦" (kExpenses (kpisFor e IMonth (Span (fromGregorian 2026 10 1) (fromGregorian 2026 11 1)))))

unit_statusFlagsTotals :: Assertion
unit_statusFlagsTotals =
  mapM statusExpenses [[], ["-C"], ["-U"], ["-R"], ["-C", "-R"], ["status:*"]]
    >>= (@?= [2200, 1000, 1200, 1500, 1000, 1000])

unit_statusFlagsEchoed :: Assertion
unit_statusFlagsEchoed =
  cmd ["-M", "-p", "2026-10", "-R", "-C", "-U", "acct:expenses"]
    >>= (@?= "hledger cal -M -p 2026-10 -U -C -R -f design.journal acct:expenses")

unit_statusFlagKeptByFilter :: Assertion
unit_statusFlagKeptByFilter = do
  s <- snd <$> settingsFor ["-C", "acct:expenses"]
  (sFlagQuery s, sQuery s) @?= (StatusQ Cleared, And [StatusQ Cleared, Acct (toRegexCI' "expenses")])

rejected :: [String] -> IO (Either String ())
rejected flags = do
  (j, _) <- settingsFor []
  o <- parseCli (flags ++ ["-f", designPath])
  pure (() <$ mkSettings fixtureToday testTerminal o j)

unit_rejectsNoCalendarMeaning :: Assertion
unit_rejectsNoCalendarMeaning =
  mapM rejected [["--depth=1"], ["-E"], ["-B"], ["--date2"], ["--pivot=payee"], ["-D"], ["--pretty"]]
    >>= (@?= map (Left . (++ " has no meaning in hledger cal"))
          ["--depth", "-E/--empty", "-B/--cost", "--date2", "--pivot", "-D/--daily", "--pretty"])

-- | A date: query term sets the focus period, as -p does (hledger merges
-- both into the report span); it leaves the query args.
unit_dateQueryFocus :: Assertion
unit_dateQueryFocus = do
  s <- snd <$> settingsFor ["date:2026-10"]
  (sFace s, sFocus s, sQueryArgs s) @?= (FaceMonth, fromGregorian 2026 10 1, [])
  cmd ["date:2026-10", "acct:expenses"] >>= (@?= "hledger cal -M -p 2026-10 -f design.journal acct:expenses")

-- | With -p and date: both given, the focus comes from their intersection.
unit_dateQueryIntersectsPeriod :: Assertion
unit_dateQueryIntersectsPeriod = do
  s <- snd <$> settingsFor ["-M", "-p", "2026", "date:2026-10"]
  sFocus s @?= fromGregorian 2026 10 1
  w <- snd <$> settingsFor ["-W", "-p", "2026-10", "date:2026-10-14.."]
  sFocus w @?= fromGregorian 2026 10 14

unit_dateQueryPrintsAsPeriod :: Assertion
unit_dateQueryPrintsAsPeriod = do
  a <- envFor ["-M", "date:2026-10", "--color=never"]
  b <- envFor ["-M", "-p", "2026-10", "--color=never"]
  renderPrint a 140 @?= renderPrint b 140

-- | An interval the calendar has no face for is rejected, named as typed.
unit_rejectsFacelessInterval :: Assertion
unit_rejectsFacelessInterval = do
  let viaP p = Left ("the interval in -p \"" ++ p ++ "\" has no meaning in hledger cal; use -Y, -Q, -M or -W")
  mapM rejected [["-p", "daily in 2026-10"], ["-p", "every 2 weeks in 2026-10"], ["-p", "every 2 days"], ["-D", "-p", "bimonthly"]]
    >>= (@?= map viaP ["daily in 2026-10", "every 2 weeks in 2026-10", "every 2 days", "bimonthly"])
  rejected ["-p", "every 2 weeks", "-D"] >>= (@?= Left "-D/--daily has no meaning in hledger cal")
  mapM rejected [["-p", "weekly in 2026-10"], ["-p", "daily in 2026-10", "-M"]] >>= (@?= [Right (), Right ()])
  face ["-p", "weekly in 2026-10"] >>= (@?= FaceWeek)

-- | -p/-b/-e and date: terms that don't overlap leave hledger an empty
-- report span; they are rejected, each named as typed.
unit_rejectsDisjointDates :: Assertion
unit_rejectsDisjointDates = do
  mapM rejected
    [ ["-p", "2026-09", "date:2026-10"], ["-b", "2026-11", "date:2026-10"]
    , ["date:2026-09", "acct:fuel", "date:2026-10"], ["-p", "last month", "date:2026-08"] ]
    >>= (@?= map Left
          [ "-p 2026-09 and date:2026-10 do not overlap", "-b 2026-11 and date:2026-10 do not overlap"
          , "date:2026-09 and date:2026-10 do not overlap", "-p 'last month' and date:2026-08 do not overlap" ])
  mapM rejected [["-p", "2026", "date:2026-10"], ["-p", "2026-09", "-e", "2026-10-15", "date:2026-10"]]
    >>= (@?= [Right (), Right ()])
