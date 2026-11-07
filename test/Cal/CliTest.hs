module Cal.CliTest where

import Cal.Cli
import Cal.Types
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Support (designPath, edgePath, fixtureToday, settingsFor, testTerminal)
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
