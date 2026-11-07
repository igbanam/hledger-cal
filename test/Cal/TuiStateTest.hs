module Cal.TuiStateTest where

import Cal.Journal (journalDataRange)
import Cal.Model.Common (Env (..))
import Cal.Model.Commodities (CommodityRow (..), CommoditiesModel (..), commoditiesModel)
import Cal.Tui.State
import Cal.Types
import Data.Maybe (isJust)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import qualified Graphics.Vty as Vty
import Hledger.Query (Query (Any))
import Support (edgePath, envFor, fixtureToday, loadFixture)
import Test.Tasty.HUnit

d :: Integer -> Int -> Int -> Day
d = fromGregorian

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 11 6) 0

focus :: UiState -> Day
focus = sFocus . envSettings . usEnv

st :: UiState -> Settings
st = envSettings . usEnv

run :: UiState -> [Cmd] -> UiState
run = foldl (\s c -> fst (step t0 c s))

mk :: [String] -> IO UiState
mk args = initState <$> envFor args

unit_moveDay, unit_moveWeek, unit_moveIntoNext, unit_nextPeriod, unit_clampFuture :: IO ()
unit_moveDay = (\s0 -> focus (run s0 [CmdMove 1 0]) @?= d 2026 10 2) =<< mk ["-M", "-p", "2026-10"]
unit_moveWeek = (\s0 -> focus (run s0 [CmdMove 0 1]) @?= d 2026 10 8) =<< mk ["-M", "-p", "2026-10"]
unit_moveIntoNext = (\s0 -> focus (run s0 [CmdMove 0 (-1)]) @?= d 2026 9 24) =<< mk ["-M", "-p", "2026-10"]
unit_nextPeriod = (\s0 -> focus (run s0 [CmdPeriod 1]) @?= d 2026 11 1) =<< mk ["-M", "-p", "2026-10"]
unit_clampFuture = (\s0 -> focus (run s0 (replicate 3 (CmdPeriod 1))) @?= fixtureToday) =<< mk ["-M", "-p", "2026-10"]

unit_faceKeepsDate :: IO ()
unit_faceKeepsDate = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 [CmdMove 0 3, CmdFace FaceWeek]
  (sFace (st s), focus s) @?= (FaceWeek, d 2026 10 22)

unit_cycleCurrency :: IO ()
unit_cycleCurrency = do
  s0 <- mk ["-M", "-p", "2026-10"]
  map (sValuation . st) (scanl (\s c -> fst (step t0 c s)) s0 (replicate 7 CmdCycleCurrency))
    @?= map ValuedIn ["₦", "$", "Dh", "€", "₹", "KD"] ++ [Native, ValuedIn "₦"]

unit_cycleRate :: IO ()
unit_cycleRate = do
  s0 <- mk ["-M", "-p", "2026-10"]
  sRateAt (st (run s0 [CmdCycleRate, CmdCycleRate])) @?= AtToday

unit_enterRegister, unit_escCloses :: IO ()
unit_enterRegister = (\s0 -> usPane (run s0 [CmdEnter]) @?= PaneRegister 0) =<< mk ["-M", "-p", "2026-10"]
unit_escCloses = (\s0 -> usPane (run s0 [CmdEnter, CmdEsc]) @?= PaneNone) =<< mk ["-M", "-p", "2026-10"]

unit_drillAndBack :: IO ()
unit_drillAndBack = do
  y <- mk ["-Y"]
  let s = run y [CmdMove 0 (-1), CmdEnter]
  (sFace (st s), focus s) @?= (FaceMonth, d 2026 10 1)
  let b = fst (step t0 CmdEsc s)
  (sFace (st b), focus b) @?= (FaceYear, d 2026 10 6)

unit_filterOk, unit_filterBad :: IO ()
unit_filterOk = do
  s0 <- mk ["-M", "-p", "2026-10"]
  sQueryArgs (st (run s0 (CmdFilterOpen : map CmdFilterChar "acct:fuel" ++ [CmdFilterSubmit]))) @?= ["acct:fuel"]
unit_filterBad = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 (CmdFilterOpen : map CmdFilterChar "date:nonsense" ++ [CmdFilterSubmit])
  (sQueryArgs (st s), isJust (usMessage s)) @?= ([], True)

unit_quit :: IO ()
unit_quit = (\s0 -> snd (step t0 CmdQuit s0) @?= EffQuit) =<< mk ["-M", "-p", "2026-10"]

unit_reloadError, unit_messageExpires :: IO ()
unit_reloadError = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = applyReload t0 (Left "parse error at line 3\nmore") s0
  (visibleMessage t0 s, focus s) @?= (Just "parse error at line 3", d 2026 10 1)
unit_messageExpires = do
  s0 <- mk ["-M", "-p", "2026-10"]
  visibleMessage (addUTCTime 4 t0) (applyReload t0 (Left "x") s0) @?= Nothing

unit_commNoPrice :: IO ()
unit_commNoPrice = do
  c <- mk ["--commodities", "-p", "2026-10", "-f", edgePath]
  let s = run c [CmdMove 0 1, CmdEnter]
  fmap fst (usMessage s) @?= Just "no price for BTC"

unit_keys :: IO ()
unit_keys =
  (keyCmd PaneNone (Vty.KChar 'Y') [], keyCmd PaneNone (Vty.KChar ']') [], keyCmd (PaneFilter "") (Vty.KChar 'q') [])
    @?= (Just (CmdFace FaceYear), Just (CmdPeriod 1), Just (CmdFilterChar 'q'))

-- Native-mode "value in this" (controller ruling).
unit_commNativePriced :: IO ()
unit_commNativePriced = do
  c <- mk ["--commodities", "-p", "2026-10"]
  let rowsOf u = let m = commoditiesModel (usEnv u) (usCursor u) in cmCurrencies m ++ cmSecurities m
      n = c { usEnv = (usEnv c) { envSettings = (envSettings (usEnv c)) { sValuation = Native } } }
      priced = [crCommodity r | r <- rowsOf c, not (crIsBase r), crPrice r /= Nothing]
      idx = [i | (i, r) <- zip [0 ..] (rowsOf n), crCommodity r `elem` take 1 priced]
  case (idx, priced) of
    (i : _, p : _) -> do
      let s = run n [CmdMove 0 i, CmdEnter]
      (usMessage s, sValuation (st s)) @?= (Nothing, ValuedIn p)
    _ -> assertFailure "fixture has no priced non-base row"

unit_commNativeUnpriced :: IO ()
unit_commNativeUnpriced = do
  c <- mk ["--commodities", "-p", "2026-10", "-f", edgePath]
  let n = c { usEnv = (usEnv c) { envSettings = (envSettings (usEnv c)) { sValuation = Native } } }
      s = run n [CmdMove 0 1, CmdEnter]
  (fmap fst (usMessage s), sValuation (st s)) @?= (Just "no price for BTC", Native)

unit_filterEmptyClears :: IO ()
unit_filterEmptyClears = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 (CmdFilterOpen : map CmdFilterChar "acct:fuel" ++ [CmdFilterSubmit, CmdFilterOpen] ++ replicate 9 CmdFilterBackspace ++ [CmdFilterSubmit])
  (sQueryArgs (st s), sQuery (st s), usPane s) @?= ([], Any, PaneNone)

unit_reloadRecomputesRange :: IO ()
unit_reloadRecomputesRange = do
  s0 <- mk ["-M", "-p", "2026-10"]
  j <- loadFixture edgePath
  let s = applyReload t0 (Right j) s0
  sDataRange (st s) @?= journalDataRange (sToday (st s)) j

unit_nativeCycleToFirst :: IO ()
unit_nativeCycleToFirst = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let n = s0 { usEnv = (usEnv s0) { envSettings = (envSettings (usEnv s0)) { sValuation = Native } } }
  sValuation (st (run n [CmdCycleCurrency])) @?= ValuedIn "₦"

unit_escNothing :: IO ()
unit_escNothing = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 [CmdEsc]
  (usPane s, usBack s, focus s) @?= (PaneNone, [], d 2026 10 1)
