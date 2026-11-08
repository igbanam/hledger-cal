-- | Pure TUI state machine: key mapping, commands and state transitions.
module Cal.Tui.State
  ( Pane (..)
  , UiState (..)
  , Cmd (..)
  , Effect (..)
  , initState
  , keyCmd
  , step
  , applyReload
  , visibleMessage
  , errorSummary
  ) where

import Cal.Currency (codeOf)
import Cal.Journal (journalDataRange)
import Cal.Model.Commodities (CommodityRow (..), CommoditiesModel (..), commoditiesModel)
import Cal.Model.Common (Env (..), mkEnv, withEnvSettings)
import Cal.Model.Shared (currencyChoices)
import Cal.Model.Week (WeekDay (..), WeekModel (..), weekModel)
import Cal.Period (clampFocus, quarterOf, stepFocus)
import Cal.Prices (rateOn)
import Cal.Types
import Data.Char (isDigit, isSpace)
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Graphics.Vty (Key (..), Modifier (..))
import Hledger.Data.Types (Journal)
import Hledger.Query (Query (..), parseQuery, simplifyQuery)
import System.FilePath (takeFileName)

-- | 'PaneFilter' carries the draft and the pane to return to on Enter/Esc.
data Pane = PaneNone | PaneRegister !Int | PaneFilter !Text !Pane | PaneHelp
  deriving (Eq, Show)

data UiState = UiState
  { usEnv :: !Env, usCursor :: !Cursor, usPane :: !Pane
  , usBack :: ![(Face, Day)], usMessage :: !(Maybe (Text, UTCTime)) }

data Cmd
  = CmdPeriod !Int | CmdFace !Face | CmdCycleFace !Int | CmdCycleCurrency | CmdCycleRate
  | CmdMove !Int !Int | CmdEnter | CmdEsc | CmdFilterOpen | CmdFilterChar !Char | CmdFilterBackspace
  | CmdFilterSubmit | CmdReload | CmdHelp | CmdQuit | CmdScroll !Int
  | CmdScrollEdge !Bool  -- ^ True = top (Home), False = bottom (End)
  deriving (Eq, Show)

data Effect = EffNone | EffQuit | EffReload
  deriving (Eq, Show)

initState :: Env -> UiState
initState env = UiState env defaultCursor PaneNone [] Nothing

keyCmd :: Pane -> Key -> [Modifier] -> Maybe Cmd
keyCmd pane key mods = case pane of
  PaneNone -> case key of
    KChar '[' -> Just (CmdPeriod (-1))
    KChar ']' -> Just (CmdPeriod 1)
    KChar 'Y' -> Just (CmdFace FaceYear)
    KChar 'Q' -> Just (CmdFace FaceQuarter)
    KChar 'M' -> Just (CmdFace FaceMonth)
    KChar 'W' -> Just (CmdFace FaceWeek)
    KChar 'C' -> Just (CmdFace FaceCommodities)
    KChar '\t' -> Just (CmdCycleFace 1)
    KBackTab -> Just (CmdCycleFace (-1))
    KChar 'c' -> Just CmdCycleCurrency
    KChar 'v' -> Just CmdCycleRate
    KLeft -> Just (CmdMove (-1) 0)
    KRight -> Just (CmdMove 1 0)
    KUp -> Just (CmdMove 0 (-1))
    KDown -> Just (CmdMove 0 1)
    KEnter -> Just CmdEnter
    KEsc -> Just CmdEsc
    KChar '/' -> Just CmdFilterOpen
    KChar 'g' -> Just CmdReload
    KChar '?' -> Just CmdHelp
    KChar 'q' -> Just CmdQuit
    _ -> Nothing
  PaneFilter _ _ -> case key of
    KChar ch | MCtrl `notElem` mods, MAlt `notElem` mods, ch /= '\t' -> Just (CmdFilterChar ch)
    KBS -> Just CmdFilterBackspace
    KEnter -> Just CmdFilterSubmit
    KEsc -> Just CmdEsc
    _ -> Nothing
  PaneRegister _ -> case key of
    KUp -> Just (CmdScroll (-1))
    KDown -> Just (CmdScroll 1)
    KPageUp -> Just (CmdScroll (-10))
    KPageDown -> Just (CmdScroll 10)
    KHome -> Just (CmdScrollEdge True)
    KEnd -> Just (CmdScrollEdge False)
    KChar 'c' -> Just CmdCycleCurrency
    KChar 'v' -> Just CmdCycleRate
    KChar '/' -> Just CmdFilterOpen
    KEnter -> Just CmdEsc
    KEsc -> Just CmdEsc
    _ -> Nothing
  PaneHelp -> case key of
    KEsc -> Just CmdEsc
    _ -> Nothing

faceOrder :: [Face]
faceOrder = [FaceYear, FaceQuarter, FaceMonth, FaceWeek, FaceCommodities]

settingsOf :: UiState -> Settings
settingsOf = envSettings . usEnv

-- | Replace the settings, rebuilding the env.
withSettings :: (Settings -> Settings) -> UiState -> UiState
withSettings f us = us { usEnv = withEnvSettings (f (envSettings env)) env }
  where env = usEnv us

setFocus :: Day -> UiState -> UiState
setFocus d us = withSettings (\s -> s { sFocus = clampFocus (sDataRange s) d }) us

-- | Focus change that also resets the register-line cursor.
refocus :: Day -> UiState -> UiState
refocus d us = (setFocus d us) { usCursor = (usCursor us) { cuWeekTxn = 0 } }

message :: UTCTime -> Text -> UiState -> UiState
message now t us = us { usMessage = Just (t, now) }

-- | Step n places through xs, wrapping; an unknown x goes to the first element.
cycleFrom :: Eq a => a -> [a] -> a -> Int -> a
cycleFrom dflt xs x n = case elemIndex x xs of
  Just i -> xs !! ((i + n) `mod` length xs)
  Nothing -> dflt

done :: UiState -> (UiState, Effect)
done u = (u, EffNone)

step :: UTCTime -> Cmd -> UiState -> (UiState, Effect)
step now cmd us = case cmd of
  CmdQuit -> (us, EffQuit)
  CmdReload -> (us, EffReload)
  CmdPeriod n -> done (refocus (stepFocus (sFace s) (sCommInterval s) n (sFocus s)) us)
  CmdFace f -> done (withSettings (\st -> st { sFace = f }) us)
  CmdCycleFace n -> done (withSettings (\st -> st { sFace = cycleFrom FaceYear faceOrder (sFace st) n }) us)
  CmdCycleCurrency ->
    let choices = map ValuedIn (currencyChoices (usEnv us)) ++ [Native]
    in done (withSettings (\st -> st { sValuation = cycleFrom Native choices (sValuation st) 1 }) us)
  CmdCycleRate ->
    let next r = case r of
          AtPeriodEnd -> AtTxnDate
          AtTxnDate -> AtToday
          AtToday -> AtPeriodEnd
    in done (withSettings (\st -> st { sRateAt = next (sRateAt st) }) us)
  CmdMove dx dy -> done (move dx dy us)
  CmdEnter -> done (enter now us)
  CmdEsc -> done $ case usPane us of
    PaneNone -> case usBack us of
      ((f, d) : rest) -> (withSettings (\st -> st { sFace = f, sFocus = d }) us) { usBack = rest }
      [] -> us
    PaneFilter _ ret -> us { usPane = ret }
    _ -> us { usPane = PaneNone }
  CmdFilterOpen -> done us { usPane = PaneFilter (T.unwords (sQueryArgs s)) returnTo }
  CmdFilterChar c -> done (onDraft (`T.snoc` c) us)
  CmdFilterBackspace -> done (onDraft (\t -> if T.null t then t else T.init t) us)
  CmdFilterSubmit -> done $ case usPane us of
    PaneFilter draft ret -> (submitFilter now draft us) { usPane = afterSubmit ret }
    _ -> us
  CmdHelp -> done us { usPane = PaneHelp }
  CmdScroll n -> done $ case usPane us of
    PaneRegister i -> us { usPane = PaneRegister (max 0 (i + n)) }
    _ -> us
  CmdScrollEdge top -> done $ case usPane us of
    PaneRegister _ -> us { usPane = PaneRegister (if top then 0 else 1000000) }  -- renderer clamps
    _ -> us
  where
    s = settingsOf us
    returnTo = case usPane us of
      PaneRegister i -> PaneRegister i
      _ -> PaneNone
    afterSubmit ret = case ret of
      PaneRegister _ -> PaneRegister 0
      p -> p

onDraft :: (Text -> Text) -> UiState -> UiState
onDraft f us = case usPane us of
  PaneFilter t ret -> us { usPane = PaneFilter (f t) ret }
  _ -> us

submitFilter :: UTCTime -> Text -> UiState -> UiState
submitFilter now draft us
  | T.null (T.strip draft) = withSettings (\st -> st { sQuery = sFlagQuery st, sQueryArgs = [] }) us
  | otherwise = case parseQuery (sToday (settingsOf us)) draft of
      Right (q, _) -> withSettings (\st -> st
        { sQuery = simplifyQuery (And [sFlagQuery st, q]), sQueryArgs = map T.pack (words (T.unpack draft)) }) us
      Left err -> message now (errorSummary err) us

-- | One footer line from a (possibly multi-line) hledger error: a leading
-- @path:pos:@ location is shortened to the file name and followed by the
-- first line of text after it, skipping blank and source-excerpt (@|@) lines.
errorSummary :: String -> Text
errorSummary err = case lines err of
  [] -> ""
  (l : ls) -> T.strip . T.pack $ case filter (\x -> not (blank x || excerpt x)) ls of
    (m : _) | isLocation l -> takeFileName (trim l) ++ " " ++ trim m
            | otherwise -> trim l
    [] -> trim l
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    blank = all isSpace
    excerpt x = take 1 (dropWhile (\c -> isSpace c || isDigit c) x) == "|"
    isLocation x = ":" `isSuffixOf` trim x

move :: Int -> Int -> UiState -> UiState
move dx dy us = case sFace s of
  FaceYear -> refocus (months (3 * dx + dy) focus) us
  FaceQuarter -> refocus (months (3 * (dx + 2 * dy)) focus) us
  FaceMonth -> refocus (addDays (fromIntegral (dx + 7 * dy)) focus) us
  FaceWeek
    | dx /= 0 -> refocus (addDays (fromIntegral dx) focus) us
    | otherwise ->
        let w = weekModel (usEnv us) (usCursor us)
            count = case [length (wdTxns d) | d <- wkDays w, wdDay d == focus] of
              (n : _) -> n
              [] -> 0
            cur = cuWeekTxn (usCursor us)
        in us { usCursor = (usCursor us) { cuWeekTxn = max 0 (min (count - 1) (cur + dy)) } }
  FaceCommodities ->
    let m = commoditiesModel (usEnv us) (usCursor us)
        n = length (cmCurrencies m) + length (cmSecurities m)
        cur = cuCommodity (usCursor us)
    in us { usCursor = (usCursor us) { cuCommodity = max 0 (min (n - 1) (cur + dy)) } }
  where
    s = settingsOf us
    focus = sFocus s
    months k = stepFocus FaceMonth IMonth k

enter :: UTCTime -> UiState -> UiState
enter now us = case sFace s of
  FaceYear -> drill (monthStart focus) (\d -> sameMonth d today)
  FaceQuarter ->
    let y = year focus
        first = fromGregorian y (3 * (quarterOf focus - 1) + 1) 1
    in drillTo first (quarterOf today == quarterOf focus && year today == y)
  FaceMonth -> us { usPane = PaneRegister 0 }
  FaceWeek -> us { usPane = PaneRegister 0 }
  FaceCommodities ->
    let m = commoditiesModel env (usCursor us)
        rows = cmCurrencies m ++ cmSecurities m
    in case drop (cuCommodity (usCursor us)) rows of
         (r : _)
           | ok m (crCommodity r) -> withSettings (\st -> st { sValuation = ValuedIn (crCommodity r) }) us
           | otherwise -> message now ("no price for " <> codeOf (crCommodity r)) us
         [] -> us
      where
        ok m c = case sValuation s of
          Native -> priced m (sDefaultBase s) c
          ValuedIn b -> priced m b c
        priced m b c = c == b || isJust (rateOn (envPrices env) c b (cmHeldAt m))
  where
    env = usEnv us
    s = settingsOf us
    focus = sFocus s
    today = sToday s
    year d = let (y, _, _) = toGregorian d in y
    monthStart d = let (y, m, _) = toGregorian d in fromGregorian y m 1
    sameMonth a b = monthStart a == monthStart b
    drill first isCurrent = drillTo first (isCurrent first)
    drillTo first current =
      let target = if current then today else first
          pushed = us { usBack = (sFace s, focus) : usBack us }
      in withSettings (\st -> st { sFace = FaceMonth, sFocus = clampFocus (sDataRange st) target }) pushed

-- | Rebuild after a journal reload.
applyReload :: UTCTime -> Either String Journal -> UiState -> UiState
applyReload now r us = case r of
  Left err -> message now (errorSummary err) us
  Right j ->
    let s = settingsOf us
        range = journalDataRange (sToday s) j
        s' = s { sDataRange = range, sFocus = clampFocus range (sFocus s) }
    in us { usEnv = mkEnv j s' }

visibleMessage :: UTCTime -> UiState -> Maybe Text
visibleMessage now us = case usMessage us of
  Just (t, at) | diffUTCTime now at < 3 -> Just t
  _ -> Nothing
