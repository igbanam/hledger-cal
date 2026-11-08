-- | Pure TUI state machine: key mapping, commands and state transitions.
module Cal.Tui.State
  ( Pane (..)
  , UiState (..)
  , Cmd (..)
  , Effect (..)
  , PickerEntry (..)
  , pickerEntries
  , pickerMatches
  , initState
  , keyCmd
  , step
  , applyReload
  , visibleMessage
  , errorSummary
  ) where

import Cal.Cli (dateTermProblem, dropDateArgs, spanFocus)
import Cal.Currency (chipLabel, codeOf, symbolOf)
import Cal.Journal (journalDataRange)
import Cal.Model.Commodities (CommodityRow (..), CommoditiesModel (..), commoditiesModel)
import Cal.Model.Common (Env (..), mkEnv, withEnvSettings)
import Cal.Model.Shared (currencyChoices)
import Cal.Model.Week (WeekDay (..), WeekModel (..), weekModel)
import Cal.Period (clampFocus, quarterOf, stepFocus)
import Cal.Prices (rateOn)
import Cal.Types
import Cal.View.Types (Tier (..), tierFor)
import Data.Char (isDigit, isSpace)
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Graphics.Vty (Key (..), Modifier (..))
import Hledger.Data.Types (Journal)
import Hledger.Data.Types (DateSpan (..))
import Hledger.Query (Query (..), filterQuery, parseQuery, queryDateSpan, queryIsDateOrDate2, simplifyQuery)
import System.FilePath (takeFileName)

-- | 'PaneFilter' carries the draft and the pane to return to on Enter/Esc.
-- 'PanePicker' is the currency picker: its filter text, the highlighted
-- index into the entries matching it, and the pane to return to.
data Pane = PaneNone | PaneRegister !Int | PaneFilter !Text !Pane | PanePicker !Text !Int !Pane | PaneHelp
  deriving (Eq, Show)

-- | 'usWidth' is the terminal width the screen was last laid out for (0
-- before the first layout); cursor moves that depend on the layout tier
-- read it.
data UiState = UiState
  { usEnv :: !Env, usCursor :: !Cursor, usPane :: !Pane
  , usBack :: ![(Face, Day)], usMessage :: !(Maybe (Text, UTCTime)), usWidth :: !Int }

data Cmd
  = CmdPeriod !Int | CmdFace !Face | CmdCycleFace !Int | CmdCycleRate
  | CmdMove !Int !Int | CmdEnter | CmdEsc | CmdFilterOpen | CmdFilterChar !Char | CmdFilterBackspace
  | CmdFilterSubmit
  | CmdPickerOpen | CmdPickerChar !Char | CmdPickerBackspace | CmdPickerMove !Int | CmdPickerPick | CmdReload | CmdHelp | CmdQuit | CmdScroll !Int
  | CmdScrollEdge !Bool  -- ^ True = top (Home), False = bottom (End)
  deriving (Eq, Show)

data Effect = EffNone | EffQuit | EffReload
  deriving (Eq, Show)

initState :: Env -> UiState
initState env = UiState env defaultCursor PaneNone [] Nothing 0

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
    KChar 'c' -> Just CmdPickerOpen
    KChar 'v' -> Just CmdCycleRate
    KChar 'h' -> Just (CmdMove (-1) 0)
    KChar 'l' -> Just (CmdMove 1 0)
    KChar 'k' -> Just (CmdMove 0 (-1))
    KChar 'j' -> Just (CmdMove 0 1)
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
    KChar 'c' -> Just CmdPickerOpen
    KChar 'v' -> Just CmdCycleRate
    KChar '/' -> Just CmdFilterOpen
    KEnter -> Just CmdEsc
    KEsc -> Just CmdEsc
    _ -> Nothing
  -- every printable key edits the filter, h/j/k/l included; only the
  -- arrows move the highlight
  PanePicker {} -> case key of
    KChar ch | MCtrl `notElem` mods, MAlt `notElem` mods, ch /= '\t' -> Just (CmdPickerChar ch)
    KBS -> Just CmdPickerBackspace
    KUp -> Just (CmdPickerMove (-1))
    KDown -> Just (CmdPickerMove 1)
    KEnter -> Just CmdPickerPick
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
  CmdPickerOpen ->
    let i = fromMaybe 0 (elemIndex (sValuation s) (map peValuation (pickerEntries (usEnv us))))
    in done us { usPane = PanePicker "" i returnTo }
  CmdPickerChar c -> done (onPicker (`T.snoc` c) us)
  CmdPickerBackspace -> done (onPicker dropLast us)
  CmdPickerMove n -> done $ case usPane us of
    PanePicker f i ret ->
      let count = length (pickerMatches f (pickerEntries (usEnv us)))
      in us { usPane = PanePicker f (max 0 (min (count - 1) (i + n))) ret }
    _ -> us
  -- revalues in place and returns; with nothing matching, does nothing
  CmdPickerPick -> done $ case usPane us of
    PanePicker f i ret -> case drop i (pickerMatches f (pickerEntries (usEnv us))) of
      (e : _) -> (withSettings (\st -> st { sValuation = peValuation e }) us) { usPane = ret }
      [] -> us
    _ -> us
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
    PanePicker _ _ ret -> us { usPane = ret }
    _ -> us { usPane = PaneNone }
  CmdFilterOpen -> done us { usPane = PaneFilter (T.unwords (sQueryArgs s)) returnTo }
  CmdFilterChar c -> done (onDraft (`T.snoc` c) us)
  CmdFilterBackspace -> done (onDraft dropLast us)
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

-- * Currency picker

-- | One valuation choice in the picker; 'peDefault' marks the default base.
data PickerEntry = PickerEntry { peValuation :: !Valuation, peLabel :: !Text, peDefault :: !Bool }
  deriving (Eq, Show)

-- | Every valuation choice: the default base, the other currencies in chip
-- order, then native.
pickerEntries :: Env -> [PickerEntry]
pickerEntries env =
  [ PickerEntry (ValuedIn c) (chipLabel c) (c == sDefaultBase (envSettings env)) | c <- currencyChoices env ]
    ++ [PickerEntry Native "native" False]

-- | The entries whose code, symbol or label contains the filter, ignoring case.
pickerMatches :: Text -> [PickerEntry] -> [PickerEntry]
pickerMatches f = filter matches
  where
    needle = T.toLower f
    matches e = T.null needle || any (T.isInfixOf needle . T.toLower) (keys e)
    keys e = peLabel e : case peValuation e of
      ValuedIn c -> c : codeOf c : maybe [] pure (symbolOf c)
      Native -> []

-- | Backspace in a typed line: the last character goes, if there is one.
dropLast :: Text -> Text
dropLast t = if T.null t then t else T.init t

-- | Edit the picker's filter, keeping the highlighted entry when it still
-- matches, else highlighting the first match.
onPicker :: (Text -> Text) -> UiState -> UiState
onPicker f us = case usPane us of
  PanePicker t i ret ->
    let entries = pickerEntries (usEnv us)
        t' = f t
        was = take 1 (drop i (pickerMatches t entries))
        i' = case was of
          [e] -> fromMaybe 0 (elemIndex e (pickerMatches t' entries))
          _ -> 0
    in us { usPane = PanePicker t' i' ret }
  _ -> us

onDraft :: (Text -> Text) -> UiState -> UiState
onDraft f us = case usPane us of
  PaneFilter t ret -> us { usPane = PaneFilter (f t) ret }
  _ -> us

submitFilter :: UTCTime -> Text -> UiState -> UiState
submitFilter now draft us
  | T.null (T.strip draft) = withSettings (\st -> st { sQuery = sFlagQuery st, sQueryArgs = [] }) us
  -- the startup -p has become the focus, which a date: term replaces; so
  -- only the typed date terms must overlap one another
  | Just problem <- dateTermProblem (sToday (settingsOf us)) Nothing (T.words draft) = message now (T.pack problem) us
  | otherwise = case parseQuery (sToday (settingsOf us)) draft of
      Right (q, _) ->
        -- a date: term sets the focus period, as on the command line
        let s = settingsOf us
            dated = case queryDateSpan False q of
              DateSpan Nothing Nothing -> id
              ds -> refocus (spanFocus (sFace s) (sCommInterval s) (sToday s) (sDataRange s) ds)
        in dated $ withSettings (\st -> st
             { sQuery = simplifyQuery (And [sFlagQuery st, filterQuery (not . queryIsDateOrDate2) q])
             , sQueryArgs = dropDateArgs (sToday st) (T.words draft) }) us
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
  -- the ≥ 140 grid has a quarter per row (design F2); narrower, a month per row
  FaceYear
    | tierFor (usWidth us) == Just TierFull ->
        -- a step up or down that would leave the data range does nothing;
        -- clamping it would move the cursor diagonally
        let target = months (dx + 3 * dy) focus
            (lo, hi) = sDataRange s
            firstOf d = let (y, m, _) = toGregorian d in fromGregorian y m 1
            inside = firstOf target >= firstOf lo && firstOf target <= hi
        in if dy /= 0 && not inside then us else refocus target us
    | otherwise -> refocus (months (3 * dx + dy) focus) us
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
        cur = cmSelected m
    in us { usCursor = (usCursor us) { cuCommodity = Just (max 0 (min (n - 1) (cur + dy))) } }
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
    in case drop (cmSelected m) rows of
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
