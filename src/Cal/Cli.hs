-- | Command-line options (hledger-style) and their resolution into 'Settings',
-- plus the echoed command line shown at the top of each view.
module Cal.Cli
  ( calMode
  , parseCli
  , mkSettings
  , commandLine
  , loadInputOpts
  , spanFocus
  , dropDateArgs
  , dateTermProblem
  ) where

import Cal.Journal (firstSeenCommodity, journalDataRange)
import Cal.Period (isoWeek, quarterOf, resolveFocus)
import Cal.Types
import Control.Applicative ((<|>))
import Data.List (tails)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, addGregorianMonthsClip, addGregorianYearsClip, toGregorian)
import Data.Time.Calendar.WeekDate (fromWeekDate)
import Hledger.Cli
  ( CliOpts (..), Mode, YNA (..), argsFlag, flagNone, flagReq, getHledgerCliOpts'
  , helpflags, hledgerCommandMode, inputflags, reportflags )
import Hledger.Data.Dates (fromEFDay, parsePeriodExpr, spanIntersect)
import Hledger.Data.Period (periodAsDateSpan)
import Hledger.Data.RawOptions
  (RawOpts, boolopt, collectopts, listofstringopt, maybestringopt, maybeposintopt, setboolopt, setopt)
import qualified Hledger.Data.Types as H
import Hledger.Data.Valuation (ConversionOp (..), ValuationType (..))
import Hledger.Query (filterQuery, parseQuery, queryDateSpan, queryIsDate, queryIsDateOrDate2)
import Hledger.Read.InputOptions (InputOpts (..))
import Hledger.Reports.ReportOptions (ReportOpts (..), ReportSpec (..), queryFromFlags, rawOptsToReportSpec)
import Safe (headMay, lastMay)
import System.FilePath (takeFileName)

calHelp :: String
calHelp = unlines
  [ "cal"
  , ""
  , "Show income and expenses as a calendar (year, quarter, month, week)"
  , "or as a commodities overview. Valued in the base currency by default."
  , ""
  , "Flags:"
  , "no command-specific flags"
  ]

calMode :: Mode RawOpts
calMode = hledgerCommandMode calHelp
  [ flagNone ["commodities"] (setboolopt "commodities") "show the commodities view"
  , flagNone ["native"] (setboolopt "native") "show native amounts, without valuation"
  , flagNone ["no-infer-market-prices"] (setboolopt "no-infer-market-prices")
      "don't infer market prices from transaction costs"
  , flagReq ["stale-days"] (\s o -> Right (setopt "stale-days" s o)) "N"
      "mark rates older than N days as stale (default: 30)"
  , flagNone ["print"] (setboolopt "print") "print the view once instead of starting the TUI"
  , flagReq ["width"] (\s o -> Right (setopt "width" s o)) "N"
      "print width (default: terminal width, else 140)"
  , flagNone ["ascii"] (setboolopt "ascii") "use ASCII glyphs only"
  ]
  [("General flags", inputflags ++ reportflags ++ helpflags)]
  []
  ([], Just (argsFlag "[QUERY]"))

parseCli :: [String] -> IO CliOpts
parseCli = getHledgerCliOpts' calMode

-- | Resolve parsed options against today, the terminal and the loaded journal.
-- The report spec is rebuilt from the raw options with the given day so
-- relative dates resolve deterministically.
mkSettings :: Day -> Terminal -> CliOpts -> H.Journal -> Either String Settings
mkSettings today term opts j = do
  let raw = rawopts_ opts
  rspec <- rawOptsToReportSpec today False raw
  let ropts = _rsReportOpts rspec
      native = boolopt "native" raw
      value = value_ ropts
  case (native, value) of
    (True, Just _) -> Left "--native can't be combined with -X, -V or --value"
    _ -> pure ()
  case unsupportedFlags today opts ropts of
    (problem : _) -> Left problem
    [] -> pure ()
  let args = map T.pack (listofstringopt "args" raw)
  maybe (pure ()) Left (dateTermProblem today (periodFlags raw (period_ ropts)) args)
  let queryArgs = dropDateArgs today args
      -- The focus period drives dates, so date terms are dropped (a date:
      -- term sets the focus, below); status (-U -P -C) and -R stay, as in
      -- hledger's own reports.
      noDates = filterQuery (not . queryIsDateOrDate2)
      query = noDates (_rsQuery rspec)
      flagQuery = noDates (queryFromFlags ropts)
      flagArgs =
        [ fl | (stt, fl) <- [(H.Unmarked, "-U"), (H.Pending, "-P"), (H.Cleared, "-C")], stt `elem` statuses_ ropts ]
          ++ ["-R" | real_ ropts]
      interval = interval_ ropts
      commodities = boolopt "commodities" raw
      -- --commodities wins; its interval then comes from -Y/-Q/-M/-W.
      face
        | commodities = FaceCommodities
        | otherwise = case interval of
            H.Years 1 -> FaceYear
            H.Quarters 1 -> FaceQuarter
            H.Weeks 1 -> FaceWeek
            _ -> FaceMonth
      commInterval = case interval of
        H.Years 1 -> IYear
        H.Quarters 1 -> IQuarter
        H.Weeks 1 -> IWeek
        _ -> IMonth
      dataRange = journalDataRange today j
      -- hledger's report span: -p/-b/-e and date: terms, intersected
      focus = spanFocus face commInterval today dataRange (queryDateSpan False (_rsQuery rspec))
      -- hledger keeps only the last of -X/-V/--value; a commodity given by an
      -- earlier -X still applies to a later --value=TYPE without one.
      lastX = T.pack <$> lastMay (listofstringopt "X" raw)
      (rateAt, givenBase) = case value of
        Just (AtEnd mc) -> (AtPeriodEnd, orX mc)
        Just (AtThen mc) -> (AtTxnDate, orX mc)
        Just (AtNow mc) -> (AtToday, orX mc)
        Just (AtDate _ mc) -> (AtPeriodEnd, orX mc)
        Nothing -> (AtPeriodEnd, Nothing)
        where orX mc = maybe lastX Just mc
      (defaultBase, baseSource) = case givenBase of
        Just c -> (c, BaseFromX)
        Nothing -> case H.jparsedefaultcommodity j of
          Just (c, _) -> (c, BaseFromD)
          Nothing -> (fromMaybe "" (firstSeenCommodity j), BaseFirstSeen)
      valuation
        | native || T.null defaultBase = Native
        | otherwise = ValuedIn defaultBase
      width = fromMaybe 140 (maybeposintopt "width" raw <|> tColumns term)
      output
        | boolopt "print" raw || not (tIsTty term) = OutPrint width
        | otherwise = OutTui
      glyphs
        | boolopt "ascii" raw || not (tUtf8 term) = GlyphsAscii
        | otherwise = GlyphsUtf8
      -- --color is hledger's YNA option: always/yes, never/no, auto (default).
      color
        | coloropt_ opts == Just No || tNoColorEnv term = ColorNone
        | coloropt_ opts /= Just Yes && not (tIsTty term) = ColorNone
        | tTrueColor term = ColorTrue
        | otherwise = Color256
      journalFile = fromMaybe (maybe "" fst (headMay (H.jfiles j))) (headMay (file_ opts))
  pure Settings
    { sFace = face, sCommInterval = commInterval, sFocus = focus, sDataRange = dataRange
    , sValuation = valuation, sRateAt = rateAt
    , sDefaultBase = defaultBase, sBaseSource = baseSource
    , sQueryArgs = queryArgs, sQuery = query, sFlagQuery = flagQuery, sFlagArgs = flagArgs
    , sToday = today
    , sStaleDays = maybe 30 fromIntegral (maybeposintopt "stale-days" raw)
    , sForecast = isJust (forecast_ (inputopts_ opts))
    , sInferPrices = not (boolopt "no-infer-market-prices" raw)
    , sJournalFile = journalFile, sOutput = output, sGlyphSet = glyphs, sColor = color }

-- | Report flags that have no calendar meaning and would otherwise be
-- silently ignored, named as they were given on the command line.
unsupportedFlags :: Day -> CliOpts -> ReportOpts -> [String]
unsupportedFlags today opts ropts =
  maybe [] pure (intervalProblem today raw (interval_ ropts))
    ++ [ name ++ meaningless | (name, given) <-
      [ ("--date2", date2_ ropts)
      , ("-E/--empty", empty_ ropts)
      , ("--depth", depth_ ropts /= mempty)
      , ("-B/--cost", conversionop_ ropts == Just ToCost)
      , ("--pretty", isJust (maybestringopt "pretty" raw))
      , ("--pivot", not (null (pivot_ (inputopts_ opts)))) ]
    , given ]
  where raw = rawopts_ opts

meaningless :: String
meaningless = " has no meaning in hledger cal"

-- | An interval with no face (daily, every 2 weeks, ...), named by the option
-- that set it: hledger keeps the last of -D/-W/-M/-Q/-Y and a -p with an
-- interval.
intervalProblem :: Day -> RawOpts -> H.Interval -> Maybe String
intervalProblem today raw ivl
  | ivl `elem` [H.NoInterval, H.Years 1, H.Quarters 1, H.Months 1, H.Weeks 1] = Nothing
  | otherwise = Just $ case lastMay (collectopts source raw) of
      Just (Just p) -> "the interval in -p \"" ++ p ++ "\"" ++ meaningless ++ "; use -Y, -Q, -M or -W"
      _ -> "-D/--daily" ++ meaningless
  where
    source (n, v)
      | n == "period" = case parsePeriodExpr today (T.pack v) of
          Right (i, _) | i /= H.NoInterval -> Just (Just v)
          _ -> Nothing
      | n `elem` ["daily", "weekly", "monthly", "quarterly", "yearly"] = Just Nothing
      | otherwise = Nothing

-- | The input options to load the journal with. hledger bounds a
-- @--forecast@ without its own span by the report period (@-p@), which would
-- leave the periods around it (\"vs previous\", @[@ @]@) without forecast
-- postings. So the report span is widened to the navigable range: forecasting
-- starts after the journal's last transaction, as without @-p@, and runs to
-- the later of today + 180 days and one period past the @-p@ end.
loadInputOpts :: Day -> CliOpts -> InputOpts
loadInputOpts today opts = case (forecast_ io, reportspan_ io) of
  (Just _, H.DateSpan ms me) | isJust ms || isJust me ->
    io { reportspan_ = H.DateSpan Nothing (H.Exact . widen . fromEFDay <$> me) }
  _ -> io
  where
    io = inputopts_ opts
    rs' = reportspec_ opts
    widen e = max (addDays 180 today) (next e)
    next e = case interval_ (_rsReportOpts rs') of
      H.Years _ -> addGregorianYearsClip 1 e
      H.Quarters _ -> addGregorianMonthsClip 3 e
      H.Weeks _ -> addDays 7 e
      _ -> addGregorianMonthsClip 1 e

-- | The focus a report date span gives the face (spec §4): today when the
-- span is open at both ends.
spanFocus :: Face -> Interval -> Day -> (Day, Day) -> H.DateSpan -> Day
spanFocus face ivl today range ds = resolveFocus face ivl today (explicitSpan range ds)

-- | Query args without the pure date terms (@date:2026-10@), which set the
-- focus period instead of filtering.
dropDateArgs :: Day -> [Text] -> [Text]
dropDateArgs today = filter (not . isDateTerm)
  where
    isDateTerm a = case parseQuery today a of
      Right (q, _) -> queryIsDateOrDate2 q
      Left _ -> False

-- | The -p/-b/-e flags as typed, and the one span hledger makes of them.
periodFlags :: RawOpts -> H.Period -> Maybe (Text, H.DateSpan)
periodFlags raw p = case collectopts named raw of
  [] -> Nothing
  given -> Just (T.unwords given, periodAsDateSpan p)
  where
    named (n, v) = (\flag -> flag <> " " <> quote (T.pack v)) <$> lookup n [("period", "-p"), ("begin", "-b"), ("end", "-e")]

-- | A problem with the date terms of a query, given the -p/-b/-e span (as
-- typed) when there is one: pieces of the report span that don't overlap,
-- named as typed, so hledger's span would be empty.
dateTermProblem :: Day -> Maybe (Text, H.DateSpan) -> [Text] -> Maybe String
dateTermProblem today flags args = listToMaybe
  [ T.unpack a <> " and " <> T.unpack b <> " do not overlap"
  | ((a, sa) : rest) <- tails (maybe [] pure flags ++ mapMaybe dateTerm args)
  , (b, sb) <- rest
  , isEmptySpan (spanIntersect sa sb) ]
  where
    dateTerm a = case parseQuery today a of
      Right (q, _) | queryIsDate q -> Just (a, queryDateSpan False q)
      _ -> Nothing
    isEmptySpan ds = case ds of
      H.DateSpan (Just s) (Just e) -> fromEFDay e <= fromEFDay s
      _ -> False

-- | An explicit report span. An open end is closed at the data range (start
-- of data, day after the later of today and the last posting).
explicitSpan :: (Day, Day) -> H.DateSpan -> Maybe Span
explicitSpan (lo, hi) ds = case ds of
  H.DateSpan Nothing Nothing -> Nothing
  H.DateSpan ms me ->
    let s = maybe (min lo (maybe lo (addDays (-1) . fromEFDay) me)) fromEFDay ms
        e = maybe (max (addDays 1 hi) (addDays 1 s)) fromEFDay me
    in Just (Span s e)

-- | The command reproducing the current view, without the leading @"$ "@.
commandLine :: Settings -> Text
commandLine s = T.unwords $
  ["hledger", "cal"] ++ faceFlags ++ ["-p", period] ++ base ++ value ++ native ++ sFlagArgs s
    ++ ["-f", T.pack (takeFileName (sJournalFile s))] ++ map quote (sQueryArgs s)
  where
    focus = sFocus s
    faceFlags = case sFace s of
      FaceYear -> ["-Y"]
      FaceQuarter -> ["-Q"]
      FaceMonth -> ["-M"]
      FaceWeek -> ["-W"]
      FaceCommodities -> "--commodities" : case sCommInterval s of
        IYear -> ["-Y"]
        IQuarter -> ["-Q"]
        IMonth -> []
        IWeek -> ["-W"]
    periodIn i = case i of
      IYear -> T.pack (show y)
      IQuarter -> T.pack (show y ++ "q" ++ show (quarterOf focus))
      IMonth -> T.pack (show y ++ "-" ++ pad2 m)
      IWeek -> let (wy, w) = isoWeek focus in T.pack (show (fromWeekDate wy w 1))
      where (y, m, _) = toGregorian focus
    period = case sFace s of
      FaceYear -> periodIn IYear
      FaceQuarter -> periodIn IYear
      FaceMonth -> periodIn IMonth
      FaceWeek -> periodIn IWeek
      FaceCommodities -> periodIn (sCommInterval s)
    -- With -X the journal-derived default is unknown, so keep the flag.
    base = case sValuation s of
      ValuedIn c | c /= sDefaultBase s || sBaseSource s == BaseFromX -> ["-X", quote c]
      _ -> []
    value = case (sValuation s, sRateAt s) of
      (ValuedIn _, AtTxnDate) -> ["--value=then"]
      (ValuedIn _, AtToday) -> ["--value=now"]
      _ -> []
    native = case sValuation s of
      Native | not (T.null (sDefaultBase s)) -> ["--native"]
      _ -> []
    pad2 n = (if n < 10 then "0" else "") ++ show n

-- | A word of the echoed command line, quoted when it holds a space.
quote :: Text -> Text
quote t = if T.any (== ' ') t then "'" <> t <> "'" else t
