-- | Pure calendar math: periods, month grids and focus navigation.
module Cal.Period
  ( intervalSpan, faceSpan, prevSpan, nextSpan, subSpans
  , lastDay, inSpan, spanStatus, monthGrid, isoWeek, quarterOf
  , stepFocus, clampFocus, resolveFocus
  ) where

import Cal.Types (Day, Face (..), Interval (..), Span (..), Status (..))
import Data.Time.Calendar
  ( addDays, addGregorianMonthsClip, addGregorianYearsClip, diffDays
  , fromGregorian, toGregorian )
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)

-- | The calendar year/quarter/month/ISO week containing the day.
intervalSpan :: Interval -> Day -> Span
intervalSpan IYear day =
  let (y, _, _) = toGregorian day in Span (fromGregorian y 1 1) (fromGregorian (y + 1) 1 1)
intervalSpan IQuarter day =
  let (y, m, _) = toGregorian day
      start = fromGregorian y (3 * ((m - 1) `div` 3) + 1) 1
  in Span start (addGregorianMonthsClip 3 start)
intervalSpan IMonth day =
  let (y, m, _) = toGregorian day
      start = fromGregorian y m 1
  in Span start (addGregorianMonthsClip 1 start)
intervalSpan IWeek day =
  let (y, w, _) = toWeekDate day
      start = fromWeekDate y w 1
  in Span start (addDays 7 start)

faceSpan :: Face -> Interval -> Day -> Span
faceSpan FaceYear _ = intervalSpan IYear
faceSpan FaceQuarter _ = intervalSpan IYear
faceSpan FaceMonth _ = intervalSpan IMonth
faceSpan FaceWeek _ = intervalSpan IWeek
faceSpan FaceCommodities i = intervalSpan i

prevSpan, nextSpan :: Interval -> Span -> Span
prevSpan i = intervalSpan i . addDays (-1) . spStart
nextSpan i = intervalSpan i . spEnd

-- | The interval-sized spans tiling the given span.
subSpans :: Interval -> Span -> [Span]
subSpans i (Span s e) = takeWhile ((< e) . spStart) (iterate (nextSpan i) (intervalSpan i s))

lastDay :: Span -> Day
lastDay = addDays (-1) . spEnd

inSpan :: Day -> Span -> Bool
inSpan day (Span s e) = day >= s && day < e

spanStatus :: Day -> Span -> Status
spanStatus today sp
  | lastDay sp < today = Closed
  | inSpan today sp = Open
  | otherwise = Future

-- | Monday-first weeks covering the day's month.
monthGrid :: Day -> [[Day]]
monthGrid day =
  let Span s e = intervalSpan IMonth day
      start = spStart (intervalSpan IWeek s)
      end = spEnd (intervalSpan IWeek (addDays (-1) e))
      days = [start .. addDays (-1) end]
  in chunks days
  where
    chunks [] = []
    chunks xs = let (a, b) = splitAt 7 xs in a : chunks b

-- | (ISO year, week number).
isoWeek :: Day -> (Integer, Int)
isoWeek day = let (y, w, _) = toWeekDate day in (y, w)

quarterOf :: Day -> Int
quarterOf day = let (_, m, _) = toGregorian day in (m - 1) `div` 3 + 1

-- | Move n face periods; day-of-period is clamped.
stepFocus :: Face -> Interval -> Int -> Day -> Day
stepFocus face interval n day = case face of
  FaceYear -> years
  FaceQuarter -> years
  FaceMonth -> months n
  FaceWeek -> addDays (7 * fromIntegral n) day
  FaceCommodities -> case interval of
    IYear -> years
    IQuarter -> months (3 * n)
    IMonth -> months n
    IWeek -> addDays (7 * fromIntegral n) day
  where
    years = addGregorianYearsClip (fromIntegral n) day
    months k = addGregorianMonthsClip (fromIntegral k) day

clampFocus :: (Day, Day) -> Day -> Day
clampFocus (lo, hi) = max lo . min hi

-- | Initial focus: an explicit span at least as wide as the face span gives
-- today if inside it, else its start; a narrower one gives its start.
resolveFocus :: Face -> Interval -> Day -> Maybe Span -> Day
resolveFocus _ _ today Nothing = today
resolveFocus face interval today (Just sp)
  | width sp >= width (faceSpan face interval (spStart sp)) =
      if inSpan today sp then today else spStart sp
  | otherwise = spStart sp
  where width (Span s e) = diffDays e s
