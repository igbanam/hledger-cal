module Cal.PeriodTest where

import Cal.Period
import Cal.Types
import Data.Time.Calendar (fromGregorian)
import Support (fixtureToday)
import Test.Tasty.HUnit

d :: Integer -> Int -> Int -> Day
d = fromGregorian

today :: Day
today = fixtureToday

firstDay :: [[Day]] -> Day
firstDay ((x : _) : _) = x
firstDay _ = error "empty grid"

unit_monthSpan :: Assertion
unit_monthSpan = faceSpan FaceMonth IMonth (d 2026 10 15) @?= Span (d 2026 10 1) (d 2026 11 1)

unit_quarterFaceIsYear :: Assertion
unit_quarterFaceIsYear = faceSpan FaceQuarter IMonth (d 2026 5 5) @?= Span (d 2026 1 1) (d 2027 1 1)

unit_weekSpan :: Assertion
unit_weekSpan = faceSpan FaceWeek IMonth (d 2026 10 28) @?= Span (d 2026 10 26) (d 2026 11 2)

unit_week53 :: Assertion
unit_week53 = (isoWeek (d 2026 12 30), faceSpan FaceWeek IMonth (d 2026 12 30))
  @?= ((2026, 53), Span (d 2026 12 28) (d 2027 1 4))

unit_commQuarter :: Assertion
unit_commQuarter = faceSpan FaceCommodities IQuarter today @?= Span (d 2026 10 1) (d 2027 1 1)

unit_gridOct :: Assertion
unit_gridOct = let g = monthGrid (d 2026 10 1) in (length g, firstDay g, last (last g)) @?= (5, d 2026 9 28, d 2026 11 1)

unit_gridAug6 :: Assertion
unit_gridAug6 = let g = monthGrid (d 2026 8 1) in (length g, firstDay g, last (last g)) @?= (6, d 2026 7 27, d 2026 9 6)

unit_gridLeap :: Assertion
unit_gridLeap = assertBool "29 Feb" (d 2028 2 29 `elem` concat (monthGrid (d 2028 2 1)))

unit_status :: Assertion
unit_status = map (spanStatus today . intervalSpan IMonth) [d 2026 10 1, d 2026 11 1, d 2026 12 1] @?= [Closed, Open, Future]

unit_stepClamp :: Assertion
unit_stepClamp = stepFocus FaceMonth IMonth 1 (d 2026 10 31) @?= d 2026 11 30

unit_stepYear :: Assertion
unit_stepYear = stepFocus FaceYear IMonth (-1) today @?= d 2025 11 6

unit_clamp :: Assertion
unit_clamp = clampFocus (d 2026 1 1, today) (d 2026 12 6) @?= today

unit_resolveNone :: Assertion
unit_resolveNone = resolveFocus FaceMonth IMonth today Nothing @?= today

unit_resolveWide :: Assertion
unit_resolveWide =
  ( resolveFocus FaceMonth IMonth today (Just (Span (d 2025 1 1) (d 2026 1 1)))
  , resolveFocus FaceMonth IMonth today (Just (Span (d 2026 1 1) (d 2027 1 1))) )
  @?= (d 2025 1 1, today)

unit_resolveNarrow :: Assertion
unit_resolveNarrow = resolveFocus FaceYear IMonth today (Just (Span (d 2026 10 1) (d 2026 11 1))) @?= d 2026 10 1

unit_subSpans :: Assertion
unit_subSpans = map spStart (subSpans IMonth (Span (d 2026 1 1) (d 2027 1 1))) @?= [d 2026 m 1 | m <- [1 .. 12]]
