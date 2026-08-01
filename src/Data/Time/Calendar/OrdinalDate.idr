module Data.Time.Calendar.OrdinalDate

import Data.Time.Calendar.Days
import Data.Time.Calendar.Internal
import Data.Time.Calendar.Week
import Data.Time.Calendar.Types

%default total

--------------------------------------------------------------------------------
--          Helpers
--------------------------------------------------------------------------------

||| Modulo operation whose result is always non-negative when the divisor is positive.
|||
||| This corresponds to the behavior needed by the Haskell calendar implementation for calculations involving dates before the epoch.
|||
public export
mod' : Integer -> Integer -> Integer
mod' x y =
  let r = mod x y
    in if r < 0 then r + abs y else r

--------------------------------------------------------------------------------
--          Leap Years
--------------------------------------------------------------------------------

||| Is this year a leap year according to the proleptic Gregorian calendar?
|||
public export
isLeapYear : Year -> Bool
isLeapYear year =
  mod year 4 == 0 &&
  (mod year 400 == 0 || mod year 100 /= 0)

--------------------------------------------------------------------------------
--          ISO 8601 Ordinal Dates
--------------------------------------------------------------------------------

||| Convert a `Day` to ISO 8601 Ordinal Date format.
|||
||| Returns the Gregorian year and the 1-based day of year.
public export
toOrdinalDate : Day -> (Year, DayOfYear)
toOrdinalDate (MkModifiedJulianDay mjd) =
  let
    a       = mjd + 678575
    quadcent = div a 146097
    b       = mod a 146097
    cent    = min (div b 36524) 3
    c       = b - (cent * 36524)
    quad    = div c 1461
    d       = mod c 1461
    y       = min (div d 365) 3
    yd      = cast (d - (y * 365) + 1)
    year    = quadcent * 400 + cent * 100 + quad * 4 + y + 1
  in
    (year, yd)

||| Convert from ISO 8601 Ordinal Date format.
|||
||| Invalid day numbers are clipped to the valid range:
||| 1 to 365 in a common year, or 1 to 366 in a leap year.
public export
fromOrdinalDate : Year -> DayOfYear -> Day
fromOrdinalDate year day =
  let
    maxDay : DayOfYear
    maxDay =
      if isLeapYear year
         then 366
         else 365

    clippedDay : DayOfYear
    clippedDay = clip 1 maxDay day

    y   = year - 1
    mjd = cast clippedDay
          + (365 * y)
          + div y 4
          - div y 100
          + div y 400
          - 678576
  in
    MkModifiedJulianDay mjd

||| Convert from ISO 8601 Ordinal Date format.
|||
||| Invalid day numbers return `Nothing`.
public export
fromOrdinalDateValid : Year -> DayOfYear -> Maybe Day
fromOrdinalDateValid year day = do
  let
    maxDay : DayOfYear
    maxDay =
      if isLeapYear year
         then 366
         else 365

  day' <- clipValid 1 maxDay day

  let
    y   = year - 1
    mjd = cast day'
          + (365 * y)
          + div y 4
          - div y 100
          + div y 400
          - 678576

  pure $ MkModifiedJulianDay mjd

--------------------------------------------------------------------------------
--          Bidirectional Year / Day Constructor
--------------------------------------------------------------------------------

||| Construct a `Day` from a Gregorian year and ordinal day.
|||
||| This is the Idris2 equivalent of the Haskell `YearDay` pattern
||| constructor. Invalid day numbers are clipped.
public export
yearDay : Year -> DayOfYear -> Day
yearDay = fromOrdinalDate

||| Deconstruct a `Day` into its Gregorian year and ordinal day.
|||
||| This is the Idris2 equivalent of matching against the Haskell
||| `YearDay` pattern.
public export
unYearDay : Day -> (Year, DayOfYear)
unYearDay = toOrdinalDate

--------------------------------------------------------------------------------
--          Formatting
--------------------------------------------------------------------------------

||| Show a `Day` in ISO 8601 Ordinal Date format (`yyyy-ddd`).
public export
showOrdinalDate : Day -> String
showOrdinalDate date =
  let
    (y, d) = toOrdinalDate date
  in
    show4 y ++ "-" ++ show3 d

--------------------------------------------------------------------------------
--          Monday-Starting Weeks
--------------------------------------------------------------------------------

||| Get the number of the Monday-starting week in the year and the day
||| of the week.
|||
||| The first Monday is the first day of week 1. Days before the first
||| Monday are in week 0.
|||
||| The returned day-of-week value uses:
|||
||| * Monday = 1
||| * ...
||| * Sunday = 7
public export
mondayStartWeek : Day -> (WeekOfYear, Nat)
mondayStartWeek date =
  let
    (_, yd) = toOrdinalDate date
    d       = toModifiedJulianDay date + 2
    k       = d - cast yd
    week    = div d 7 - div k 7
    weekday = cast (mod' d 7) + 1
  in
    (cast week, weekday)

--------------------------------------------------------------------------------
--          Sunday-Starting Weeks
--------------------------------------------------------------------------------

||| Get the number of the Sunday-starting week in the year and the day
||| of the week.
|||
||| The first Sunday is the first day of week 1. Days before the first
||| Sunday are in week 0.
|||
||| The returned day-of-week value uses:
|||
||| * Sunday = 0
||| * ...
||| * Saturday = 6
public export
sundayStartWeek : Day -> (WeekOfYear, Nat)
sundayStartWeek date =
  let
    (_, yd) = toOrdinalDate date
    d       = toModifiedJulianDay date + 3
    k       = d - cast yd
    week    = div d 7 - div k 7
    weekday = cast (mod' d 7)
  in
    (cast week, weekday)

--------------------------------------------------------------------------------
--          Inverse: Monday-Starting Weeks
--------------------------------------------------------------------------------

||| Get a `Day` from a year, Monday-starting week number, and day of week.
|||
||| Monday is 1 and Sunday is 7.
|||
||| Invalid values are not validated and may produce a date outside the
||| requested year.
public export
fromMondayStartWeek : Year -> WeekOfYear -> Nat -> Day
fromMondayStartWeek year w d =
  let
    -- First day of the year.
    firstDay = fromOrdinalDate year 1

    -- 0-based day-of-year of the first Monday.
    zbFirstMonday =
      mod' (5 - toModifiedJulianDay firstDay) 7

    -- 0-based week number.
    zbWeek =
      cast w - 1

    -- 0-based day of week.
    zbDay =
      cast d - 1

    -- 0-based day of year.
    zbYearDay =
      zbFirstMonday
      + 7 * zbWeek
      + zbDay
  in
    addDays zbYearDay firstDay

||| Validating inverse of `mondayStartWeek`.
|||
||| Returns `Nothing` when the day-of-week or resulting day-of-year
||| is outside the valid range.
public export
fromMondayStartWeekValid : Year -> WeekOfYear -> Nat -> Maybe Day
fromMondayStartWeekValid year w d = do
  d' <- clipValid 1 7 d

  let
    firstDay = fromOrdinalDate year 1

    zbFirstMonday =
      mod' (5 - toModifiedJulianDay firstDay) 7

    zbWeek =
      cast w - 1

    zbDay =
      cast d' - 1

    zbYearDay =
      zbFirstMonday
      + 7 * zbWeek
      + zbDay

    maxYearDay =
      if isLeapYear year
         then 365
         else 364

  zbYearDay' <- clipValid 0 maxYearDay (cast zbYearDay)

  pure $ addDays (cast zbYearDay') firstDay

--------------------------------------------------------------------------------
--          Inverse: Sunday-Starting Weeks
--------------------------------------------------------------------------------

||| Get a `Day` from a year, Sunday-starting week number, and day of week.
|||
||| Sunday is 0 and Saturday is 6.
|||
||| Invalid values are not validated and may produce a date outside the
||| requested year.
public export
fromSundayStartWeek : Year -> WeekOfYear -> Nat -> Day
fromSundayStartWeek year w d =
  let
    -- First day of the year.
    firstDay = fromOrdinalDate year 1

    -- 0-based day-of-year of the first Sunday.
    zbFirstSunday =
      mod' (4 - toModifiedJulianDay firstDay) 7

    -- 0-based week number.
    zbWeek =
      cast w - 1

    -- 0-based day of week.
    zbDay =
      cast d

    -- 0-based day of year.
    zbYearDay =
      zbFirstSunday
      + 7 * zbWeek
      + zbDay
  in
    addDays zbYearDay firstDay

||| Validating inverse of `sundayStartWeek`.
|||
||| Returns `Nothing` when the day-of-week or resulting day-of-year
||| is outside the valid range.
|||
public export
fromSundayStartWeekValid : Year -> WeekOfYear -> Nat -> Maybe Day
fromSundayStartWeekValid year w d = do
  d' <- clipValid 0 6 d
  let
    firstDay = fromOrdinalDate year 1
    zbFirstSunday =
      mod' (4 - toModifiedJulianDay firstDay) 7
    zbWeek =
      cast w - 1
    zbDay =
      cast d'
    zbYearDay =
      zbFirstSunday
      + 7 * zbWeek
      + zbDay
    maxYearDay =
      if isLeapYear year
         then 365
         else 364
  zbYearDay' <- clipValid 0 maxYearDay (cast zbYearDay)
  pure $ addDays (cast zbYearDay') firstDay
