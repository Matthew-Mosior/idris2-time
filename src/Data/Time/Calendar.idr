module Data.Time.Calendar

import Derive.Prelude

%language ElabReflection

--------------------------------------------------------------------------------
--          ModifiedJulianDay
--------------------------------------------------------------------------------

||| The modified julian day is a standard count of days, with zero being day 1858-11-17.
|||
public export
record ModifiedJulianDay where
  constructor MkModifiedJulianDay
  toModifiedJulianDay : Integer

%runElab derive "ModifiedJulianDay" [Show,Eq,Ord]

||| Add day(s) to a `ModifiedJulianDay`.
|||
export
addModifiedJulianDays : Integer -> ModifiedJulianDay -> ModifiedJulianDay
addModifiedJulianDays n (MkModifiedJulianDay a) = MkModifiedJulianDay (a + n)

||| Subtract day(s) from a `ModifiedJulianDay`.
|||
export
diffModifiedJulianDays : ModifiedJulianDay -> ModifiedJulianDay -> Integer
diffModifiedJulianDays (MkModifiedJulianDay a) (MkModifiedJulianDay b) = a - b

--------------------------------------------------------------------------------
--          CalendarDiffDays
--------------------------------------------------------------------------------

||| A difference representation of calendar days.
|||
public export
record CalendarDiffDays where
  constructor MkCalendarDiffDays
  cddmonths : Integer
  cdddays   : Integer

%runElab derive "CalendarDiffDays" [Show,Eq,Ord]

public export
Semigroup CalendarDiffDays where
  (MkCalendarDiffDays m1 d1) <+> (MkCalendarDiffDays m2 d2) = MkCalendarDiffDays (m1 + m2) (d1 + d2)

public export
Semigroup CalendarDiffDays => Monoid CalendarDiffDays where
  neutral = MkCalendarDiffDays 0 0

||| A `CalendarDiffDays` representation of a day.
|||
export
calendarDay : CalendarDiffDays
calendarDay = MkCalendarDiffDays 0 1

||| A `CalendarDiffDays` representation of a week.
|||
export
calendarWeek : CalendarDiffDays
calendarWeek = MkCalendarDiffDays 0 7

||| A `CalendarDiffDays` representation of a month.
|||
export
calendarMonth : CalendarDiffDays
calendarMonth = MkCalendarDiffDays 1 0

||| A `CalendarDiffDays` representation of a year.
|||
export
calendarYear : CalendarDiffDays
calendarYear = MkCalendarDiffDays 12 0

||| Scale by a factor.
|||
||| Note that this function will not perfectly invert a duration, due to variable month lengths.
|||
export
scaleCalendarDiffDays : Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k (MkCalendarDiffDays m d) = MkCalendarDiffDays (k * m) (k * d)

--------------------------------------------------------------------------------
--          Year
--------------------------------------------------------------------------------

||| A year represented as an `Integer`.
|||
Year : Type
Year = Integer

||| Common Era, also known as Anno Domini.
|||
commonEra : Integer -> Year
commonEra n =
  case n > 0 of
    True  =>
      Just 
    False => 
