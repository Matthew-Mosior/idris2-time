module Data.Time.Calendar.Week

import Data.Finite
import Data.Time.Calendar.Days
import Derive.Prelude

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          DayOfWeek
--------------------------------------------------------------------------------

public export
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

%runElab derive "DayOfWeek" [Show,Eq,Ord]

public export
Finite DayOfWeek where
  values =
    [ Monday
    , Tuesday
    , Wednesday
    , Thursday
    , Friday
    , Saturday
    , Sunday
    ]

||| Convert a day of the week to its 1-based numeric representation.
|||
||| Monday = 1, ..., Sunday = 7.
|||
public export
dayOfWeekNumber : DayOfWeek -> Integer
dayOfWeekNumber Monday    = 1
dayOfWeekNumber Tuesday   = 2
dayOfWeekNumber Wednesday = 3
dayOfWeekNumber Thursday  = 4
dayOfWeekNumber Friday    = 5
dayOfWeekNumber Saturday  = 6
dayOfWeekNumber Sunday    = 7

||| Convert an integer to a day of the week, cycling every seven days.
|||
public export
dayOfWeekFromNumber : Integer -> DayOfWeek
dayOfWeekFromNumber i =
  case mod i 7 of
    0 => Sunday
    1 => Monday
    2 => Tuesday
    3 => Wednesday
    4 => Thursday
    5 => Friday
    _ => Saturday

--------------------------------------------------------------------------------
--          Range
--------------------------------------------------------------------------------

public export covering
Range DayOfWeek where
  rangeFromTo wd1 wd2 =
    if wd1 == wd2
       then [wd1]
       else wd1 :: rangeFromTo (succ wd1) wd2
    where
      succ : DayOfWeek -> DayOfWeek
      succ Monday    = Tuesday
      succ Tuesday   = Wednesday
      succ Wednesday = Thursday
      succ Thursday  = Friday
      succ Friday    = Saturday
      succ Saturday  = Sunday
      succ Sunday    = Monday
  rangeFromThenTo wd1 wd2 wd3 =
    if wd2 == wd3
       then [wd1, wd2]
       else wd1 :: rangeFromThenTo
         wd2
         (dayOfWeekFromNumber $
            2 * dayOfWeekNumber wd2 - dayOfWeekNumber wd1)
         wd3
  rangeFrom wd =
    countFrom wd succ
    where
      succ : DayOfWeek -> DayOfWeek
      succ Monday    = Tuesday
      succ Tuesday   = Wednesday
      succ Wednesday = Thursday
      succ Thursday  = Friday
      succ Friday    = Saturday
      succ Saturday  = Sunday
      succ Sunday    = Monday
  rangeFromThen wd1 wd2 =
    countFrom wd1 $ \wd =>
      dayOfWeekFromNumber $
        2 * dayOfWeekNumber wd - dayOfWeekNumber wd1

--------------------------------------------------------------------------------
--          Day of Week
--------------------------------------------------------------------------------

||| Get the day of the week for a modified Julian day.
|||
export
dayOfWeek : Day -> DayOfWeek
dayOfWeek (MkModifiedJulianDay d) =
  dayOfWeekFromNumber (d + 3)

||| @dayOfWeekDiff a b = a - b@ in the range 0 to 6.
|||
||| The number of days from `b` to the next `a`.
|||
export
dayOfWeekDiff : DayOfWeek -> DayOfWeek -> Integer
dayOfWeekDiff a b =
  mod (dayOfWeekNumber a - dayOfWeekNumber b) 7

||| The first day-of-week on or after some day.
|||
export
firstDayOfWeekOnAfter : DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d =
  addDays (dayOfWeekDiff dw (dayOfWeek d)) d

||| Returns the first day of a week containing the given `Day`.
|||
export
weekFirstDay : DayOfWeek -> Day -> Day
weekFirstDay firstDay day =
  addDays (-7) $
    firstDayOfWeekOnAfter firstDay (addDays 1 day)

||| Returns the last day of a week containing the given `Day`.
|||
export
weekLastDay : DayOfWeek -> Day -> Day
weekLastDay firstDay day =
  addDays (-1) $
    firstDayOfWeekOnAfter firstDay (addDays 1 day)

||| Returns a week containing the given `Day` where the first day is the
||| `DayOfWeek` specified.
|||
export
weekAllDays : DayOfWeek -> Day -> List Day
weekAllDays firstDay day =
  [weekFirstDay firstDay day .. weekLastDay firstDay day]
