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
--          Year, Month and Day
--------------------------------------------------------------------------------

||| A year represented as an `Integer`.
|||
public export
Year : Type
Year = Integer

||| An era, either CE or BCE.
|||
public export
data Era
  = CommonEra Year
  | BeforeCommonEra Year

||| Convert a `Year` into an `Era`.
|||
export
fromYear : Year -> Era
fromYear y =
  case y > 0 of
    True  =>
      CommonEra y
    False =>
      BeforeCommonEra (1 - y)

||| Convert an `Era` into a `Year`.
|||
export
toYear : Era -> Year
toYear (CommonEra n)       =
  n
toYear (BeforeCommonEra n) =
  1 - n

||| Display a `Year` containing its `Era` representation.
|||
export
describe : Year -> String
describe y =
  case fromYear y of
    CommonEra n       =>
      show n ++ " CE"
    BeforeCommonEra n =>
      show n ++ " BCE"

public export
data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December

export
monthName : Month -> String
monthName January  = "January"
monthName February = "February"
monthName March    = "March"
monthName April    = "April"
monthName May      = "May"
monthName June     = "June"
monthName July     = "July"
monthName August   = "August"
monthName September = "September"
monthName October  = "October"
monthName November = "November"
monthName December = "December"

export
monthToInt : Month -> Int
monthToInt January   = 1
monthToInt February  = 2
monthToInt March     = 3
monthToInt April     = 4
monthToInt May       = 5
monthToInt June      = 6
monthToInt July      = 7
monthToInt August    = 8
monthToInt September = 9
monthToInt October   = 10
monthToInt November  = 11
monthToInt December  = 12

export
intToMonth : Int -> Maybe Month
intToMonth 1  = Just January
intToMonth 2  = Just February
intToMonth 3  = Just March
intToMonth 4  = Just April
intToMonth 5  = Just May
intToMonth 6  = Just June
intToMonth 7  = Just July
intToMonth 8  = Just August
intToMonth 9  = Just September
intToMonth 10 = Just October
intToMonth 11 = Just November
intToMonth 12 = Just December
intToMonth _  = Nothing

public export
record DayOfMonth where
  constructor MkDayOfMonth
  value : Nat
  0 validLower : 1 <= value
  1 validUpper : value <= 31

public export
record DayOfQuarter where
  constructor MkDayOfQuarter
  value : Nat
  0 validLower : 1 <= value
  1 validUpper : value <= 92

public export
record DayOfYear where
  constructor MkDayOfYear
  value : Nat
  0 validLower : 1 <= value
  1 validUpper : value <= 366

public export
WeekOfYear : Type
WeekOfYear = Nat
