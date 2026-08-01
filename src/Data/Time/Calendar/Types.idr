module Data.Time.Calendar.Types

import Derive.Prelude

%language ElabReflection

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
  dayofmonthvalue : (val ** (LTE 1 val, LTE val 32)) 

public export
record DayOfQuarter where
  constructor MkDayOfQuarter
  dayofquartervalue : (val ** (LTE 1 val, LTE val 92))

public export
record DayOfYear where
  constructor MkDayOfYear
  dayofyearvalue : (val ** (LTE 1 val, LTE val 366))

public export
WeekOfYear : Type
WeekOfYear = Nat
