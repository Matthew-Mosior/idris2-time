module Data.Time.Calendar.Days

import Data.Time.Calendar.Internal

import Derive.Finite
import Derive.Prelude

%default total
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

public export
Range ModifiedJulianDay where
  rangeFromTo (MkModifiedJulianDay a) (MkModifiedJulianDay b) =
    map MkModifiedJulianDay (rangeFromTo a b)
  rangeFromThenTo (MkModifiedJulianDay a)
                  (MkModifiedJulianDay b)
                  (MkModifiedJulianDay c) =
    map MkModifiedJulianDay (rangeFromThenTo a b c)
  rangeFrom (MkModifiedJulianDay a) =
    map MkModifiedJulianDay (rangeFrom a)
  rangeFromThen (MkModifiedJulianDay a) (MkModifiedJulianDay b) =
    map MkModifiedJulianDay (rangeFromThen a b)

||| Type alias for `ModifiedJulianDay`.
|||
public export
Day : Type
Day = ModifiedJulianDay

||| Add day(s) to a `ModifiedJulianDay`.
|||
export
addDays : Integer -> Day -> Day
addDays n (MkModifiedJulianDay a) = MkModifiedJulianDay (a + n)

||| Subtract day(s) from a `ModifiedJulianDay`.
|||
export
diffDays : Day -> Day -> Integer
diffDays (MkModifiedJulianDay a) (MkModifiedJulianDay b) = a - b

--------------------------------------------------------------------------------
--          DayPeriod
--------------------------------------------------------------------------------

public export
interface Ord p => DayPeriod p where
  periodFirstDay : p -> ModifiedJulianDay
  periodLastDay  : p -> ModifiedJulianDay
  dayPeriod      : ModifiedJulianDay -> p  

||| A list of all the days in this period.
|||
export
periodAllDays : DayPeriod p => p -> List Day
periodAllDays p = [periodFirstDay p .. periodLastDay p]

||| Test whether a day is in a given period.
|||
export
periodIn : DayPeriod p => p -> Day -> Bool
periodIn p d = (d >= periodFirstDay p) && (d <= periodLastDay p)

||| The number of days in this period.
|||
export
periodLength : DayPeriod p => p -> Nat
periodLength p = S $ cast {to=Nat} $ diffDays (periodLastDay p) (periodFirstDay p)

||| Get the period this day is in, with the 1-based day number within the period.
|||
export
periodFromDay : DayPeriod p => Day -> (p, Nat)
periodFromDay d =
  let p  = dayPeriod d
      dt = S $ cast {to=Nat} $ diffDays d $ periodFirstDay p
    in (p, dt)

||| Inverse of 'periodFromDay'.
|||
export
periodToDay : DayPeriod p => p -> Nat -> Day
periodToDay p i = addDays (cast {to=Integer} $ pred i) $ periodFirstDay p

||| Inverse of 'periodFromDay', clipping the day number to the period.
|||
export
periodToDayClip : DayPeriod p => p -> Nat -> Day
periodToDayClip p i = periodToDay p $ clip 1 (periodLength p) i

||| Validating inverse of 'periodFromDay'.
|||
export
periodToDayValid : DayPeriod p => p -> Nat -> Maybe Day
periodToDayValid p i =
  let d = periodToDay p i
    in if fst (periodFromDay d) == p then Just d else Nothing

public export
DayPeriod ModifiedJulianDay where
  periodFirstDay = id
  periodLastDay = id
  dayPeriod = id
