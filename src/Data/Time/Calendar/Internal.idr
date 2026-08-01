module Data.Time.Calendar.Internal

import Data.Fixed
import Data.IntegralGCD
import Data.List1
import Data.Ratio
import Data.String
import Derive.Prelude

%default total
%language ElabReflection

data PadOption
  = Pad Int Char
  | NoPad

%runElab derive "PadOption" [Show,Eq,Ord]

export
showPadded : PadOption -> String -> String
showPadded NoPad     s = s
showPadded (Pad i c) s = replicate (cast {to=Nat} (i - (cast {to=Int} (Prelude.List.length $ unpack s)))) c ++ s  

export
show2Fixed : Pico -> String
show2Fixed x =
  case x < 10 of
    True  =>
      (singleton '0') ++ (showFixed True x)
    False =>
      showFixed True x

public export
interface Ord t => Show t => ShowPadded t where
  showPaddedNum : PadOption -> t -> String 

export
show2 : ShowPadded t => t -> String
show2 = showPaddedNum $ Pad 2 '0'

export
show3 : ShowPadded t => t -> String
show3 = showPaddedNum $ Pad 3 '0'

export
show4 : ShowPadded t => t -> String
show4 = showPaddedNum $ Pad 4 '0'

export
mod100 : Integral i => i -> i
mod100 x = mod x 100

export
div100 : Integral i => i -> i
div100 x = div x 100

export
clip : Ord t => t -> t -> t -> t
clip a b x =
  case x < a of
    True  =>
      a
    False =>
      case x > b of
        True  =>
          b
        False =>
          x

export
clipValid : Ord t => t -> t -> t -> Maybe t
clipValid a b x =
  case x < a of
    True  =>
      Nothing
    False =>
      case x > b of
        True  =>
          Nothing
        False =>
          Just x

export partial
quotBy : IntegralGCD a => a -> a -> a
quotBy d n = truncate (mkRatio n d)
  where
    truncate : Integral a => Ratio a -> a
    truncate r = (numer r) `div` (denom r)

export partial
remBy : IntegralGCD a => Neg a => a -> a -> a
remBy d n = n - f * d
  where
    f = quotBy d n

export partial
quotRemBy : IntegralGCD a => Neg a => a -> a -> (a, a)
quotRemBy d n =
  let f = quotBy d n
    in (f, n - f * d)
