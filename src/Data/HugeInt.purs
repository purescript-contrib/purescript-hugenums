module Data.HugeInt
  ( HugeInt
  , fromInt
  , fromString
  , toInt
  , toHugeNum
  , fromHugeNum
  , round
  , ceil
  , floor
  , isPositive
  , isZero
  , isNegative
  , numOfDigits
  , even
  , odd
  ) where

import Prelude

import Data.String (length, drop)
import Data.String.CodeUnits (takeWhile)
import Data.Int (toNumber)
import Data.Int (round) as Int
import Data.Maybe (Maybe(..))
import Data.HugeNum (HugeNum)
import Data.HugeNum as HN

newtype HugeInt = HugeInt HugeNum

instance eqHugeInt :: Eq HugeInt where
  eq (HugeInt h1) (HugeInt h2) = h1 == h2

instance ordHugeInt :: Ord HugeInt where
  compare (HugeInt h1) (HugeInt h2) = compare h1 h2

instance showHugeInt :: Show HugeInt where
  show (HugeInt h) = "HugeInt " <> takeWhile (_ /= '.') (HN.toString h)

instance semiringHugeInt :: Semiring HugeInt where
  zero = HugeInt zero
  add (HugeInt h1) (HugeInt h2) = HugeInt (h1 + h2)
  one = HugeInt one
  mul (HugeInt h1) (HugeInt h2) = HugeInt (h1 * h2)

instance ringHugeInt :: Ring HugeInt where
  sub (HugeInt h1) (HugeInt h2) = HugeInt (h1 - h2)

fromInt :: Int -> HugeInt
fromInt = HugeInt <<< HN.fromNumber <<< toNumber

toHugeNum :: HugeInt -> HugeNum
toHugeNum (HugeInt h) = h

-- | Requires the input to have a zero-valued fractional component.
fromHugeNum :: HugeNum -> Maybe HugeInt
fromHugeNum h | HN.integerPart h == h = pure $ HugeInt h
              | otherwise = Nothing

-- | Input must look like a Purescript `Int`.
-- | For example, `fromString "123456789012345" => Just (HugeInt 123456789012345)`.
fromString :: String -> Maybe HugeInt
fromString s = HugeInt <$> HN.fromString (s <> ".0")

toInt :: HugeInt -> Maybe Int
toInt h@(HugeInt r)
  | fromInt bottom <= h && h <= fromInt top = pure $ Int.round $ HN.toNumber r
  | otherwise = Nothing

abs :: HugeInt -> HugeInt
abs (HugeInt h) = HugeInt (HN.abs h)

neg :: HugeInt -> HugeInt
neg (HugeInt h) = HugeInt (HN.neg h)

round :: HugeNum -> HugeInt
round = HugeInt <<< HN.round

ceil :: HugeNum -> HugeInt
ceil = HugeInt <<< HN.ceil

floor :: HugeNum -> HugeInt
floor = HugeInt <<< HN.floor

isPositive :: HugeInt -> Boolean
isPositive (HugeInt h) = HN.isPositive h

isZero :: HugeInt -> Boolean
isZero (HugeInt h) = HN.isZero h

isNegative :: HugeInt -> Boolean
isNegative (HugeInt h) = HN.isNegative h

numOfDigits :: HugeInt -> Int
numOfDigits (HugeInt h) = HN.numOfIntegral h

even :: HugeInt-> Boolean
even h = case drop (length (show h) - 1) (show h) of
              "0" -> true
              "2" -> true
              "4" -> true
              "6" -> true
              "8" -> true
              _ -> false

odd :: HugeInt -> Boolean
odd = not <<< even
