module Data.Digit
  ( Digit
  , fromInt , toInt
  , fromChar , toChar
  , _zero, _one, _two, _three, _four, _five, _six, _seven, _eight, _nine
  ) where

import Prelude
import Data.Char (toCharCode, fromCharCode)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

newtype Digit = Digit Int

instance showDigit :: Show Digit where
  show (Digit n) = "Digit " <> show n

instance eqDigit :: Eq Digit where
  eq (Digit n) (Digit m) = n == m

instance ordDigit :: Ord Digit where
  compare (Digit n) (Digit m) = compare n m

fromInt :: Int -> Maybe Digit
fromInt c
  | 0 <= c && c <= 9 = Just (Digit c)
  | otherwise = Nothing

toInt :: Digit -> Int
toInt (Digit c) = c

fromChar :: Char -> Maybe Digit
fromChar c = let code = toCharCode c
              in if code >= 48 && code <= 57
                    then Just (Digit $ code - 48)
                    else Nothing

toChar :: Digit -> Char
toChar (Digit c) = unsafePartial $ fromJust $ fromCharCode (c + 48)

_zero :: Digit
_zero = Digit 0

_one :: Digit
_one = Digit 1

_two :: Digit
_two = Digit 2

_three :: Digit
_three = Digit 3

_four :: Digit
_four = Digit 4

_five :: Digit
_five = Digit 5

_six :: Digit
_six = Digit 6

_seven :: Digit
_seven = Digit 7

_eight :: Digit
_eight = Digit 8

_nine :: Digit
_nine = Digit 9
