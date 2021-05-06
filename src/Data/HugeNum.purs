module Data.HugeNum
  ( HugeNum
  , fromString
  , fromNumber
  , toNumber
  , toString
  , integerPart
  , fractionalPart
  , numOfIntegral
  , numOfFractional
  , abs
  , neg
  , isNegative
  , isPositive
  , isZero
  , floor
  , ceil
  , round
  , googol
  , subHugeNum
  , pow, (^)
  , truncate
  ) where

import Prelude
import Data.List as L
import Data.Digit (Digit, toInt, fromInt, fromChar, toChar, _zero, _one)
import Data.Foldable (foldl, all, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Int (odd, round) as Int
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as Number
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (replicate)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafePartial)

-- | ##Type definitions
-- | Well-formed HugeNums are such that the decimal is a positive number less
-- | than the length of the list of digits. For example, to denote the integer
-- | 2, we would set `sign = Plus, digits = 2 : 0 : Nil, decimal = 1`.
-- | Any extraneous 0's on either end of the list of digits should be trimmed.

data Sign = Plus | Minus
type HugeRec = { digits :: List Digit, decimal :: Int, sign :: Sign }
-- Can use type synonym when bug fixed: https://github.com/purescript/purescript/issues/1443
newtype HugeNum = HugeNum { digits :: List Digit, decimal :: Int, sign :: Sign }

-- | ##Instances

derive instance genericSign :: Generic Sign _

instance eqSign :: Eq Sign where
  eq Plus Plus = true
  eq Minus Minus = true
  eq _ _ = false

instance ordSign :: Ord Sign where
  compare Plus Minus = GT
  compare Minus Plus = LT
  compare _ _ = EQ

timesSign :: Sign -> Sign -> Sign
timesSign Plus Plus = Plus
timesSign Minus Minus = Plus
timesSign _ _ = Minus

instance showHugeNum :: Show HugeNum where
  show = append "HugeNum " <<< toString -- <<< dropZeroes

instance eqHugeNum :: Eq HugeNum where
  eq x y
    | isZero x && isZero y = true
    | otherwise = strictlyEqual (dropZeroes x) (dropZeroes y)

instance ordHugeNum :: Ord HugeNum where
  compare = compareHugeNum

instance semiringHugeNum :: Semiring HugeNum where
  one = oneHugeNum
  mul = times
  zero = zeroHugeNum
  add = plus

instance ringHugeNum :: Ring HugeNum where
  sub r1 r2 = r1 + neg r2

-- | ## Utility functions

rec :: HugeNum -> HugeRec
rec (HugeNum r) = r

strictlyEqual :: HugeNum -> HugeNum -> Boolean
strictlyEqual (HugeNum r1) (HugeNum r2) =
  r1.decimal == r2.decimal && r1.digits == r2.digits && r1.sign == r2.sign

compareHugeNum :: HugeNum -> HugeNum -> Ordering
compareHugeNum x@(HugeNum r1) y@(HugeNum r2)
  | r1.sign < r2.sign = LT
  | r1.sign > r2.sign = GT
  | r1.decimal > r2.decimal = if r1.sign == Minus then LT else GT
  | r1.decimal < r2.decimal = if r1.sign == Minus then GT else LT
  | x == y = EQ
  | otherwise = z where
    dec = r1.decimal
    r = equivalize { fst: x, snd: y }
    s = rec r.fst
    m = rec r.snd
    x' = if x == r.fst then s else m
    y' = if y == r.fst then s else m
    z = case compare (L.take dec r1.digits) (L.take dec r2.digits) of
             EQ -> compare (L.drop dec x'.digits) (L.drop dec y'.digits)
             other -> other

-- | Drops leading and trailing _zeroes.
dropZeroes :: HugeNum -> HugeNum
dropZeroes = dropIntegralZeroes <<< dropFractionalZeroes where
  dropFractionalZeroes (HugeNum r) = HugeNum z where
    fractionalDigits = L.reverse (L.drop r.decimal r.digits)
    meatyFraction = L.dropWhile (_ == _zero) fractionalDigits
    digits = L.reverse meatyFraction
    z = r { digits = L.take r.decimal r.digits <> if L.null digits then pure _zero else digits }
  dropIntegralZeroes (HugeNum r) = HugeNum z where
    integralPart = L.take r.decimal r.digits
    zeroes = L.takeWhile (_ == _zero) integralPart
    digits = if L.length integralPart == L.length zeroes
                then pure _zero
                else L.drop (L.length zeroes) integralPart
    decimal = L.length digits
    z = r { digits = digits <> L.drop r.decimal r.digits, decimal = decimal }

-- | Adds _zeroes where necessary to make two HugeNums have the same number of
-- | digits and the same decimal place.
equivalize :: { fst:: HugeNum, snd :: HugeNum } -> { fst :: HugeNum, snd :: HugeNum }
equivalize = integralize <<< fractionalize where
  fractionalize { fst: x@(HugeNum r1), snd: y@(HugeNum r2) }
    | L.length (L.drop r1.decimal r1.digits) == L.length (L.drop r2.decimal r2.digits) = { fst: x, snd: y }
    | otherwise = z where
      test = x `lessPrecise` y
      lesser = if test then r1 else r2
      greater = if test then r2 else r1
      lesserDecimal = L.length (L.drop greater.decimal greater.digits) - L.length (L.drop lesser.decimal lesser.digits)
      zeroes = replicate lesserDecimal _zero
      lesser' = lesser { digits = lesser.digits <> zeroes }
      z = { fst: HugeNum lesser', snd: HugeNum greater }
  integralize { fst: x@(HugeNum r1), snd: y@(HugeNum r2) }
    | L.length (L.take r1.decimal r1.digits) == L.length (L.take r2.decimal r2.digits) = { fst: x, snd: y }
    | otherwise = z where
      lesser = rec (min x y)
      greater = rec (max x y)
      zeroesLength = L.length (L.take greater.decimal greater.digits) - L.length (L.take lesser.decimal lesser.digits)
      zeroes = replicate zeroesLength _zero
      lesser' = lesser { digits = zeroes <> lesser.digits, decimal = greater.decimal }
      z = { fst: HugeNum lesser', snd: HugeNum greater }

-- | Check whether a HugeNum has fewer significant fractional digits than another.
lessPrecise :: HugeNum -> HugeNum -> Boolean
lessPrecise (HugeNum r1) (HugeNum r2) =
  L.length (L.drop r1.decimal r1.digits) < L.length (L.drop r2.decimal r2.digits)

-- | Creates a HugeNum from a Number.
-- | May lose precision if the argument is too large.
toNumber :: HugeNum -> Number
toNumber n = unsafePartial $ fromJust $ Number.fromString $ toString n

toString :: HugeNum -> String
toString (HugeNum r) =
  let charray = map toChar r.digits :: List Char
      numray = unsafePartial $ fromJust $ L.insertAt r.decimal '.' charray
      sign = case r.sign of
                  Plus -> mempty :: List Char
                  Minus -> pure '-' :: List Char
   in foldMap singleton (sign <> numray)

-- | Create a HugeNum from a String.
-- | Strings should be in the form of a Purescript `Number`. For example,
-- | `fromString "123.456" => Just (HugeNum 123.456)`
fromString :: String -> Maybe HugeNum
fromString s = do
  let charlist = L.fromFoldable $ toCharArray s
  leadingChar <- L.head charlist
  let signDigits = case leadingChar of
                  '-' -> { sign: Minus, digits: L.drop 1 charlist }
                  _ -> { sign: Plus, digits: charlist }
      sign = signDigits.sign
  decimal <- L.elemIndex '.' signDigits.digits
  newCharlist <- L.deleteAt decimal signDigits.digits
  digits <- sequence $ map fromChar newCharlist
  pure $ HugeNum { sign, decimal, digits }

-- | Create a HugeNum from a Number.

-- | Numbers have three different string representations in Javascript.
-- | Floats look like "123.456"; Scientific look like "-3.34e+56" and
-- | Integrals look like "9000000000000000000".
data NumberStyle = Float | Integral | Scientific

-- | May lose precision if the argument is too large.
-- | For example, the fractional part of `9000000000000000.5` is unrecoverable.
parseNumber :: Number -> NumberStyle
parseNumber n
  | contains (Pattern "e") (show n) = Scientific
  | contains (Pattern ".") (show n) = Float
  | otherwise = Integral

floatToHugeNum :: Number -> HugeNum
floatToHugeNum n = HugeNum r where
  pos = n >= zero
  split = if pos
             then L.span (_ /= '.') (L.fromFoldable <<< toCharArray $ show n)
             else L.span (_ /= '.') (L.drop 1 $ L.fromFoldable <<< toCharArray $ show n)
  big = split.init
  small = L.drop 1 split.rest
  sign = if n < zero then Minus else Plus
  digits = L.mapMaybe fromChar $ big <> small
  decimal = L.length big
  r = { digits, decimal, sign }

integralToHugeNum :: Number -> HugeNum
integralToHugeNum n =
  let integral = L.fromFoldable <<< toCharArray $ show n
      fractional = pure _zero
      sign = if n < zero then Minus else Plus
      decimal = case sign of
                     Minus -> L.length integral - 1
                     _ -> L.length integral
   in HugeNum { digits: L.mapMaybe fromChar integral <> fractional
              , decimal, sign }

scientificToHugeNum :: Number -> HugeNum
scientificToHugeNum n = HugeNum r where
  parsed = parseScientific n
  r = case parsed.expSign of
           Plus -> case parsed.sign of
                        Plus -> parsePlusPlus parsed.exponent parsed.base
                        _ -> parsePlusMinus parsed.exponent parsed.base
           _ -> case parsed.sign of
                     Plus -> parseMinusPlus parsed.exponent parsed.base
                     _ -> parseMinusMinus parsed.exponent parsed.base

parseScientific :: Number -> { exponent :: Int, expSign :: Sign, base :: List Char, sign :: Sign }
parseScientific n = z where
  split = L.span (_ /= 'e') (L.fromFoldable <<< toCharArray $ show n)
  base = L.filter (_ /= '.') split.init
  sign = if n < zero then Minus else Plus
  signSplit = unsafePartial $ fromJust $ L.uncons $ L.drop 1 split.rest
  expSign = case signSplit.head of
                 '+' -> Plus
                 _ -> Minus
  exponent = Int.round $ unsafePartial $ fromJust $ Number.fromString $ foldMap singleton signSplit.tail
  z = { exponent, expSign, base, sign }

parsePlusPlus :: Int -> List Char -> HugeRec
parsePlusPlus exp base = r where
  zeroCardinality = exp - L.length base + 2
  zeroes = replicate zeroCardinality _zero
  digits = L.mapMaybe fromChar base <> zeroes
  decimal = 1 + exp
  r = { digits: digits, decimal: decimal, sign: Plus }

parsePlusMinus :: Int -> List Char -> HugeRec
parsePlusMinus exp base =
  let r = parsePlusPlus exp base
   in r { sign = Minus, digits = r.digits <> pure _zero }

parseMinusPlus :: Int -> List Char -> HugeRec
parseMinusPlus exp base = r where
  zeroes = replicate exp _zero
  digits = zeroes <> L.mapMaybe fromChar base
  decimal = 1
  r = { digits: digits, decimal: decimal, sign: Plus }

parseMinusMinus :: Int -> List Char -> HugeRec
parseMinusMinus exp base =
  let r = parseMinusPlus exp base
   in r { sign = Minus }

fromNumber :: Number -> HugeNum
fromNumber n = case parseNumber n of
                    Float -> floatToHugeNum n
                    Scientific -> scientificToHugeNum n
                    Integral -> integralToHugeNum n

-- | Limits the number of digits past the decimal.
truncate :: Int -> HugeNum -> HugeNum
truncate n (HugeNum r) = HugeNum z where
  integral = L.take r.decimal r.digits
  fractional = L.drop r.decimal r.digits
  newFractional = L.take n fractional
  z = r { digits = integral <> newFractional }

-- | Counts how many digits are before the decimal.
numOfIntegral :: HugeNum -> Int
numOfIntegral (HugeNum r) = r.decimal

-- | Counts how many digits are after the decimal.
numOfFractional :: HugeNum -> Int
numOfFractional (HugeNum r) = L.length r.digits - r.decimal

-- | Returns the integer part of a HugeNum.
integerPart :: HugeNum -> HugeNum
integerPart (HugeNum r) =
  HugeNum (r { digits = L.take r.decimal r.digits <> pure _zero})

-- | Returns the closest integer-valued HugeNum less than or equal to the argument.
floor :: HugeNum -> HugeNum
floor h | isPositive h = integerPart h
        | isZero h = zero
        | otherwise = integerPart h - one

-- | Returns the closest integer-valued HugeNum greater than or equal to the argument.
ceil :: HugeNum -> HugeNum
ceil h | isNegative h = integerPart h
       | isZero h = zero
       | otherwise = integerPart h + one

-- | Returns the closest integer-valued HugeNum to the argument.
round :: HugeNum -> HugeNum
round h | abs (h - floor h) < abs (ceil h - h) = floor h
        | otherwise = ceil h

-- | Returns the fractional part of a HugeNum.
fractionalPart :: HugeNum -> HugeNum
fractionalPart (HugeNum r) =
  HugeNum (r { digits = _zero : L.drop r.decimal r.digits, decimal = 1 })

-- | Creates a nonnegative value with the same magnitude as the argument.
abs :: HugeNum -> HugeNum
abs (HugeNum r) = HugeNum (r { sign = Plus })

isNegative :: HugeNum -> Boolean
isNegative (HugeNum { sign: Minus }) = true
isNegative _ = false

isPositive :: HugeNum -> Boolean
isPositive (HugeNum { sign: Plus }) = true
isPositive _ = false

isZero :: HugeNum -> Boolean
isZero (HugeNum r) = all (_ == _zero) r.digits

-- | Flips the sign. While `negate` from the Prelude does the same, this is faster.
neg :: HugeNum -> HugeNum
neg (HugeNum r@{ sign: Minus }) = HugeNum (r { sign = Plus})
neg (HugeNum r) = HugeNum (r { sign = Minus })

-- | Specific HugeNums

zeroHugeNum :: HugeNum
zeroHugeNum = HugeNum { digits: _zero : _zero : Nil, decimal: 1, sign: Plus }

oneHugeNum :: HugeNum
oneHugeNum = HugeNum { digits: _one : _zero : Nil, decimal: 1, sign: Plus }

googol :: HugeNum
googol = HugeNum { digits: _one : replicate 101 _zero, decimal: 101, sign: Plus }

-- | ## Addition

addPlusPlus :: HugeNum -> HugeNum -> HugeNum
addPlusPlus x y = dropZeroes (HugeNum z) where
  eqv = equivalize { fst: x, snd: y }
  r1 = rec eqv.fst
  r2 = rec eqv.snd
  r = L.zip (L.reverse r1.digits) (L.reverse r2.digits)
  digits'' = foldl digitwiseAdd (Tuple mempty _zero) r
  spill = snd digits''
  digits' = fst digits''
  digits = unsafeRemoveFrontZeroes $ spill : digits'
  decimal = adjustDecimalForFrontZeroes (spill : digits') (r1.decimal + 1)
  z = { digits: adjustDigitsForDecimal decimal digits, decimal: decimal, sign: Plus }

adjustDigitsForDecimal :: Int -> List Digit -> List Digit
adjustDigitsForDecimal decimal digits = go (decimal - L.length digits + 1) digits
  where
  go n ds
    | n <= 0 = ds
    | otherwise = go (n - 1) (_zero : ds)

digitwiseAdd :: Tuple (List Digit) Digit -> Tuple Digit Digit -> Tuple (List Digit) Digit
digitwiseAdd (Tuple xs d) (Tuple t b) =
  let tint = toInt t + toInt d
      bint = toInt b
      summ' = tint + bint
      summ = unsafePartial $ fromJust $ fromInt if summ' > 9 then summ' - 10 else summ'
      spill = if summ' > 9 then _one else _zero
   in Tuple (summ : xs) spill

addMinusMinus :: HugeNum -> HugeNum -> HugeNum
addMinusMinus x y =
  let z = rec (addPlusPlus x y)
   in HugeNum (z { sign = Minus })

-- | Assumes 0 <= x, 0 <= abs y <= x, y <= 0
addPlusMinus :: HugeNum -> HugeNum -> HugeNum
addPlusMinus x y = (HugeNum z) where
  eqv = equivalize { fst: x, snd: y }
  r2 = rec $ max eqv.fst eqv.snd
  r1 = rec $ min eqv.fst eqv.snd
  r = L.zip (L.reverse r2.digits) (L.reverse r1.digits)
  digits'' = foldl digitwiseSubtract (Tuple mempty _zero) r
  integralDigits'' = L.take r1.decimal $ fst digits''
  fractionalDigits = L.drop r1.decimal $ fst digits''
  integralDigits' = unsafeRemoveFrontZeroes integralDigits''
  integralDigits = if L.null integralDigits' then pure _zero else integralDigits'
  decimal = adjustDecimalForFrontZeroes (fst digits'') r1.decimal
  digits = integralDigits <> fractionalDigits
  z = { digits: digits, decimal: decimal, sign: Plus }

digitwiseSubtract :: Tuple (List Digit) Digit -> Tuple Digit Digit -> Tuple (List Digit) Digit
digitwiseSubtract (Tuple xs d) (Tuple t b) =
  let tint = toInt t - toInt d
      bint = toInt b
      diff' = tint - bint
      diff = unsafePartial $ fromJust $ fromInt if diff' < 0 then diff' + 10 else diff'
      spill = if diff' < 0 then _one else _zero
   in Tuple (diff : xs) spill

unsafeRemoveFrontZeroes :: List Digit -> List Digit
unsafeRemoveFrontZeroes = L.dropWhile (_ == _zero)

adjustDecimalForFrontZeroes :: List Digit -> Int -> Int
adjustDecimalForFrontZeroes xs oldDec =
  let newDigits' = unsafeRemoveFrontZeroes $ L.take oldDec xs
      newDigits = if L.null newDigits' then pure _zero else newDigits'
   in L.length newDigits

plus :: HugeNum -> HugeNum -> HugeNum
plus x y
  | isZero x = y
  | isZero y = x
  | isPositive x && isPositive y = addPlusPlus x y
  | isNegative x && isNegative y = addMinusMinus x y
  | otherwise = z where
    greaterMag = max (abs x) (abs y)
    lesserMag = min (abs x) (abs y)
    greater = max x y
    lesser = min x y
    z = if greater == greaterMag
           then addPlusMinus greater lesser
           else neg (addPlusMinus greaterMag lesserMag)

subHugeNum :: HugeNum -> HugeNum -> HugeNum
subHugeNum x y = plus x (neg y)

-- | ## Multiplication

-- | For any integral x with n digits in base B, x = x1 * B^m + x0 for all m and
-- | x0 < B^m.
type KRep = { exp :: Int, coeff :: List Digit, const :: HugeNum }

-- | Drop the _zeroes on the tail.
takeMeatyParts :: List Digit -> List Digit
takeMeatyParts arr =
  L.reverse (L.dropWhile (_ == _zero) (L.reverse arr))

-- | Turn a `HugeNum` into a `KRep`, given an exponent m for B^m.
toKRep :: Int -> HugeNum -> KRep
toKRep exp h@(HugeNum r) = z where
  bm = _one : replicate exp _zero
  coeff = L.take (r.decimal - exp) r.digits
  prod = coeff <> L.drop 1 bm
  leftSummand = arrayToHugeNum prod
  k = plus h (neg leftSummand)
  z = { exp: exp, coeff: coeff, const: k }

-- | Takes two HugeNums and calculates a suitable exponent m in B^m.
getPowForKRep :: HugeNum -> HugeNum -> Int
getPowForKRep x y = (_ - 1) $ _.decimal $ rec $ min (abs x) (abs y)

-- | Turns an array of digits into an integral HugeNum.
arrayToHugeNum :: List Digit -> HugeNum
arrayToHugeNum xs =
  HugeNum { sign: Plus, digits: xs <> pure _zero, decimal: L.length xs }

-- | Test for whether we can reach the base case of recursive multiplication.
smallEnough :: HugeNum -> Boolean
smallEnough (HugeNum { digits: digits, decimal: 1 }) | L.length digits == 2 = true
smallEnough _ = false

-- | Multiplying by a power of ten is easy. All we have to do is append _zeroes!
timesPowOfTen :: Int -> HugeNum -> HugeNum
timesPowOfTen n (HugeNum r) = z where
  newDecimal = r.decimal + n
  newZeroesLength = if newDecimal >= L.length (takeMeatyParts r.digits)
                       then newDecimal - L.length (takeMeatyParts r.digits)
                       else 0
  newZeroes = replicate newZeroesLength _zero
  z = HugeNum { digits: r.digits <> newZeroes, sign: r.sign, decimal: newDecimal }

-- | Karatsuba multiplication.
times :: HugeNum -> HugeNum -> HugeNum
times r1 r2
  | timesSign (_.sign $ rec r1) (_.sign $ rec r2) == Minus = neg (times (abs r1) (abs r2))
  | not (trivialFraction r1) || not (trivialFraction r2) =
    -- Multiply without decimal points first, then move the decimal point to the
    --   correct spot in the result. e.g. 1.1 * 2*2 === 11 * 22 / 100
    adjustDecimalForTriviality r1 r2 $ times (makeHugeInteger r1) (makeHugeInteger r2)
  | smallEnough r1 = multSmallNum r1 r2
  | smallEnough r2 = multSmallNum r2 r1
  | otherwise = z where
    exp = getPowForKRep r1 r2
    k1 = toKRep exp r1
    k2 = toKRep exp r2
    x0 = k1.const
    y0 = k2.const
    x1 = k1.coeff
    y1 = k2.coeff
    z0 = times x0 y0
    z2 = times (arrayToHugeNum x1) (arrayToHugeNum y1)
    z1 = let leftFactor = plus (arrayToHugeNum x1) x0
             rightFactor = plus (arrayToHugeNum y1) y0
             prod = times leftFactor rightFactor
          in plus (plus prod (neg z2)) (neg z0)
    z2B2m = timesPowOfTen (2 * exp) z2
    z1Bm = timesPowOfTen exp z1
    z = plus (plus z2B2m z1Bm) z0

-- | "Scalar" multiplication by an Int.
scale :: Int -> HugeNum -> HugeNum
scale = go zeroHugeNum where
  go x 0 _ = x
  go x n k = go (plus x k) (n - 1) k

-- | The base case of recursive multiplication
multSmallNum :: HugeNum -> HugeNum -> HugeNum
multSmallNum (HugeNum r) r2 =
  case L.uncons r.digits of
       Just result -> if result.head == _zero then zeroHugeNum else scale (toInt result.head) r2
       _ -> unsafeThrow "Error: The impossible happened"

-- | Count how much information the fractional part of a HugeNum holds.
meatyDecimals :: HugeNum -> Int
meatyDecimals (HugeNum r) =
  let decimals = L.reverse $ L.drop r.decimal r.digits
      meaty = L.dropWhile (_ == _zero) decimals
   in L.length meaty

isHugeInteger :: HugeNum -> Boolean
isHugeInteger (HugeNum r) = all (_ == _zero) $ L.drop r.decimal r.digits

-- | Moves the decimal place in a HugeNum so it has a trivial fractional part.
makeHugeInteger :: HugeNum -> HugeNum
makeHugeInteger r = if isHugeInteger r then r else makeHugeInteger' r

-- | Assumes a nontrivial fractional component
makeHugeInteger' :: HugeNum -> HugeNum
makeHugeInteger' (HugeNum r) = HugeNum z where
  digits = unsafeRemoveFrontZeroes r.digits <> pure _zero
  decimal = L.length digits - 1
  z = r { digits = digits, decimal = decimal }

-- | Test for whether a HugeNum has any non-_zero digits in its fractional part.
trivialFraction :: HugeNum -> Boolean
trivialFraction (HugeNum r) =
  let decimals = L.reverse $ L.drop r.decimal r.digits
      meaty = L.dropWhile (_ == _zero) decimals
   in L.null meaty

-- | When multiplying two HugeNums and one has a nontrivial fractional part,
-- | we first turn them into integral HugeNums, then calculate where the
-- | decimal should be.
adjustDecimalForTriviality :: HugeNum -> HugeNum -> HugeNum -> HugeNum
adjustDecimalForTriviality h1 h2 (HugeNum r3) = dropZeroes (HugeNum r) where
  digitsLength = L.length r3.digits - 1
  digits' = L.take digitsLength r3.digits
  decimalMod = meatyDecimals h1 + meatyDecimals h2
  digits = replicate (decimalMod - digitsLength + 1) _zero <> digits'
  decimal = L.length $ L.drop decimalMod $ L.reverse digits
  sign = Plus
  r = { digits, decimal, sign }

-- | Raise a HugeNum to an integer power.
pow :: HugeNum -> Int -> HugeNum
pow _ 0 = one
pow r 1 = r
pow r n =
  let c = r * r
      ans = pow c (n / 2)
   in if Int.odd n
         then r * ans
         else ans

infixr 8 pow as ^

-- | Division
{-- long :: HugeNum -> HugeNum -> HugeNum --}
{-- long num den --}
{--   | isZero den = unsafeThrow "division by zero" --}
{--   | num == den = oneHugeNum --}
{--   | num < den = divProper num den --}
{--   | otherwise = divImproper num den --}

{-- divProper :: HugeNum -> HugeNum -> HugeNum --}
{-- divProper num den = oneHugeNum --}

{-- divImproper :: HugeNum -> HugeNum -> HugeNum --}
{-- divImproper num den = oneHugeNum --}

{-- adjustDecimalsForProper :: HugeNum -> HugeNum -> { num :: HugeNum, den :: HugeNum } --}
{-- adjustDecimalsForProper (HugeNum h1) (HugeNum h2) = HugeNum h where --}
