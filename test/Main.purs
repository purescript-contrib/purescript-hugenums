module Test.Main where

import Prelude

import Data.HugeNum (HugeNum)
import Data.HugeNum.Gen (genHugeNum)
import Data.HugeInt (HugeInt)
import Data.HugeInt.Gen (genHugeInt)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Ring (checkRing)
import Test.QuickCheck.Laws.Data.Semiring (checkSemiring)
import Type.Proxy (Proxy(..))

-- | HugeNum Arbs
newtype ArbHugeNum = ArbHugeNum HugeNum

derive newtype instance eqArbHugeNum :: Eq ArbHugeNum
derive newtype instance ordArbHugeNum :: Ord ArbHugeNum
derive newtype instance semiringArbHugeNum :: Semiring ArbHugeNum
derive newtype instance ringArbHugeNum :: Ring ArbHugeNum

instance arbitraryArbHugeNum :: Arbitrary ArbHugeNum where
  arbitrary = ArbHugeNum <$> genHugeNum

prxHugeNum :: Proxy ArbHugeNum
prxHugeNum = Proxy

-- | HugeInt Arbs
newtype ArbHugeInt = ArbHugeInt HugeInt

derive newtype instance eqArbHugeInt :: Eq ArbHugeInt
derive newtype instance ordArbHugeInt :: Ord ArbHugeInt
derive newtype instance semiringArbHugeInt :: Semiring ArbHugeInt
derive newtype instance ringArbHugeInt :: Ring ArbHugeInt

instance arbitraryArbHugeInt :: Arbitrary ArbHugeInt where
  arbitrary = ArbHugeInt <$> genHugeInt

prxHugeInt :: Proxy ArbHugeInt
prxHugeInt = Proxy

main :: Effect Unit
main = do
  log "Checking HugeNum instances...\n"
  checkEq prxHugeNum
  checkOrd prxHugeNum
  checkSemiring prxHugeNum
  checkRing prxHugeNum
  log "Checking HugeInt instances...\n"
  checkEq prxHugeInt
  checkOrd prxHugeInt
  checkSemiring prxHugeInt
  checkRing prxHugeInt
