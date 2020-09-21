module Data.HugeInt.Gen where

import Prelude

import Control.Monad.Gen as Gen
import Data.HugeInt ( HugeInt )
import Data.HugeInt as HugeInt

genHugeInt :: forall m. Gen.MonadGen m => m HugeInt
genHugeInt = do
  i <- Gen.chooseInt 0 1000
  d <- Gen.chooseInt 0 10
  pure $ HugeInt.fromInt (i + d / 10)
