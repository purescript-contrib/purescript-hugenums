module Test.Main where

import Prelude
import Test.QuickCheck.Laws.Data.Eq
import Test.QuickCheck.Laws.Data.Ord
import Test.QuickCheck.Laws.Data.Semiring
import Test.QuickCheck.Laws.Data.Ring
import Data.HugeNum
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (F, Foreign, unsafeFromForeign)
import Data.Foreign.Class (readJSON, write)
import Test.QuickCheck ((===))
import Type.Proxy (Proxy(..))

prxHugeNum :: Proxy HugeNum
prxHugeNum = Proxy

testForeign :: forall eff. Eff (console :: CONSOLE | eff) Unit
testForeign = do
  -- Read from Foreign
  let x = runExcept $ (readJSON """1.01""" :: F (HugeNum))
  let x' = (either (\e -> fromNumber 0.0) id x) :: HugeNum
  let y = runExcept $ (readJSON """2.02""" :: F (HugeNum))
  let y' = (either (\e -> fromNumber 0.0) id y) :: HugeNum
  logShow $ x' + y' === (fromNumber 3.03)
  -- Write to Foreign
  let z = unsafeFromForeign $ (write (fromNumber 5.05) :: Foreign)
  log $ z <> " should equal 5.05"
  --logShow $ z === """5.05"""
  --log $ unsafeFromForeign $ write (PriceRecord { price: fromNumber 1.01 })

main :: forall eff.
  Eff
    ( console :: CONSOLE
    , random :: RANDOM
    , err :: EXCEPTION
    | eff
    )
    Unit
main = do
  log "Testing Foreign instances...\n"
  testForeign
  log "Checking HugeNum instances...\n"
  checkEq prxHugeNum
  checkOrd prxHugeNum
  checkSemiring prxHugeNum
  checkRing prxHugeNum
