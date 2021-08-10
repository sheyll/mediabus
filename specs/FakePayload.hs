{-# LANGUAGE NumericUnderscores #-}
module FakePayload where

import Data.MediaBus
import Test.QuickCheck
import Control.Lens
import Data.Default
import Control.DeepSeq

newtype FakePayload
  = FP
      { fakeDuration :: Ticks64At8000
      }
  deriving (Eq, Show, Num, Arbitrary, Ord, NFData)

instance Default FakePayload where
  def = FP (MkTicks 60)

instance HasDuration FakePayload where
  getDuration = view (to fakeDuration . nominalDiffTime)

instance HasStaticDuration FakePayload where
  type GetStaticDuration FakePayload = 20 ':/: Hz 1000
