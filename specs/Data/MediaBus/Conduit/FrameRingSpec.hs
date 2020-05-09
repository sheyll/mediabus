{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MediaBus.Conduit.FrameRingSpec (spec) where

import Conduit
import Control.Monad
import Data.Conduit.List
import Data.MediaBus
import Data.Proxy
import Data.Word
import Debug.Trace
import Test.Hspec
import Test.QuickCheck


-- Behaviour:

--   If the ratio of the duration between frame arrivals and the duration
--   (i.e. /how many times longer it takes to receive a frame than to play a frame duration/)
--   is > 1, then the ring will likely undeflow, otherwise, the ring will overflow.
--
--   For example, of the time between frame arrival is 25ms and each frame has a duration of 20ms,
--   then it takes 1.2 times as long to receive than to play a frame.
--
--   A full frame is lost every ? frames.
--     frameDuration/dt_arrive
--   In order to reduce fragmentation as much as possible, the ring polling thread would need to
--   delay for 1 frame (where it would send 1 frame of silence) and could then send 4
--
--   If no new frame was received after waiting for the duration of one packet, put a 'Missing' into the queue.
--   If a frame arrives, put it into the ring, ignore the timestamp and sequence number.
--   The stream must be reordered before being passed to the FrameRing.
--   If a start frame is received, pass it through.
--   When an overflow occurs with a frame that is not a start-frame, and the oldest frame is a start-frame,
--   the second oldest frame will be removed.
--   An overflow will  push out a start-frame, unless it is itself a start-frame.

spec :: Spec
spec =
  describe "FrameRing" $ do

    describe "basics"
      $ do
           it "can be created with mkFrameRing" pending


           it "passes a single frame from a frameRingSink to a frameRingSource"
             pending
