module Data.MediaBus.Transport.Udp (udpDatagramSource) where

import Conduit
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import Data.Conduit.Network.UDP
import Data.Default
import Data.MediaBus.Basics.Clock
import Data.MediaBus.Basics.Sequence
import Data.MediaBus.Basics.SourceId
import Data.MediaBus.Conduit.Stream
import Data.MediaBus.Media.Stream
import Data.Streaming.Network
import Network.Socket (SockAddr, close)
import Control.Monad.Logger (logInfo, MonadLogger, logDebug)
import Text.Printf (printf)
import Data.String (IsString(fromString))

-- | A UDP source that uses 'MonandResource' to make sure the socket is closed.
udpDatagramSource ::
  (IsClock c, MonadClock c m, MonadResource m, Num s, Default p, MonadLogger m) =>
  proxy c ->
  Int ->
  HostPreference ->
  ConduitT () (Stream (SourceId (Maybe SockAddr)) (SeqNum s) (ClockTimeDiff c) p B.ByteString) m ()
udpDatagramSource _clk port host = do
  !t0 <- lift now
  $logInfo (fromString (printf "starting UDP listener %s:%i" (show host) port))
  bracketP (bindPortUDP port host) close (`sourceSocket` 1024)
    .| evalStateC (Nothing, 0, t0) (awaitForever createFrame)
  $logInfo (fromString (printf "stopped UDP listener %s:%i" (show host) port))
  where
    yieldMsg = fromString (printf "datagram received on %s:%i" (show host) port)
    createFrame m = do
      let currentSender = msgSender m
      lastSender <- _1 <<.= Just currentSender
      tNow <- lift (lift now)
      when (Just currentSender /= lastSender) $ do
        _2 .= 0
        _3 .= tNow
        $logInfo (yieldMsg <> fromString (printf " sender changed: old was %s -> new is %s" (show lastSender) (show currentSender)))
        yieldStartFrameCtx
          ( MkFrameCtx
              (MkSourceId (Just currentSender))
              (timeAsTimeDiff tNow)
              0
              def
          )
      sn <- _2 <<+= 1
      tStart <- use _3
      unless (Just currentSender /= lastSender)  ($logDebug yieldMsg)
      yieldNextFrame (MkFrame (diffTime tNow tStart) sn (msgData m))
