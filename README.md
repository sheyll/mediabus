# Multimedia streaming on top of Conduit

[![Build Status](https://travis-ci.org/sheyll/mediabus.svg?branch=master)](https://travis-ci.org/sheyll/mediabus)
[![Hackage](https://img.shields.io/hackage/v/mediabus.svg)](http://hackage.haskell.org/package/mediabus)

Functions and types for multimedia streaming on top of conduit.

It is supposed to be a minimalistic rip-off of gstreamer.

Extra functionality is contained in external packages, usually called
_mediabus-foo_.

Special care has been taken to ensure that the functions in this package do not
have space leaks.

Also, this package supports concurrent stream processing.


## TODO

### Padding:

```haskell
-- --------------------------------------------------------------------------------------------
--
--  TODO use padding to prevent the tiny, hardly audible, but still perceivable .0999 second
--  gaps during playback.
appendPaddingC ::
  forall i s t p c m.
  ( Semigroup c,
    HasDuration c,
    CanGenerateBlankMedia c,
    Monad m
  ) =>
  NominalDiffTime ->
  ConduitT
    (Data.MediaBus.Stream i s t p (Segment c))
    (SyncStream i p (Padded c))
    m
    ()
appendPaddingC d =
  mapFrameContentC
    ( \(MkSegment segmentIn) ->
        let !paddingDuration = max 0 (d - getDuration segmentIn)
         in MkPadded
              { _unpadded = segmentIn <> blankFor @c paddingDuration,
                _paddingDuration = paddingDuration
              }
    )
    .| assumeSynchronizedC

data Padded c = MkPadded {_unpadded :: !c, _paddingDuration :: !NominalDiffTime}

-- ---

-- | Replace the timestamp using a function over the current time.
setTimestampFromCurrentTimeC ::
  forall i s t t' p c m.
  ( Monad m,
    MonadIO m
  ) =>
  (UTCTime -> t') ->
  ConduitT
    (Data.MediaBus.Stream i s t p c)
    (Data.MediaBus.Stream i s t' p c)
    m
    ()
setTimestampFromCurrentTimeC fTs =
  Conduit.mapMC $ \x -> do
    now <- liftIO getCurrentTime
    return (x & timestamp .~ fTs now)
```
