# Ideas for Later

## Padding:

```haskell
-- --------------------------------------------------------------------------------------------
--
--  TODO
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
