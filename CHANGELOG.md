# Changes

## 0.7.0

* Streamline and simplify `aggregateDurationC` and `aggregateCountC`

## 0.6.0

* Add buffer aggregation.

## 0.5.0.2

* Fix a small bug in `segmentC` and improve the tests

## 0.5.0.1

* Adapt to new upstream versions of base and conduit
* Create new `FrameRing` with better performance
* Deprecate `Data.MediaBus.Conduit.Async`
* Fix `assumeSynchronizedC`

## 0.4.0.0

* Add removeTimestamp, removeTimestampC
* Add setTimestampFromDurations, setTimestampFromDurationsC
* Merge Data.MediaBus.Conduit.Timing into Data.MediaBus.Conduit.SyncStream
* Rename setSequenceNumbersAndTimestamps to setSequenceNumberAndTimestamp
* Rename convertTicksC' to convertTimestampC'
* Rename mapTicksC to mapTimestampC
* Rename deriveFrameTimestamp to setTimestampFromDurationsC
* Improve SyncStream documentation
* Add conduit wrapper functions for the SyncStream functions

## 0.3.3.0

* Remove `Reframer`
* Add `SyncStream` type
* Add functions to add a sequence number and timestamp to a `SyncStream`
  rendering it a (reqular) `Stream`

## 0.3.2.0

* Add utility module `Reframer` for _timestamp_ adaption of 'Frame's
* Add utility module `VectorExtra`
* Add logging utility modules to prefix all log messages done in nested
  `Conduit`s and `MonadLoggerIO` monads

## 0.3.0.2 - Beginning of Changelog
