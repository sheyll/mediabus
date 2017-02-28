module Data.MediaBus.SequenceSpec ( spec ) where

import           Conduit                         as C
import           Data.Conduit.List               ( consume, sourceList )
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
import           Data.MediaBus

-- -----------------------------------------------------------------------------
-- * Tests/Specs
-- -----------------------------------------------------------------------------
spec :: Spec
spec =
    describe "synchronizeToSeqNum" $
        it "produces dense, strictly monotonic output" $
            property synchronizeToSeqNumIsMonotone

synchronizeToSeqNumIsMonotone :: NonEmptyList (SeqNum ())
                              -> Word64
                              -> Expectation
synchronizeToSeqNumIsMonotone (NonEmpty xs) startVal = do
    let inEvents = sourceList xs :: Producer Identity (SeqNum ())
        (e0 : rest) = runConduitPure (inEvents .|
                                          synchronizeToSeqNum startVal .|
                                          consume)
    e0 `shouldBe` MkSeqNum startVal

    (rest `zip` drop 1 rest) `shouldSatisfy`
        all (not .
                 uncurry succeeds)
