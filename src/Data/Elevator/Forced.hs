{-# Language ExplicitForAll, PatternSynonyms, DerivingVia #-}

module Data.Elevator.Forced
  ( ForcedWHNF
  , pattern ForcedWHNF
  , ForcedNF
  , pattern ForcedNF
  , strictlyWHNF
  , strictlyNF
  ) where

import Data.Elevator
import Control.DeepSeq
import NoThunks.Class

{- The invariants of @ForcedWHNF@ and @ForcedNF@ depends on the constructors
not being exported. The only way to construct these value is through the CBV
functions.
-}
newtype ForcedWHNF a = ForcedOuter a
  deriving NoThunks via InspectHeap a

pattern ForcedWHNF :: forall a. a -> ForcedWHNF a
pattern ForcedWHNF a <- ForcedOuter a

newtype ForcedNF a = ForcedFull a
  deriving NoThunks via InspectHeap a

pattern ForcedNF :: forall a. a -> ForcedNF a
pattern ForcedNF a <- ForcedFull a

-- | This is a CBV function. Evaluates the argument to WHNF before
-- returning.
strictlyWHNF :: forall a. a -> Strict (ForcedWHNF a)
strictlyWHNF a = Strict (ForcedOuter a) -- Yeah, really.

-- | This is a CBV function. Evaluates the argument to NF before returning.
strictlyNF :: forall a. NFData a => a -> Strict (ForcedNF a)
strictlyNF a = rnf a `seq` Strict (ForcedFull a)
