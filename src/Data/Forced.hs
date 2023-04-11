{-# Language ExplicitForAll, UnliftedDatatypes, PatternSynonyms, GADTSyntax #-}

module Data.Forced
  ( Strict(..)
  , ForcedWHNF
  , pattern ForcedWHNF
  , ForcedNF
  , pattern ForcedNF
  , strictlyWHNF
  , strictlyNF
  ) where

import Data.Elevator ( UnliftedType, LiftedType )
import Control.DeepSeq

type Strict :: LiftedType -> UnliftedType
data Strict a where
  Strict :: !a -> Strict a

{- The invariants of @ForcedWHNF@ and @ForcedNF@ depends on the constructors
not being exported. The only way to construct these value is through the CBV
functions. Pattern matching is done via a unidirectional pattern.
-}
newtype ForcedWHNF a = ForcedOuter a

pattern ForcedWHNF :: forall a. a -> ForcedWHNF a
pattern ForcedWHNF a <- ForcedOuter a

newtype ForcedNF a = ForcedFull a

pattern ForcedNF :: forall a. a -> ForcedNF a
pattern ForcedNF a <- ForcedFull a

{- | This is a CBV function. Evaluates the argument to WHNF before
returning.
-}
strictlyWHNF :: forall a. a -> Strict (ForcedWHNF a)
strictlyWHNF a = Strict (ForcedOuter a)

{- | This is a CBV function. Evaluates the argument to NF before returning.
-}
strictlyNF :: forall a. NFData a => a -> Strict (ForcedNF a)
strictlyNF a = Strict (ForcedFull (rnf a `seq` a))
