{-# Language ExplicitForAll, UnliftedDatatypes, PatternSynonyms, GADTSyntax #-}

module Data.Forced
  ( Strict
  , Pairy(..)
  , StrictValueExtractor
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

extractStrict :: Strict a -> a
extractStrict (Strict a) = a

type Pairy :: UnliftedType -> LiftedType -> UnliftedType
data Pairy u l = Pairy u l

type StrictValueExtractor a = Pairy (Strict a) (Strict a -> a)

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
strictlyWHNF :: forall a. a -> StrictValueExtractor (ForcedWHNF a)
strictlyWHNF a = Pairy (Strict (ForcedOuter a)) extractStrict

{- | This is a CBV function. Evaluates the argument to NF before returning.
-}
strictlyNF :: forall a. NFData a => a -> StrictValueExtractor (ForcedNF a)
strictlyNF a = Pairy (Strict (ForcedFull (rnf a `seq` a))) extractStrict
