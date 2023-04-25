{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Data.Forced (
    -- * How to use this library

    -- ** Add a new flag on ghc-options
    -- $howToUse1

    -- ** Put ForcedWHNF or ForcedNF types on fields that need to have __no__ references when hold on a long lived data structure.
    -- $howToUse2

    -- ** Use this common idiom whenever you need to obtain a forced value
    -- $howToUse3

    -- * The 'UnliftedType' calling convention (or how to avoid pitfalls)
    -- $unliftedCallingConvetion

    -- * Unlifted types

    -- | We need these so whenever we bound a strict computation, all the
    -- lazy values will be forced as needed.
    Pairy (..),
    StrictValueExtractor,
    Strict,

    -- * Newtypes that hold a evaluation invariant
    -- $invariantNewtypes
    ForcedWHNF,
    pattern ForcedWHNF,
    ForcedNF,
    pattern ForcedNF,

    -- * Call By Value functions
    strictlyWHNF,
    strictlyNF,
) where

import Control.DeepSeq (NFData (rnf))
import Data.Elevator (LiftedType, UnliftedType)

{- $howToUse1
Add this to your .cabal file It will save us from a pitfall.

@
common warnings
    ghc-options: -Werror=unbanged-strict-patterns

library
  import: warnings
  ...

executable myAwesomeProgram
  import: warnings
@
-}

{- $howToUse2
@
import Data.Map.Lazy -- it is fine, really.
import Data.Vector

type MyMap a = Map (ForcedWHNF Char) (ForcedNF (Maybe Vector))

-- Prompt removal of deleted elements.
type MyMap2 a = ForcedWHNF (Map (ForcedWHNF Char) (ForcedNF (Maybe Vector)))
@

This way it will be a type error to store a thunk that is keeping references
alive.
-}

{- $howToUse3
  1. Strictly @let@ bound on your current context the result of a call to
  'strictlyWHNF' or 'strictlyNF'. __This is the most important part.__
  2. Use a lazy let to extract the underlying @ForcedWHNF a@ or @ForcedNF a@
  with the paired extractor.
  3. Store the previous result on the long lived data structure.

The ideal code piece looks like this:

@
import Data.Map.Lazy

type MyMap a = Map Char (ForcedNF (Maybe Int))

noThunksForWHNF :: IO ()
noThunksForWHNF = do
    let map0 :: ML.Map Char (ForcedWHNF Int)
        map0 = ML.empty

        -- Step 1. Strict let bound is done given the kind of
        -- StrictValueExtractor
        val0 :: StrictValueExtractor (ForcedWHNF Int)
        val0 = strictlyWHNF (const (2 + 2) map0)

        -- Step 2. The extractor is inside the Pairy constructor of val0
        val1 = case val0 of { Pairy v ext -> ext v }

        -- Step 3. Store as a lazy thunk without the references.
        map1 = ML.insert 'a' val1 map0
    pure ()
@
-}

{- $unliftedCallingConvetion

Types that have kind 'UnliftedType' have an different calling convention
than normal values. To achieve the correct evaluation level:

  1. We __should__ bound with a name (@let@) computations that return a type
     with 'UnliftedType' kind to the top level of our current context.
  2. We __must not__ inline computation with kind 'UnliftedType' at use
     sites. Specially if the use site is inside of a lazy function.

The first kind of mistake is hard to trigger if we follow the first section
rules. The library also steers you in the right direction by recommending
the following stanza.

@
common warnings
    ghc-options: -Wall -Werror=unbanged-strict-patterns
@

on the cabal file of your project. It will protect your against this common
error

@
noThunksForWHNF :: IO (ML.Map Char (Forced Int))
noThunksForWHNF = do
    let map0 :: ML.Map Char (ForcedWHNF Int)
        map0 = ML.empty

        -- Step 1 & 2 merged
        val0 :: StrictValueExtractor (ForcedWHNF Int)
        val0@(Pairy v ext) = strictlyWHNF (const (2 + 2) map0)

        -- Step 3. Store as a lazy thunk without the references.
        map1 = ML.insert 'a' val1 map0
    pure map1
@

Now @val0@ merged steps 1 and 2. __But in doing so it turned a strict let__
__into a lazy let__. The @-Werror=unbanged-strict-patterns@ will highlight
this at compile time and require you to put a @BangPattern@ on @val0@.

The problem about inlining is hiding a strict computation inside of a lazy
computation. So in the previous example

@
noThunksForWHNF :: IO (ML.Map Char (Forced Int))
noThunksForWHNF = do
    let map0 :: ML.Map Char (ForcedWHNF Int)
        map0 = ML.empty

        -- Step 1 & 2 merged
        val1= case strictlyWHNF (const (2 + 2) map0) of
                Pairy v ext -> ext

        -- Step 3. Store as a lazy thunk without the references.
        map1 = ML.insert 'a' val1 map0
    pure map1
@

val1 __has been bound by a lazy let__. Top level bound plus explicit types
in @let@ bindings will help us to avoid this.
-}

{- | Unlifted pair type. When a value of this type is bound, it will have
 already evaluated @u@.
-}
type Pairy :: UnliftedType -> LiftedType -> UnliftedType
data Pairy (u :: UnliftedType) (l :: LiftedType) :: UnliftedType where
    Pairy :: u -> l -> Pairy u l

{- | A type synonym for the unlifted pair type synonym. It contains a strict
 value and a way to extract it to a lazy/normal context.
-}
type StrictValueExtractor a = Pairy (Strict a) (Strict a -> a)

-- | A wrapper for a lifted type that makes sure to have it evaluated.
type Strict :: LiftedType -> UnliftedType
data Strict (a :: LiftedType) :: UnliftedType where
    Strict :: !a -> Strict a

{- | We don't ship the constructor of 'Strict' as it could be used to bypass
 our pushes to bind values to a name.
-}
extractStrict :: Strict a -> a
extractStrict (Strict a) = a

{- $invariantNewtypes

The invariants of @ForcedWHNF@ and @ForcedNF@ depends on the constructors
not being exported. The only way to construct these value is through the CBV
functions. Pattern matching is done via a unidirectional pattern.
-}

{- | Contains a value of type @a@ that has been forced to __W__eak __H__ead
 __N__ormal __F__orm. Constructor not exported (so no
 'Data.Coercible.coerce').
-}
newtype ForcedWHNF a = ForcedOuter a
  deriving (Show)

-- | The only way to extract the underlying value.
pattern ForcedWHNF :: forall a. a -> ForcedWHNF a
pattern ForcedWHNF a <- ForcedOuter a

{- | Contains a value of type @a@ that has been forced to __N__ormal
 __F__orm. Constructor not exported (so no 'Data.Coercible.coerce').
-}
newtype ForcedNF a = ForcedFull a

-- | The only way to extract the underlying value.
pattern ForcedNF :: forall a. a -> ForcedNF a
pattern ForcedNF a <- ForcedFull a

{- | This is a CBV function. Evaluates the argument to WHNF before
returning.
-}
strictlyWHNF :: forall a. a -> StrictValueExtractor (ForcedWHNF a)
strictlyWHNF a = Pairy (Strict (ForcedOuter a)) extractStrict

-- | This is a CBV function. Evaluates the argument to NF before returning.
strictlyNF :: forall a. NFData a => a -> StrictValueExtractor (ForcedNF a)
strictlyNF a = Pairy (Strict (ForcedFull (rnf a `seq` a))) extractStrict
