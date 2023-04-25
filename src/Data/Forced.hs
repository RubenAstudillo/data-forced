{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Data.Forced
  ( -- * How to use this library
    -- | You should use the following imports
    --
    -- @
    --   import qualified Data.Forced as DF
    --   import Data.Forced hiding (pure, fmap, (<*>), return, (>>=), (>>))
    -- @

    -- ** Design the type of your long lived data structure
    -- $dataTypeUserDesign

    -- ** Construct values on the @Demand@ monad
    -- $useMonad

    -- * Newtypes to be used to specify how evaluated a type should be
    ForcedWHNF (ForcedWHNF)
  , ForcedNF (ForcedNF)
    -- * Monadic environment to execute the needed demands.
  , Demand
  , demandWHNF
  , demandNF
  , extractDemand

    -- * Qualified Do support.
    -- | These are available to construct value by hand. But they clash with
    -- 'P.Functor', 'P.Applicative' and 'P.Monad' functions. We cannot
    -- provide instances to those classes as the 'Demand' monad is
    -- 'UnliftedType' kinded. But using @-XQualifiedDo@, GHC will pick up
    -- these names and use it on a @DF.do@ notation that does the right
    -- thing.
  , fmap
  , pure
  , (<*>)
  , return
  , (>>=)
  , (>>)
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Elevator (LiftedType, UnliftedType)
import Prelude ()
import qualified Prelude as P

{- $dataTypeUserDesign
The main way this library helps you avoid leaks is by specifying the types
of your long lived data structures. They should contain new demands on the
type variables.

@
import Data.Map.Lazy -- it is fine, really.
import Data.Vector

-- On insertion of the lazy map, the keys and the values will evaluated.
type MyMap a = Map (ForcedWHNF Char) (ForcedNF (Maybe (Vector Int)))

-- On top, prompt removal of deleted elements.
type MyMap2 a = ForcedWHNF (Map (ForcedWHNF Char) (ForcedNF (Maybe (Vector Int))))
@

This way it will be a type error to store a thunk that is keeping references
alive.
-}

{- $useMonad
We use the 'Demand' monad to construct values with the correct strictness.
You either construct the values by hand, but it is better to use the
@-XQualifiedDo@ extension.

The main functions to keep in mind on this monad are: 'demandWHNF' and
'demandNF'.

Once you have the value specified, you need to extract it to the IO
environment. Hopefully this will be close to main where your long lived data
should be stored. We do this as is the obvious sequence point, so from the
PoV of the rest of the program, the action is visible on the default lifted
environment.

The ideal code piece looks like this:

@
\{\-\# Language QualifiedDo \#\-\}

import qualified Data.Forced as DF
import Data.Forced hiding (pure, fmap, (\<*\>), return, (>>=), (>>))
import Data.Map.Lazy qualified as ML

noThunksForWHNF :: IO ()
noThunksForWHNF = do
    -- map0 actually evaluated on here.
    let map0 :: Demand (ML.Map Char (ForcedWHNF Int))
        map0 = DF.do
          v <- demandWHNF (const (2 + (2 :: Int)) \'a\')
          DF.pure $ ML.insert \'a\' v ML.empty

    map1 <- extractDemand map0
    go (ML.lookup \'a\' map1)

-- pattern matching for de-structuring, no construction allowed.
go :: ForcedWHNF Int -> IO ()
go (ForcedWHNF i) =  print i
@
-}

{- | A strict identity monad of 'UnliftedType' kind. To be used via
@-XQualifiedDo@.
-}
type Demand :: LiftedType -> UnliftedType
data Demand (a :: LiftedType) :: UnliftedType where
    Demand :: a -> Demand a

{- | We don't ship the constructor of 'Demand'. The only way to extract a
'Demand' is to sequence to a know point on 'P.IO'. From the PoV of the rest
of the program, the tagged values with t'ForcedWHNF' or t'ForcedNF'
will have been demanded.
-}
extractDemand :: Demand a -> P.IO a
extractDemand (Demand a) = P.pure a

{- $invariantNewtypes

The invariants of @ForcedWHNF@ and @ForcedNF@ depends on the constructors
not being exported. The only way to construct these value is through the CBV
functions of the 'Demand' type. Pattern matching is done via a
unidirectional pattern.
-}

{- | Contains a value of type @a@ that has been forced to __W__eak __H__ead
 __N__ormal __F__orm. Constructor not exported (so no
 'Data.Coercible.coerce').
-}
newtype ForcedWHNF a = ForcedOuter a

instance P.Show a => P.Show (ForcedWHNF a) where
  show (ForcedOuter a) = "ForcedWHNF " P.++ P.show a

-- | The only way to extract the underlying value.
pattern ForcedWHNF :: forall a. a -> ForcedWHNF a
pattern ForcedWHNF a <- ForcedOuter a

{- | Contains a value of type @a@ that has been forced to __N__ormal
 __F__orm. Constructor not exported (so no 'Data.Coercible.coerce').
-}
newtype ForcedNF a = ForcedFull a

instance P.Show a => P.Show (ForcedNF a) where
  show (ForcedFull a) = "ForcedNF " P.++ P.show a

-- | The only way to extract the underlying value.
pattern ForcedNF :: forall a. a -> ForcedNF a
pattern ForcedNF a <- ForcedFull a

{- | This is a CBV function. Evaluates the argument to WHNF before
returning.
-}
demandWHNF :: forall a. a -> Demand (ForcedWHNF a)
demandWHNF a = a `P.seq` Demand (ForcedOuter a)

-- | This is a CBV function. Evaluates the argument to NF before returning.
demandNF :: forall a. NFData a => a -> Demand (ForcedNF a)
demandNF a = rnf a `P.seq` Demand (ForcedFull a)

{- $qualifiedDoSupport

There are no 'P.Functor', 'P.Applicative' or 'P.Monad' classes for
'UnliftedType' types yet. This package is not the right place to define
them. We can get @do@ notation using @-XQualifiedDo@.
-}
-- | 'P.fmap' analogue for 'Demand's which are of the 'UnliftedType' kind.
fmap :: (a -> b) -> Demand a -> Demand b
fmap f (Demand a) = Demand (f a)

-- | Places __no__ demand on the value. 'P.pure' analogue for 'Demand's
-- which are of the 'UnliftedType' kind.
pure :: a -> Demand a
pure a = Demand a

-- | 'P.<*>' analogue for 'Demand's which are of the 'UnliftedType' kind.
(<*>) :: Demand (a -> b) -> Demand a -> Demand b
(<*>) (Demand fun) (Demand fa) = Demand (fun fa)

-- | 'P.return' analogue for 'Demand's which are of the 'UnliftedType' kind.
-- Same as 'pure'.
return :: a -> Demand a
return = pure

-- | 'P.>>=' analogue for 'Demand's which are of the 'UnliftedType' kind.
(>>=) :: Demand a -> (a -> Demand b) -> Demand b
(Demand ma) >>= f = f ma

-- | 'P.>>' analogue for 'Demand's which are of the 'UnliftedType' kind.
(>>) :: Demand a -> Demand b -> Demand b
(>>) fa fb = fa >>= (\_ -> fb)
