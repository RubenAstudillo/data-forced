`data-forced` is a tiny library to remove references to unintended values on
long lived data structures. It targets _liveness leaks_ on the
classification done on
[this](https://epicandmonicisnotiso.blogspot.com/2023/04/how-to-avoid-correctness-space-leaks-on.html#orgbb37ed8)
blog post.

You should consider using it on data structures that are long lived. These
are structured that are either:

- Stored and retrieved via `get`/`put` on a state monad close to `main`.
- Stored on  `IORef`, `MVar`, `TVar` or related mutable locations.
- Passed recursively as an argument on a event loop.

# The big idea

*We should not throw the information that a value was forced to WHNF. We neither
should be required to remember to put a `seq` or BangPattern at the use
site to maintain this invariant*.

We accomplish this using two newtypes:

- `newtype ForcedWHNF a = ForcedWHNF a`
- `newtype ForcedNF a = ForcedNF a`

that don't have their constructors exported. Only unidirectional patterns
are provided so you can de-structure them.

The **only** way to construct values of these types is via two CBVs
functions:

- `strictlyWHNF :: forall a. a -> Strict (ForcedWHNF a)`
- `strictlyNF :: forall a. NFData a => a -> Strict (ForcedNF a)`

The `Strict` data type is inspired by its analog on
[`data-elevator`](https://hackage.haskell.org/package/data-elevator). It is
a `UnliftedDatatype`, which means it cannot contain bottoms. The dynamic
semantics of bound values of unlifted data types is that **before being
bound, these are evaluated to WHNF**. It is a BangPattern you cannot forget!

Once you obtain the `Strict (ForcedWHNF a)` value, you de-structurate it to
obtain the *lifted* `ForceWHNF a` and store that. You are guaranteed that
the references necessary to force it to WHNF are now free. Hopefully you
will store it on a data structure that requires values to have been forced
before like

``` haskell
    import Data.Map.Lazy

    type Accum1 = Map Char (ForcedWHNF Int)
    type Accum2 = Map Char (ForcedNF (Maybe Text))
```

# How to use it

You should tag your long lived data structures with the correct type as in
the previous example. It is less ergonomic than bang patterns, but you
cannot forget to put them at the call site, parse not validate.

``` haskell

import Data.Map.Lazy as ML -- Spine strict

-- No references on added leafs even though it is a lazy map.
basicEvent :: ML.Map Char (ForcedWHNF Int) -> IO (ML.Map Char (ForcedWHNF Int))
basicEvent map0 = do
  let val0 :: Strict (ForcedWHNF Int)
      -- val0 = strictlyWHNF (error "argument evaluated") -- would fail
      val0 = strictlyWHNF (2 + 2)
      -- CBV function, 2 + 2 reduced before val0 is bound.
      Strict val1 = val0  -- De-structure
      map1 = ML.insert 'a' val1 map0
  pure map1

-- Same as before, but not references to values that were deleted and not forced.
basicEventPromptDeletion
  :: Strict (ML.Map Char (ForcedNF (Maybe Int)))
  -> IO (Strict (ML.Map Char (ForcedNF (Maybe Int))))
basicEventPromptDeletion (Strict map0) = do -- map0 evaluated before being bound.
  let val0 :: Strict (ForcedNF (Maybe Int))
      val0 = strictlyWHNF ((+1) <$> Just 28)
      Strict val1 = val0  -- De-structure
      map1 :: Strict (ML.Map Char (ForcedNF (Maybe Int)))
      map1 = Strict (ML.insert 'a' val1 map0) -- forced when bound as map1
  pure map1
```

# Acknowledgment

Sebastian Graf (sgraf812) should take all the credit. I was inspired by his
`data-elevator` package that solves a sightly different problem. He infact
pushed the proposal for unlifted data types and made the work to integrate
them to GHC.
