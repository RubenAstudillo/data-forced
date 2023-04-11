{-# Language PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main (main) where

import qualified Data.Map.Lazy as ML
import Data.Elevator.Forced
import Data.Elevator (pattern Strict)
import NoThunks.Class (NoThunks(noThunks))
import Data.Maybe (isNothing)
import Test.HUnit

main :: IO ()
main = runTestTT tests >>= print

tests :: Test
tests = TestList
  [ "test1" ~: "noThunksForWHNF" ~: (== True) <$> noThunksForWHNF
  , "test2" ~: "noThunksForWHNFMaybe" ~: (== False) <$> noThunksForWHNFMaybe
  ]

noThunksForWHNF :: IO Bool
noThunksForWHNF = do
  let map0 :: ML.Map Char (ForcedWHNF Int)
      map0 = ML.empty
      val0 = strictlyWHNF (const 8 'a')
      Strict val = val0
      !map1 = ML.insert 'a' val map0
  qthunks <- noThunks ["noThunksForWHNF"] map1
  pure (isNothing qthunks)

noThunksForWHNFMaybe :: IO Bool
noThunksForWHNFMaybe = do
  let map0 :: ML.Map Char (ForcedWHNF (Maybe Int))
      map0 = ML.empty
      val0 = strictlyWHNF ((+1) <$> Just 27)
      Strict val = val0
      !map1 = ML.insert 'a' val map0
  qthunks <- noThunks ["noThunksForWHNFMaybe"] val
  pure (isNothing qthunks)
