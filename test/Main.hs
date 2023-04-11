{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unused-binds #-}
module Main (main) where

import qualified Data.Map.Lazy as ML
import Data.Elevator.Forced
import Test.HUnit ( (~:), runTestTT, Counts, Test(TestList) )
import Control.Exception (catch, ErrorCall (..))

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList
  [ "test1" ~: "noThunksForWHNF" ~: errorCalledWithMsg "argument evaluated" noThunksForWHNF
  , "test2" ~: "thunksForWHNFMaybe" ~: thunksForWHNFMaybe
  , "test3" ~: "noThunksForNFMaybe" ~: errorCalledWithMsg "argument evaluated" noThunksForNFMaybe
  ]

errorCalledWithMsg :: String -> IO a -> IO Bool
errorCalledWithMsg msg io =
  catch (False <$ io) (\(ErrorCall msg1) -> pure (msg == msg1))

noThunksForWHNF :: IO ()
noThunksForWHNF = do
  let map0 :: ML.Map Char (ForcedWHNF Int)
      map0 = ML.empty
      val0 :: Strict (ForcedWHNF Int)
      val0 = strictlyWHNF (error "argument evaluated")
      Strict val1 = val0
      map1 = ML.insert 'a' val1 map0
  pure ()

thunksForWHNFMaybe :: IO ()
thunksForWHNFMaybe = do
  let map0 :: ML.Map Char (ForcedWHNF (Maybe Int))
      map0 = ML.empty
      val0 :: Strict (ForcedWHNF (Maybe Int))
      val0 = strictlyWHNF (const (Just (error "argument evaluated")) 'a')
      Strict val1 = val0
      map1 = ML.insert 'a' val1 map0
  pure ()

noThunksForNFMaybe :: IO ()
noThunksForNFMaybe = do
  let map0 :: ML.Map Char (ForcedNF (Maybe Int))
      map0 = ML.empty
      val0 :: Strict (ForcedNF (Maybe Int))
      val0 = strictlyNF ((+1) <$> Just (error "argument evaluated" :: Int))
      Strict val = val0
      map1 = ML.insert 'a' val map0
  pure ()
