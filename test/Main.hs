{-# Language QualifiedDo #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}

module Main (main) where

import Control.Exception (ErrorCall (..), catch)
import qualified Data.Forced as DF
import Data.Forced hiding (pure, fmap, (<*>), return, (>>=), (>>))
import Data.Map.Lazy qualified as ML
import Test.HUnit (Counts, Test (TestList), runTestTT, (~:))

main :: IO Counts
main = runTestTT tests

tests :: Test
tests =
    TestList
        [ "test1" ~: "noThunksForWHNF" ~: errorCalledWithMsg "argument evaluated" noThunksForWHNF
        , "test2" ~: "thunksForWHNFMaybe" ~: thunksForWHNFMaybe
        , "test3" ~: "noThunksForNFMaybe" ~: errorCalledWithMsg "argument evaluated" noThunksForNFMaybe
        ]

errorCalledWithMsg :: String -> IO a -> IO Bool
errorCalledWithMsg msg io =
    catch (False <$ io) (\(ErrorCall msg1) -> pure (msg == msg1))

noThunksForWHNF :: IO ()
noThunksForWHNF = do
    let map0 :: Demand (ML.Map Char (ForcedWHNF Int))
        map0 = DF.do
          v <- demandWHNF (error "argument evaluated")
          DF.pure $ ML.insert 'a' v ML.empty
    map1 <- extractDemand map0
    pure ()

-- | Should not fail.
thunksForWHNFMaybe :: IO ()
thunksForWHNFMaybe = do
    let map0 :: Demand (ML.Map Char (ForcedWHNF (Maybe Int)))
        map0 = DF.do
          v <- demandWHNF (const (Just (error "argument evaluated")) 'a')
          DF.pure $ ML.insert 'a' v ML.empty
    map1 <- extractDemand map0
    pure ()

noThunksForNFMaybe :: IO ()
noThunksForNFMaybe = do
    let map0 :: Demand (ML.Map Char (ForcedNF (Maybe Int)))
        map0 = DF.do
          v <- demandNF (const (Just (error "argument evaluated")) 'a')
          DF.pure $ ML.insert 'a' v ML.empty
    map1 <- extractDemand map0
    pure ()
