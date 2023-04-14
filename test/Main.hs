{-# OPTIONS_GHC -Wno-unused-binds #-}

module Main (main) where

import Control.Exception (ErrorCall (..), catch)
import Data.Forced
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
    let map0 :: ML.Map Char (ForcedWHNF Int)
        map0 = ML.empty
        val0 :: StrictValueExtractor (ForcedWHNF Int)
        !val0@(Pairy val1 ext) = strictlyWHNF (error "argument evaluated")
        map1 = ML.insert 'a' (ext val1) map0
    pure ()

noThunksForWHNF1 :: IO ()
noThunksForWHNF1 = do
    let map0 :: ML.Map Char (ForcedWHNF Int)
        map0 = ML.empty
        val0 :: StrictValueExtractor (ForcedWHNF Int)
        val0 = strictlyWHNF (error "argument evaluated")
        val1 = case val0 of Pairy v ext -> ext v
        map1 = ML.insert 'a' val1 map0
    pure ()

noThunksForWHNF2 :: IO ()
noThunksForWHNF2 = do
    let map0 :: ML.Map Char (ForcedWHNF Int)
        map0 = ML.empty
        val1 = case strictlyWHNF (error "argument evaluated") of Pairy v ext -> ext v
        map1 = ML.insert 'a' val1 map0
    pure ()

thunksForWHNFMaybe :: IO ()
thunksForWHNFMaybe = do
    let map0 :: ML.Map Char (ForcedWHNF (Maybe Int))
        map0 = ML.empty
        val0 :: StrictValueExtractor (ForcedWHNF (Maybe Int))
        val0 = strictlyWHNF (const (Just (error "argument evaluated")) 'a')
        val1 = case val0 of Pairy v ext -> ext v
        map1 = ML.insert 'a' val1 map0
    pure ()

noThunksForNFMaybe :: IO ()
noThunksForNFMaybe = do
    let map0 :: ML.Map Char (ForcedNF (Maybe Int))
        map0 = ML.empty
        val0 :: StrictValueExtractor (ForcedNF (Maybe Int))
        val0 = strictlyNF ((+ 1) <$> Just (error "argument evaluated" :: Int))
        val = case val0 of Pairy st ext -> ext st
        map1 = ML.insert 'a' val map0
    pure ()
