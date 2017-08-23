{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Test.Framework (Test, defaultMain)

import qualified Integration.WriteOpenRead.Debug

tests :: [Test]
tests =
    [ Integration.WriteOpenRead.Debug.tests
    ]

main :: IO ()
main = defaultMain tests
