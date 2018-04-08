module Properties.Impure.Insert where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad ((>=>))

import Data.Int
import Data.Word (Word8)
import qualified Data.Map as M

import Data.BTree.Alloc.Debug
import qualified Data.BTree.Impure as B

tests :: Test
tests = testGroup "Impure.Insert"
    [ testProperty "insertMany" prop_insertMany
    , testProperty "insertOverflows" prop_insertOverflows
    ]

prop_insertMany :: [(Int64, Integer)] -> [(Int64, Integer)] -> Bool
prop_insertMany xs ys = ty1 == ty2
  where
    tx  = insertAll xs B.empty

    ty1 = evalDebug emptyPages $
              tx
              >>= insertAll ys
              >>= B.toList

    ty2 = evalDebug emptyPages $
              tx
              >>= B.insertMany (M.fromList ys)
              >>= B.toList

    insertAll kvs = foldl (>=>) return (map (uncurry B.insert) kvs)

prop_insertOverflows :: M.Map Int64 [Word8] -> Bool
prop_insertOverflows kvs
    | v <- evalDebug emptyPages $
        B.insertMany kvs B.empty
        >>= B.toList
    = v == M.toList kvs
