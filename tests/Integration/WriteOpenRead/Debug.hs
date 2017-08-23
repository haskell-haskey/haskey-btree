{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Integration.WriteOpenRead.Debug where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.State

import Data.Binary (Binary)
import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import qualified Data.Map as M

import GHC.Generics (Generic)

import Data.BTree.Alloc.Class
import Data.BTree.Alloc.Debug
import Data.BTree.Impure
import Data.BTree.Primitives
import qualified Data.BTree.Impure as Tree

import Integration.WriteOpenRead.Transactions

tests :: Test
tests = testGroup "WriteOpenRead.Concurrent"
    [ testProperty "debug allocator" prop_debug_allocator
    ]

data AllocatorState k v = AllocatorState {
    allocatorStatePages :: Pages
  , allocatorStateTree :: Tree k v }

prop_debug_allocator :: Property
prop_debug_allocator = forAll genTestSequence $ \(TestSequence txs) ->
    let s = AllocatorState emptyPages Tree.empty
        m = runIdentity $ evalStateT (runSeq txs) s
    in
    m `seq` True
  where
    runSeq = foldlM writeReadTest M.empty

    writeReadTest :: Map Integer TestValue
                  -> TestTransaction Integer TestValue
                  -> StateT (AllocatorState Integer TestValue)
                            Identity
                            (Map Integer TestValue)
    writeReadTest m tx = do
        openAndWrite tx
        read' <- openAndRead
        let expected = testTransactionResult m tx
        if read' == M.toList expected
            then return expected
            else error $ "error:"
                    ++ "\n    after:   " ++ show tx
                    ++ "\n    expectd: " ++ show (M.toList expected)
                    ++ "\n    got:     " ++ show read'

    openAndRead = do
        pages <- gets allocatorStatePages
        tree  <- gets allocatorStateTree
        return $ evalDebug pages (readAll tree)

    openAndWrite tx = do
        pages <- gets allocatorStatePages
        tree  <- gets allocatorStateTree

        let (tree', pages') = runDebug pages (doTx tree tx)
        put AllocatorState {
            allocatorStatePages = pages'
          , allocatorStateTree = tree' }

readAll :: (AllocM m, Key k, Value v)
        => Tree k v
        -> m [(k, v)]
readAll = Tree.toList

doTx :: (AllocM m, Key k, Value v)
     => Tree k v
     -> TestTransaction k v
     -> m (Tree k v)
doTx tree (TestTransaction actions) =
    foldl (>=>) return (map writeAction actions) tree
  where
    writeAction (Insert k v) = insertTree k v
    writeAction (Replace k v) = insertTree k v
    writeAction (Delete k) = deleteTree k

--------------------------------------------------------------------------------

-- | Value used for testing.
--
-- This value will overflow 20% of the time.
newtype TestValue = TestValue (Either Integer [Word8])
                  deriving (Eq, Generic, Show, Typeable)

instance Binary TestValue where
instance Value TestValue where

instance Arbitrary TestValue where
    arbitrary =
        TestValue <$> frequency [(80, Left <$> small), (20, Right <$> big)]
      where
        small = arbitrary
        big = arbitrary

--------------------------------------------------------------------------------
