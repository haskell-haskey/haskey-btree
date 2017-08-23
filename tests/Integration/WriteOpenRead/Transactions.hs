{-# LANGUAGE RecordWildCards #-}
module Integration.WriteOpenRead.Transactions where

import Test.QuickCheck

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.State

import Data.List (inits)
import Data.Map (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------

newtype TestSequence k v = TestSequence [TestTransaction k v]
                         deriving (Show)

data TransactionSetup = TransactionSetup { sequenceInsertFrequency :: !Int
                                         , sequenceReplaceFrequency :: !Int
                                         , sequenceDeleteFrequency :: !Int }
                      deriving (Show)

deleteHeavySetup :: TransactionSetup
deleteHeavySetup = TransactionSetup { sequenceInsertFrequency = 35
                                    , sequenceReplaceFrequency = 20
                                    , sequenceDeleteFrequency = 45 }

insertHeavySetup :: TransactionSetup
insertHeavySetup = TransactionSetup { sequenceInsertFrequency = 12
                                    , sequenceReplaceFrequency = 4
                                    , sequenceDeleteFrequency = 4 }

genTransactionSetup :: Gen TransactionSetup
genTransactionSetup =
    frequency [(45, return deleteHeavySetup),
               (45, return insertHeavySetup)]

newtype TestTransaction k v = TestTransaction [TestAction k v]
                         deriving (Show)

testTransactionResult :: Ord k => Map k v -> TestTransaction k v -> Map k v
testTransactionResult m (TestTransaction actions)
    = foldl (flip doAction) m actions

data TestAction k v = Insert k v
                    | Replace k v
                    | Delete k
                    deriving (Show)

doAction :: Ord k => TestAction k v -> Map k v -> Map k v
doAction action m
    | Insert  k v <- action = M.insert k v m
    | Replace k v <- action = M.insert k v m
    | Delete  k   <- action = M.delete k m

genTestTransaction :: (Ord k, Arbitrary k, Arbitrary v) => Map k v -> TransactionSetup -> Gen (TestTransaction k v, Map k v)
genTestTransaction db TransactionSetup{..} = sized $ \n -> do
    k            <- choose (0, n)
    (m, actions) <- execStateT (replicateM k next) (db, [])
    tx           <- TestTransaction <$> pure (reverse actions)
    return (tx, m)
  where
    genAction :: (Ord k, Arbitrary k, Arbitrary v)
              => Map k v
              -> Gen (TestAction k v)
    genAction m
        | M.null m = genInsert
        | otherwise = frequency [(sequenceInsertFrequency,    genInsert   ),
                                 (sequenceReplaceFrequency,   genReplace m),
                                 (sequenceDeleteFrequency,    genDelete m )]

    genInsert :: (Arbitrary k, Arbitrary v) => Gen (TestAction k v)
    genInsert = Insert <$> arbitrary <*> arbitrary
    genReplace m = Replace <$> elements (M.keys m) <*> arbitrary
    genDelete m = Delete <$> elements (M.keys m)

    next :: (Ord k, Arbitrary k, Arbitrary v)
         => StateT (Map k v, [TestAction k v]) Gen ()
    next = do
        (m, actions) <- get
        action <- lift $ genAction m
        put (doAction action m, action:actions)

shrinkTestTransaction :: (Ord k, Arbitrary k, Arbitrary v)
                   => TestTransaction k v
                   -> [TestTransaction k v]
shrinkTestTransaction (TestTransaction actions) = map TestTransaction (init (inits actions))

genTestSequence :: (Ord k, Arbitrary k, Arbitrary v) => Gen (TestSequence k v)
genTestSequence = sized $ \n -> do
    k <- choose (0, n)
    (_, txs) <- execStateT (replicateM k next) (M.empty, [])
    return $ TestSequence (reverse txs)
  where
    next :: (Ord k, Arbitrary k, Arbitrary v)
         => StateT (Map k v, [TestTransaction k v]) Gen ()
    next = do
        (m, txs) <- get
        (tx, m') <- lift $ genTransactionSetup >>= genTestTransaction m
        put (m', tx:txs)

shrinkTestSequence :: (Ord k, Arbitrary k, Arbitrary v)
                   => TestSequence k v
                   -> [TestSequence k v]
shrinkTestSequence (TestSequence txs) = map TestSequence (shrinkList shrinkTestTransaction txs)
