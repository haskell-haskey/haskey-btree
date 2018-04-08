{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Algorithms related to looking up key-value pairs in an impure B+-tree.
module Data.BTree.Impure.Internal.Lookup where

import Prelude hiding (lookup)

import qualified Data.Map as M

import Control.Applicative ((<$>))

import Data.BTree.Alloc.Class
import Data.BTree.Impure.Internal.Overflow
import Data.BTree.Impure.Internal.Structures
import Data.BTree.Primitives

--------------------------------------------------------------------------------

lookupRec :: forall m height key val. (AllocReaderM m, Key key, Value val)
    => key
    -> Height height
    -> NodeId height key val
    -> m (Maybe val)
lookupRec k = fetchAndGo
  where
    fetchAndGo :: forall hgt.
        Height hgt ->
        NodeId hgt key val ->
        m (Maybe val)
    fetchAndGo hgt nid =
        readNode hgt nid >>= go hgt

    go :: forall hgt.
        Height hgt ->
        Node hgt key val ->
        m (Maybe val)
    go hgt (Idx children) = do
        let (_ctx,childId) = valView k children
        fetchAndGo (decrHeight hgt) childId
    go _hgt (Leaf items) =
        case M.lookup k items of Nothing -> return Nothing
                                 Just v  -> Just <$> fromLeafValue v

-- | Lookup a value in an impure B+-tree.
lookup :: forall m key val. (AllocReaderM m, Key key, Value val)
    => key
    -> Tree key val
    -> m (Maybe val)
lookup k tree
    | Tree
      { treeHeight = height
      , treeRootId = Just rootId
      } <- tree
    = lookupRec k height rootId
    | Tree
      { treeRootId = Nothing
      } <- tree
    = return Nothing

--------------------------------------------------------------------------------

-- | The minimal key of the map, returns 'Nothing' if the map is empty.
lookupMin :: (AllocReaderM m, Key key, Value val)
              => Tree key val
              -> m (Maybe (key, val))
lookupMin tree
    | Tree { treeRootId = Nothing } <- tree = return Nothing
    | Tree { treeHeight = height
           , treeRootId = Just rootId } <- tree
    = lookupMinRec height rootId
  where
    lookupMinRec :: (AllocReaderM m, Key key, Value val)
                 => Height height
                 -> NodeId height key val
                 -> m (Maybe (key, val))
    lookupMinRec h nid = readNode h nid >>= \case
        Idx children -> let (_, childId) = valViewMin children in
                        lookupMinRec (decrHeight h) childId
        Leaf items -> case lookupMin' items of
            Nothing -> return Nothing
            Just (k, v) -> do
                v' <- fromLeafValue v
                return $ Just (k, v')

    lookupMin' m | M.null m  = Nothing
                 | otherwise = Just $! M.findMin m

-- | The maximal key of the map, returns 'Nothing' if the map is empty.
lookupMax :: (AllocReaderM m, Key key, Value val)
              => Tree key val
              -> m (Maybe (key, val))
lookupMax tree
    | Tree { treeRootId = Nothing } <- tree = return Nothing
    | Tree { treeHeight = height
           , treeRootId = Just rootId } <- tree
    = lookupMaxRec height rootId
  where
    lookupMaxRec :: (AllocReaderM m, Key key, Value val)
                 => Height height
                 -> NodeId height key val
                 -> m (Maybe (key, val))
    lookupMaxRec h nid = readNode h nid >>= \case
        Idx children -> let (_, childId) = valViewMax children in
                        lookupMaxRec (decrHeight h) childId
        Leaf items -> case lookupMax' items of
            Nothing -> return Nothing
            Just (k, v) -> do
                v' <- fromLeafValue v
                return $ Just (k, v')

    lookupMax' m | M.null m  = Nothing
                 | otherwise = Just $! M.findMax m

--------------------------------------------------------------------------------
