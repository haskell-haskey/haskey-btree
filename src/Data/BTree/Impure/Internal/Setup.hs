-- | Setup of an impure B+-tree
module Data.BTree.Impure.Internal.Setup where

minFanout :: Int
minFanout = 2

minLeafItems :: Int
minLeafItems = minFanout

minIdxKeys :: Int
minIdxKeys = minFanout - 1
