{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Serialize.Internal.Prim where

import Data.Primitive
import Data.Primitive.ByteArray.Unaligned
import Serialize.Internal.Get
import Serialize.Internal.Put
import Serialize.Internal.Util

putPrim :: forall a. (Prim a, PrimUnaligned a) => a -> Put
putPrim x = Put# \e# ps@(PS# i#) s# -> case writeEnv# e# i# x s# of
  s# -> (# s#, incPS# (sizeOf## @a) ps #)
{-# INLINE putPrim #-}

getPrim :: forall a. (Prim a, PrimUnaligned a) => Get a
getPrim = Get# \bs# gs@(GS# i#) -> case indexBS# i# bs# of
  x -> GR# (incGS# (sizeOf## @a) gs) x
{-# INLINE getPrim #-}
