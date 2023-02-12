module Serialize.Internal.Put where

import Data.Primitive (MutableByteArray#, Prim)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..), writeUnalignedByteArray)
import Serialize.Internal.Exts
import Serialize.Internal.Util

newtype Put :: Type where
  Put# ::
    { runPut# ::
        PE# ->
        PS# ->
        S# ->
        PR#
    } ->
    Put

newtype PE# = PE## (# MutableByteArray# RealWorld, Int# #)

newtype PS# = PutState# (# Int# #)

newtype PR# = PR## (# S#, PS# #)

pattern PE# :: MutableByteArray# RealWorld -> Int# -> PE#
pattern PE# marr# l# = PE## (# marr#, l# #)

pattern PS# :: Int# -> PS#
pattern PS# x = PutState# (# x #)

pattern PR# :: S# -> PS# -> PR#
pattern PR# s# ps# = PR## (# s#, ps# #)

{-# COMPLETE PS# #-}

{-# COMPLETE PE# #-}

{-# COMPLETE PR# #-}

incPS# :: Int# -> PS# -> PS#
incPS# i# (PS# x#) = PS# (i# +# x#)
{-# INLINE incPS# #-}

writePE# :: forall a. (Prim a, PrimUnaligned a) => PE# -> Int# -> a -> S# -> S#
writePE# (PE# marr# l#) i# x s# = case i# +# sizeOf## @a ># l# of
  1# -> error "Index out of bounds"
  _ -> writeUnalignedByteArray# marr# i# x s#
{-# INLINE writePE# #-}

instance Semigroup Put where
  Put# p1 <> Put# p2 = Put# \marr# ps# s# -> case p1 marr# ps# s# of
    PR# s# ps# -> p2 marr# ps# s#
  {-# INLINE (<>) #-}

instance Monoid Put where
  mempty = Put# \_marr# ps# s# -> PR# s# ps#
  {-# INLINE mempty #-}

unsafeWithPut :: Int -> (Primitive.MutableByteArray RealWorld -> Int -> IO ()) -> Put
unsafeWithPut (I# o#) f = Put# \(PE# marr# l#) ps@(PS# i#) s# -> case i# +# o# ># l# of
  1# -> error "Index out of bounds"
  _ -> case runIO# (f (Primitive.MutableByteArray marr#) (I# i#)) s# of
    (# s#, () #) -> PR# s# (incPS# o# ps)
{-# INLINE unsafeWithPut #-}

putPrim :: forall a. (Prim a, PrimUnaligned a) => a -> Put
putPrim x = unsafeWithPut (sizeOf' @a) \marr i -> writeUnalignedByteArray marr i x
{-# INLINE putPrim #-}
