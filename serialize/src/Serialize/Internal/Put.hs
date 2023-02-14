module Serialize.Internal.Put where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad.ST (ST, stToIO)
import Data.Primitive (MutableByteArray#, Prim)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..), writeUnalignedByteArray)
import GHC.Exts (getSizeofMutableByteArray#)
import Serialize.Internal.Exts
import Serialize.Internal.Util

-- Note: Because everything is unboxed there are a lot more arguments to the function
-- This makes recursive calls more expensive
newtype Put :: Type where
  Put# ::
    { runPut# ::
        PE# ->
        PS# ->
        S# ->
        PR#
    } ->
    Put

newtype PE# = PE## (# MutableByteArray# RealWorld #)

newtype PS# = PutState# (# Int# #)

newtype PR# = PR## (# S#, PS# #)

pattern PE# :: MutableByteArray# RealWorld -> PE#
pattern PE# marr# = PE## (# marr# #)

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

instance Semigroup Put where
  Put# p1 <> Put# p2 = Put# \marr# ps# s# -> case p1 marr# ps# s# of
    PR# s# ps# -> p2 marr# ps# s#
  {-# INLINE (<>) #-}

instance Monoid Put where
  mempty = Put# \_marr# ps# s# -> PR# s# ps#
  {-# INLINE mempty #-}

data PutException
  = IndexOutOfBounds !Int !Int
  deriving (Show, Eq, Ord)

instance Exception PutException

throwPut :: Exception e => e -> Put
throwPut e = Put# \_ ps# s# -> case runIO# (Exception.throwIO e) s# of
  (# s#, _ #) -> PR# s# ps#
{-# NOINLINE throwPut #-}

-- Even more unsafe!
-- Used because ByteString stuff uses IO instead of ST
-- Just don't shoot the missiles in here!
unsafeWithPutIO :: Int -> (Primitive.MutableByteArray RealWorld -> Int -> IO ()) -> Put
unsafeWithPutIO (I# o#) f = Put# \(PE# marr#) ps@(PS# i#) s# -> case getSizeofMutableByteArray# marr# s# of
  (# s#, l# #) -> case i# +# o# ># l# of
    1# -> case runIO# (Exception.throwIO $ IndexOutOfBounds (I# (i# +# o#)) (I# l#)) s# of
      (# s#, _ #) -> PR# s# ps
    _ -> case runIO# (f (Primitive.MutableByteArray marr#) (I# i#)) s# of
      (# s#, () #) -> PR# s# (incPS# o# ps)
{-# INLINE unsafeWithPutIO #-}

unsafeWithPut :: Int -> (forall s. Primitive.MutableByteArray s -> Int -> ST s ()) -> Put
unsafeWithPut o f = unsafeWithPutIO o (\marr i -> stToIO (f marr i))
{-# INLINE unsafeWithPut #-}

putPrim :: forall a. (Prim a, PrimUnaligned a) => a -> Put
putPrim x = unsafeWithPut (sizeOf' @a) \marr i -> writeUnalignedByteArray marr i x
{-# INLINE putPrim #-}
