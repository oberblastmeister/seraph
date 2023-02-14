module Serialize.Internal.Get where

import Control.Exception qualified as Exception
import Data.Primitive
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..), indexUnalignedByteArray)
import Serialize.Internal.Exts
import Serialize.Internal.Util

newtype Get :: Type -> Type where
  Get# ::
    { runGet# ::
        GE# ->
        GS# ->
        S# ->
        GR# a
    } ->
    Get a

newtype GS# = GetState# (# Int# #)

pattern GS# :: Int# -> GS#
pattern GS# x = GetState# (# x #)

newtype GR# a = GR## (# State# RealWorld, GS#, a #)

pattern GR# :: State# RealWorld -> GS# -> a -> GR# a
pattern GR# s# gs# x = GR## (# s#, gs#, x #)

{-# COMPLETE GR# #-}

{-# COMPLETE GS# #-}

newtype GE# = GE## (# ByteArray#, Int# #)

pattern GE# :: ByteArray# -> Int# -> GE#
pattern GE# arr# l# = GE## (# arr#, l# #)

{-# COMPLETE GE# #-}

indexGE# :: forall a. (Prim a, PrimUnaligned a) => Int# -> GE# -> a
indexGE# i# (GE# arr# l#) = case i# +# sizeOf## @a ># l# of
  1# -> Exception.throw $ IndexOutOfBounds (I# (i# +# sizeOf## @a)) (I# l#)
  _ -> case indexUnalignedByteArray# arr# i# of
    !x -> x
{-# INLINE indexGE# #-}

incGS# :: Int# -> GS# -> GS#
incGS# i# (GS# x#) = GS# (i# +# x#)
{-# INLINE incGS# #-}

instance Functor Get where
  fmap f (Get# g) = Get# \arr# gs# s# -> case g arr# gs# s# of
    GR# s# gs# x -> GR# s# gs# (f x)
  {-# INLINE fmap #-}

instance Applicative Get where
  pure x = Get# \_arr# gs# s# -> GR# s# gs# x
  {-# INLINE pure #-}

  Get# g1 <*> Get# g2 = Get# \arr# gs# s# -> case g1 arr# gs# s# of
    GR# s# gs# f -> case g2 arr# gs# s# of
      GR# s# gs# x -> GR# s# gs# (f x)
  {-# INLINE (<*>) #-}

instance Monad Get where
  Get# g >>= fm = Get# \arr# gs# s# -> case g arr# gs# s# of
    GR# s# gs# x -> runGet# (fm x) arr# gs# s#
  {-# INLINE (>>=) #-}

data GetException
  = IndexOutOfBounds !Int !Int
  | InvalidSumTag !Int !Int
  deriving (Eq)

instance Show GetException where
  show (IndexOutOfBounds i l) = "Index out of bounds: " ++ show i ++ " >= " ++ show l
  show (InvalidSumTag cur tag) = "Invalid sum tag: " ++ show cur ++ " != " ++ show tag

instance Exception.Exception GetException

unsafeWithGet :: Int -> (Primitive.ByteArray -> Int -> a) -> Get a
unsafeWithGet (I# o#) f = Get# \(GE# arr# l#) gs@(GS# i#) s# -> case i# +# o# ># l# of
  1# -> Exception.throw $ IndexOutOfBounds (I# (i# +# o#)) (I# l#)
  _ -> case f (Primitive.ByteArray arr#) (I# i#) of
    !x -> GR# s# (incGS# o# gs) x
{-# INLINE unsafeWithGet #-}

getPrim :: forall a. (Prim a, PrimUnaligned a) => Get a
getPrim = unsafeWithGet (sizeOf' @a) indexUnalignedByteArray
{-# INLINE getPrim #-}
