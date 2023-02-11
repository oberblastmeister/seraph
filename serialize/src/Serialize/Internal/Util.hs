{-# LANGUAGE AllowAmbiguousTypes #-}

module Serialize.Internal.Util
  ( S#,
    sizeOf##,
    BS#,
    pattern BS#,
    indexBS#,
  )
where

import Data.Primitive (ByteArray#, Prim, sizeOf#)
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..))
import GHC.Exts (Int#, RealWorld, State#, (>=#))

newtype BS# = BS## (# ByteArray#, Int# #)

pattern BS# :: ByteArray# -> Int# -> BS#
pattern BS# arr# l# = BS## (# arr#, l# #)

{-# COMPLETE BS# #-}

indexBS# :: PrimUnaligned a => Int# -> BS# -> a
indexBS# i# (BS# arr# l#) = case i# >=# l# of
  1# -> case indexUnalignedByteArray# arr# i# of
    x -> x
  _ -> error "Index out of bounds"
{-# INLINE indexBS# #-}

type S# = State# RealWorld

sizeOf## :: forall a. Prim a => Int#
sizeOf## = sizeOf# (undefined :: a)
{-# INLINE sizeOf## #-}
