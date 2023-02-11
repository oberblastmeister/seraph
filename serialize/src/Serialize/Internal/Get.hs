module Serialize.Internal.Get where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Serialize.Internal.Exts
import Data.Primitive
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..))
import Foreign (withForeignPtr)
import Foreign qualified
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (PlainPtr))
import Serialize.Internal.Util
import System.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Exts (mutableByteArrayContents#)

newtype Get :: Type -> Type where
  Get# ::
    { runGet# ::
        Env# ->
        GS# ->
        GR# a
    } ->
    Get a

newtype GS# = GetState# (# Int# #)

pattern GS# :: Int# -> GS#
pattern GS# x = GetState# (# x #)

newtype GR# a = GetRes# (# GS#, a #)

pattern GR# :: GS# -> a -> GR# a
pattern GR# gs# x = GetRes# (# gs#, x #)

{-# COMPLETE GR# #-}

{-# COMPLETE GS# #-}

newtype Env# = BS## (# ByteArray#, Int# #)

pattern BS# :: ByteArray# -> Int# -> Env#
pattern BS# arr# l# = BS## (# arr#, l# #)

{-# COMPLETE BS# #-}

indexBS# :: forall a. (Prim a, PrimUnaligned a) => Int# -> Env# -> a
indexBS# i# (BS# arr# l#) = case i# +# sizeOf## @a >=# l# of
  1# -> case indexUnalignedByteArray# arr# i# of
    x -> x
  _ -> error "Index out of bounds"
{-# INLINE indexBS# #-}

incGS# :: Int# -> GS# -> GS#
incGS# i# (GS# x#) = GS# (i# +# x#)
{-# INLINE incGS# #-}

instance Functor Get where
  fmap f (Get# g) = Get# \arr# gs# -> case g arr# gs# of
    GR# gs# x -> GR# gs# (f x)
  {-# INLINE fmap #-}

instance Applicative Get where
  pure x = Get# \_arr# gs# -> GR# gs# x
  {-# INLINE pure #-}

  Get# g1 <*> Get# g2 = Get# \arr# gs# -> case g1 arr# gs# of
    GR# gs# f -> case g2 arr# gs# of
      GR# gs# x -> GR# gs# (f x)
  {-# INLINE (<*>) #-}

instance Monad Get where
  Get# g >>= fm = Get# \arr# gs# -> case g arr# gs# of
    GR# gs# x -> runGet# (fm x) arr# gs#
  {-# INLINE (>>=) #-}

unpackByteString# :: ByteString -> (# ByteArray#, Int#, Int# #)
unpackByteString# bs@(B.Internal.PS fp@(ForeignPtr _ fpc) o l) =
  let res = unsafeDupablePerformIO $ withForeignPtr fp $ \p -> case fpc of
        PlainPtr marr -> do
          let base = Ptr (mutableByteArrayContents# marr)
              off = p `Foreign.minusPtr` base
          arr <- unsafeFreezeByteArray $ MutableByteArray marr
          pure (arr, off, off + l + o)
        _ -> case B.copy bs of
          B.Internal.PS fp@(ForeignPtr _ fpc) o l -> withForeignPtr fp $ \p -> case fpc of
            PlainPtr marr -> do
              let base = Ptr (mutableByteArrayContents# marr)
                  off = p `Foreign.minusPtr` base
              arr <- unsafeFreezeByteArray $ MutableByteArray marr
              pure (arr, off, off + l + o)
            _ -> error "should be PlainPtr"
   in case res of
        (ByteArray arr, I# off, I# len) -> (# arr, off, len #)
{-# INLINE unpackByteString# #-}
