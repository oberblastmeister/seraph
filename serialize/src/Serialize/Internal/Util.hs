{-# LANGUAGE AllowAmbiguousTypes #-}

module Serialize.Internal.Util
  ( sizeOf##,
    unI#,
    unW#,
    (<!$!>),
    unpackByteString#,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.Primitive
import Foreign (withForeignPtr)
import Foreign qualified
import GHC.Exts (mutableByteArrayContents#)
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (PlainPtr))
import Serialize.Internal.Exts
import System.IO.Unsafe (unsafeDupablePerformIO)

sizeOf## :: forall a. Prim a => Int#
sizeOf## = sizeOf# (undefined :: a)
{-# INLINE sizeOf## #-}

unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}

unW# :: Word -> Word#
unW# (W# w#) = w#
{-# INLINE unW# #-}

(<!$!>) :: Monad m => (a -> b) -> m a -> m b
f <!$!> m = do
  !x <- m
  pure $! f x
{-# INLINE (<!$!>) #-}

unpackByteString# :: ByteString -> (# ByteArray#, Int#, Int# #)
unpackByteString# bs@(B.Internal.PS fp@(ForeignPtr _ fpc) o l) =
  let res = unsafeDupablePerformIO $ withForeignPtr fp $ \p -> case fpc of
        PlainPtr marr -> do
          let base = Ptr (mutableByteArrayContents# marr)
              off = p `Foreign.minusPtr` base
          arr <- unsafeFreezeByteArray $ MutableByteArray marr
          pure (arr, off + o, off + o + l)
        _ -> case B.copy bs of
          B.Internal.PS fp@(ForeignPtr _ fpc) o l -> withForeignPtr fp $ \p -> case fpc of
            PlainPtr marr -> do
              let base = Ptr (mutableByteArrayContents# marr)
                  off = p `Foreign.minusPtr` base
              arr <- unsafeFreezeByteArray $ MutableByteArray marr
              pure (arr, off + o, off + o + l)
            _ -> error "should be PlainPtr"
   in case res of
        (ByteArray arr, I# off, I# len) -> (# arr, off, len #)
{-# INLINE unpackByteString# #-}
