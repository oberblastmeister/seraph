{-# LANGUAGE AllowAmbiguousTypes #-}

module Seraph.Internal.Util
  (
    unpackByteString,
    pinnedToByteString,
    sizeOf',
    ( .# ),
    ( #. ),
    Impossible(..),
    impossible,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.Primitive (Prim)
import Data.Primitive qualified as Primitive
import Foreign qualified
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (PlainPtr))
import GHC.IO qualified
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Coerce
import GHC.Exts (unsafeCoerce#)
import GHC.ST qualified
import Control.Monad.ST (ST)
import Control.Exception (Exception)
import qualified Control.Exception as Exception

sizeOf' :: forall a. Prim a => Int
sizeOf' = Primitive.sizeOf (undefined :: a)
{-# INLINE sizeOf' #-}

unpackByteString :: ByteString -> (Primitive.ByteArray, Int, Int)
unpackByteString bs@(B.Internal.PS (ForeignPtr (Primitive.Ptr -> p) fpc) o l) =
 unsafeDupablePerformIO $ case fpc of
  PlainPtr (Primitive.MutableByteArray -> marr) -> do
    let base = Primitive.mutableByteArrayContents marr
        off = p `Foreign.minusPtr` base
    arr <- Primitive.unsafeFreezeByteArray marr
    pure (arr, off + o, off + o + l)
  _ -> case B.copy bs of
    B.Internal.PS (ForeignPtr (Primitive.Ptr -> p) fpc) o l -> case fpc of
      PlainPtr (Primitive.MutableByteArray -> marr) -> do
        let base = Primitive.mutableByteArrayContents marr
            off = p `Foreign.minusPtr` base
        arr <- Primitive.unsafeFreezeByteArray marr
        pure (arr, off + o, off + o + l)
      _ -> error "should be PlainPtr"

pinnedToByteString :: Int -> Int -> Primitive.ByteArray -> ByteString
pinnedToByteString off len bs@(Primitive.ByteArray b#)
  | Primitive.isByteArrayPinned bs = B.Internal.PS fp off len
  | otherwise = error "ByteArray must be pinned"
  where
    !(Primitive.Ptr addr#) = Primitive.byteArrayContents bs
    fp = ForeignPtr addr# (PlainPtr (unsafeCoerce# b#))

( #. ) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
( #. ) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b

( .# ) :: Coercible b a => (b -> c) -> (a -> b) -> (a -> c)
( .# ) pbc _ = coerce pbc

{-# INLINE ( #. ) #-}
{-# INLINE ( .# ) #-}

infixr 9 #.
infixl 8 .#

data Impossible = Impossible
  deriving (Show, Eq)

instance Exception Impossible

impossible :: forall a. a
impossible = Exception.throw Impossible
