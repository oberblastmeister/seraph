{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Seraph.Internal.Put where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad.ST (ST, stToIO)
import Control.Monad.ST.Unsafe qualified as ST.Unsafe
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Primitive (Prim)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..))
import Data.Primitive.ByteArray.Unaligned qualified as Unaligned
import GHC.Exts (RealWorld)
import GHC.Exts qualified as Exts
import Seraph.Internal.Util

-- | This represents serialization actions.
-- This is essentally a bytestring builder.
-- Unlike 'Get', it only implements 'Monoid', but not 'Monad'.
-- Chain 'Put' actions using '<>' instead of '>>='.
newtype Put :: Type where
  Put## :: {runPut# :: Primitive.MutableByteArray RealWorld -> Int -> IO Int} -> Put

pattern Put# :: (Primitive.MutableByteArray RealWorld -> Int -> IO Int) -> Put
pattern Put# f <- Put## f
  where
    Put# f = Put## $ Exts.oneShot \marr -> Exts.oneShot \i -> f marr i

{-# COMPLETE Put# #-}

instance Semigroup Put where
  Put# p1 <> Put# p2 = Put# \marr i -> p1 marr i >>= p2 marr
  {-# INLINE (<>) #-}

instance Monoid Put where
  mempty = Put# \_marr i -> pure i
  {-# INLINE mempty #-}

data PutException
  = IndexOutOfBounds !Int !Int
  deriving (Show, Eq, Ord)

instance Exception PutException

throwST :: Exception e => e -> ST s a
throwST = ST.Unsafe.unsafeIOToST . Exception.throwIO
{-# NOINLINE throwST #-}

unsafeWithPut :: (forall s. Primitive.MutableByteArray s -> Int -> ST s Int) -> Put
unsafeWithPut f = Put# \marr i -> stToIO (f marr i)
{-# INLINE unsafeWithPut #-}

unsafeWithSizedPutIO :: Int -> (Primitive.MutableByteArray RealWorld -> Int -> IO ()) -> Put
unsafeWithSizedPutIO len f = Put# \marr i -> do
  let i' = len + i
  let l = Primitive.sizeofMutableByteArray marr
  if i' <= l
    then f marr i $> i'
    else Exception.throwIO $ IndexOutOfBounds i' l
{-# INLINE unsafeWithSizedPutIO #-}

unsafeWithSizedPut :: Int -> (forall s. Primitive.MutableByteArray s -> Int -> ST s ()) -> Put
unsafeWithSizedPut len f = unsafeWithSizedPutIO len \marr i -> stToIO (f marr i)
{-# INLINE unsafeWithSizedPut #-}

putPrim :: forall a. (Prim a, PrimUnaligned a) => a -> Put
putPrim x = unsafeWithSizedPut (sizeOf' @a) \marr i -> Unaligned.writeUnalignedByteArray marr i x
{-# INLINE putPrim #-}
