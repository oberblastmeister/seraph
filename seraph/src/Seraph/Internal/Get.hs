module Seraph.Internal.Get where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad.ST (ST)
import Data.Primitive
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..))
import Data.Primitive.ByteArray.Unaligned qualified as Unaligned
import Seraph.Internal.Exts
import Seraph.Internal.Util
import qualified Control.Monad.ST.Unsafe as ST.Unsafe

-- | This represents deserialization actions.
-- Unlike 'Put', this type implements 'Monad'.
newtype Get :: Type -> Type where
  Get# :: {runGet# :: GE -> Int -> IO (GR a)} -> Get a

data GR a = GR !Int !a

data GE = GE !Primitive.ByteArray !Int

instance Functor Get where
  fmap f (Get# g) = Get# \ge i -> do
    GR i x <- g ge i
    pure $! GR i (f x)
  {-# INLINE fmap #-}

instance Applicative Get where
  pure x = Get# \_arr i -> pure $! GR i x
  {-# INLINE pure #-}

  Get# g1 <*> Get# g2 = Get# \arr i -> do
    GR i f <- g1 arr i
    GR i x <- g2 arr i
    pure $! GR i (f x)
  {-# INLINE (<*>) #-}

instance Monad Get where
  Get# g >>= fm = Get# \arr i -> do
    GR i x <- g arr i
    runGet# (fm x) arr i
  {-# INLINE (>>=) #-}

data GetException
  = IndexOutOfBounds !Int !Int
  | InvalidSumTag !Int
  deriving (Eq, Ord)

instance Show GetException where
  show (IndexOutOfBounds i l) = "Index out of bounds: " ++ show i ++ " >= " ++ show l
  show (InvalidSumTag tag) = "Invalid sum tag: " ++ show tag

instance Exception GetException

unsafeLiftIO :: IO a -> Get a
unsafeLiftIO m = Get# \_ i -> GR i <$> m
{-# INLINE unsafeLiftIO #-}

unsafeLiftST :: ST s a -> Get a
unsafeLiftST m = unsafeLiftIO (ST.Unsafe.unsafeSTToIO m)
{-# INLINE unsafeLiftST #-}

unsafeWithSizeGet :: Int -> (Primitive.ByteArray -> Int -> a) -> Get a
unsafeWithSizeGet o f = Get# \(GE arr l) i -> do
  let i' = i + o
  if i' <= l
    then pure $! GR i' $! f arr i
    else Exception.throwIO $ IndexOutOfBounds i' l
{-# INLINE unsafeWithSizeGet #-}

throwGet :: Exception e => e -> Get a
throwGet e = Get# \_ _ -> Exception.throwIO e
{-# NOINLINE throwGet #-}

getPrim :: forall a. (Prim a, PrimUnaligned a) => Get a
getPrim = unsafeWithSizeGet (sizeOf' @a) Unaligned.indexUnalignedByteArray
{-# INLINE getPrim #-}
