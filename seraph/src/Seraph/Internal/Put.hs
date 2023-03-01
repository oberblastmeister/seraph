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

-- Note: Because everything is unboxed there are a lot more arguments to the function
-- This makes recursive calls more expensive

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

#if MIN_VERSION_base(4,16,1)
-- inline pragma on pattern synonyms only available from GHC 9.2.1
-- https://gitlab.haskell.org/ghc/ghc/-/issues/12178
{-# INLINE Put# #-}
#else
#endif

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

-- throwPut :: Exception e => e -> Put
-- throwPut :: Except
-- throwPut e = Put# \_ ps# s# -> case runIO# (Exception.throwIO e) s# of
--   (# s#, _ #) -> PR# s# ps#
-- {-# NOINLINE throwPut #-}

-- Even more unsafe!
-- Used because ByteString stuff uses IO instead of ST
-- Just don't shoot the missiles in here!
-- unsafeWithPutIO :: Int -> (Primitive.MutableByteArray RealWorld -> Int -> IO Int) -> Put
-- unsafeWithPutIO = Put#
-- unsafeWithPutIO (I# o#) f = Put# \(PE# marr#) ps@(PS# i#) s# ->
--   case runIO# (f (Primitive.MutableByteArray marr#) (I# i#)) s# of
--     (# s#, () #) -> PR# s# (incPS# o# ps)
-- unsafeWithPutIO (I# o#) f = Put# \(PE# marr#) ps@(PS# i#) s# ->
--   case getSizeofMutableByteArray# marr# s# of
--     (# s#, l# #) -> case i# +# o# ># l# of
--       1# -> case runIO# (Exception.throwIO $ IndexOutOfBounds (I# (i# +# o#)) (I# l#)) s# of
--         (# s#, _ #) -> PR# s# ps
--       _ ->
--         case runIO# (f (Primitive.MutableByteArray marr#) (I# i#)) s# of
--         (# s#, () #) -> PR# s# (incPS# o# ps)
-- {-# INLINE unsafeWithPutIO #-}

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
