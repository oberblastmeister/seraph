module Serialize.Internal.Get where

import Control.Exception qualified as Exception
import Data.Primitive
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..))
import Serialize.Internal.Exts
import Serialize.Internal.Util

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
  _ -> Exception.throw $ IndexOutOfBounds (I# (i# +# sizeOf## @a)) (I# l#)
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

data GetException
  = IndexOutOfBounds !Int !Int
  | InvalidSumTag !Word !Word
  deriving (Eq)

instance Show GetException where
  show (IndexOutOfBounds i l) = "Index out of bounds: " ++ show i ++ " >= " ++ show l
  show (InvalidSumTag cur tag) = "Invalid sum tag: " ++ show cur ++ " != " ++ show tag

instance Exception.Exception GetException
