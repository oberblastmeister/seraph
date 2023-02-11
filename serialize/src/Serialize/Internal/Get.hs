module Serialize.Internal.Get where

import Data.Kind (Type)
import GHC.Exts (Int#, (+#))
import Serialize.Internal.Util

newtype Get :: Type -> Type where
  Get# ::
    { runGet# ::
        BS# ->
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
