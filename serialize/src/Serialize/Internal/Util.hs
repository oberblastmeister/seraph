{-# LANGUAGE AllowAmbiguousTypes #-}

module Serialize.Internal.Util
  ( S#,
    sizeOf##,
    unI#,
  )
where

import Data.Primitive (Prim, sizeOf#)
import GHC.Exts (Int (..), Int#, RealWorld, State#)

type S# = State# RealWorld

sizeOf## :: forall a. Prim a => Int#
sizeOf## = sizeOf# (undefined :: a)
{-# INLINE sizeOf## #-}

unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}
