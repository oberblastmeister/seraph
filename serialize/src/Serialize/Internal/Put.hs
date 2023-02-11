module Serialize.Internal.Put where

import Data.Kind (Type)
import Data.Primitive (MutableByteArray#)
import GHC.Exts (Int#, State#, (+#))

newtype Put :: Type where
  Put# ::
    { runPut# ::
        forall s.
        MutableByteArray# s ->
        PS# ->
        State# s ->
        (# State# s, PS# #)
    } ->
    Put

newtype PS# = PutState# (# Int# #)

pattern PS# :: Int# -> PS#
pattern PS# x = PutState# (# x #)

{-# COMPLETE PS# #-}

incPS# :: Int# -> PS# -> PS#
incPS# i# (PS# x#) = PS# (i# +# x#)
{-# INLINE incPS# #-}

instance Semigroup Put where
  Put# p1 <> Put# p2 = Put# \marr# ps# s# -> case p1 marr# ps# s# of
    (# s#, ps# #) -> p2 marr# ps# s#
  {-# INLINE (<>) #-}

instance Monoid Put where
  mempty = Put# \_marr# ps# s# -> (# s#, ps# #)
  {-# INLINE mempty #-}
