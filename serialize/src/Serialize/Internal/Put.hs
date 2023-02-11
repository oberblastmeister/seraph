module Serialize.Internal.Put where

import Data.Kind (Type)
import Data.Primitive (MutableByteArray#, Prim)
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned (..))
import GHC.Exts (Int#, State#, (+#), (>=#))
import Serialize.Internal.Util (sizeOf##)

newtype Put :: Type where
  Put# ::
    { runPut# ::
        forall s.
        Env# s ->
        PS# ->
        State# s ->
        (# State# s, PS# #)
    } ->
    Put

newtype Env# s = Env## (# MutableByteArray# s, Int# #)

newtype PS# = PutState# (# Int# #)

pattern Env# :: MutableByteArray# s -> Int# -> Env# s
pattern Env# marr# l# = Env## (# marr#, l# #)

pattern PS# :: Int# -> PS#
pattern PS# x = PutState# (# x #)

{-# COMPLETE PS# #-}

{-# COMPLETE Env# #-}

incPS# :: Int# -> PS# -> PS#
incPS# i# (PS# x#) = PS# (i# +# x#)
{-# INLINE incPS# #-}

writeEnv# :: forall a s. (Prim a, PrimUnaligned a) => Env# s -> Int# -> a -> State# s -> State# s
writeEnv# (Env# marr# l#) i# x s# = case i# +# sizeOf## @a >=# l# of
  1# -> writeUnalignedByteArray# marr# i# x s#
  _ -> error "Index out of bounds"

instance Semigroup Put where
  Put# p1 <> Put# p2 = Put# \marr# ps# s# -> case p1 marr# ps# s# of
    (# s#, ps# #) -> p2 marr# ps# s#
  {-# INLINE (<>) #-}

instance Monoid Put where
  mempty = Put# \_marr# ps# s# -> (# s#, ps# #)
  {-# INLINE mempty #-}
