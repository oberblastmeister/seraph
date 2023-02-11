{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo -O2 #-}

{-# HLINT ignore "Redundant bracket" #-}

module Serialize.Internal where

import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Int
import Data.Primitive (MutablePrimArray, Prim, sizeOf#)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray.Unaligned
import Data.Word
import GHC.Generics (Generic)
import GHC.Generics qualified as G
import GHC.TypeLits
import Serialize.Internal.Exts
import Serialize.Internal.Get
import Serialize.Internal.Put
import Serialize.Internal.Util
import System.ByteOrder qualified as ByteOrder

#include "serialize.h"

type ConstSize# = (# Int# | (# #) #)

pattern ConstSize# :: Int# -> ConstSize#
pattern ConstSize# i# = (# i# | #)

pattern VarSize# :: ConstSize#
pattern VarSize# = (# | (# #) #)

{-# COMPLETE ConstSize#, VarSize# #-}

(<>#) :: ConstSize# -> ConstSize# -> ConstSize#
ConstSize# i# <># ConstSize# j# = ConstSize# (i# +# j#)
_ <># _ = VarSize#
{-# INLINE (<>#) #-}

constSizeAdd# :: Int# -> ConstSize# -> ConstSize#
constSizeAdd# i# (ConstSize# j#) = ConstSize# (i# +# j#)
constSizeAdd# _ VarSize# = VarSize#
{-# INLINE constSizeAdd# #-}

class Serialize a where
  size# :: a -> Int#
  default size# :: (Generic a, GSerializeSize (G.Rep a)) => a -> Int#
  size# x = gSize# (G.from x)
  {-# INLINE size# #-}

  constSize# :: Proxy# a -> ConstSize#
  constSize# _ = VarSize#
  {-# INLINE constSize# #-}

  put :: a -> Put

  get :: Get a

constSize## :: forall a. Serialize a => ConstSize#
constSize## = constSize# (proxy# @a)
{-# INLINE constSize## #-}

unsafeSize :: Serialize a => a -> Int
unsafeSize x = I# (size# x)
{-# INLINE unsafeSize #-}

class GSerializeSize f where
  gSize# :: f a -> Int#

class GSerializeConstSize f where
  gConstSize# :: Proxy# (f a) -> ConstSize#

class GSerializePut f where
  gPut :: f a -> Put

class GSerializeGet f where
  gGet :: Get (f a)

instance GSerializeSize f => GSerializeSize (G.M1 i c f) where
  gSize# (G.M1 x) = gSize# x
  {-# INLINE gSize# #-}

instance GSerializeConstSize f => GSerializeConstSize (G.M1 i c f) where
  gConstSize# _ = gConstSize# (proxy# @(f _))
  {-# INLINE gConstSize# #-}

instance GSerializePut f => GSerializePut (G.M1 i c f) where
  gPut (G.M1 x) = gPut x
  {-# INLINE gPut #-}

instance GSerializeGet f => GSerializeGet (G.M1 i c f) where
  gGet = fmap G.M1 gGet
  {-# INLINE gGet #-}

instance Serialize f => GSerializeSize (G.K1 i f) where
  gSize# (G.K1 x) = size# x
  {-# INLINE gSize# #-}

instance Serialize f => GSerializeConstSize (G.K1 i f) where
  gConstSize# _ = constSize# (proxy# @f)
  {-# INLINE gConstSize# #-}

instance Serialize f => GSerializePut (G.K1 i f) where
  gPut (G.K1 x) = put x
  {-# INLINE gPut #-}

instance Serialize f => GSerializeGet (G.K1 i f) where
  gGet = fmap G.K1 get
  {-# INLINE gGet #-}

instance GSerializeSize G.U1 where
  gSize# G.U1 = 0#
  {-# INLINE gSize# #-}

instance GSerializeConstSize G.U1 where
  gConstSize# _ = ConstSize# 0#
  {-# INLINE gConstSize# #-}

instance GSerializePut G.U1 where
  gPut _ = mempty
  {-# INLINE gPut #-}

instance GSerializeGet G.U1 where
  gGet = pure G.U1
  {-# INLINE gGet #-}

instance GSerializeSize G.V1 where
  gSize# = \case {}
  {-# INLINE gSize# #-}

instance GSerializePut G.V1 where
  gPut = \case {}
  {-# INLINE gPut #-}

instance (GSerializeSize f, GSerializeSize g) => GSerializeSize (f G.:*: g) where
  gSize# (x G.:*: y) = gSize# x +# gSize# y
  {-# INLINE gSize# #-}

instance (GSerializeConstSize f, GSerializeConstSize g) => GSerializeConstSize (f G.:*: g) where
  gConstSize# _ = gConstSize# (proxy# @(f _)) <># gConstSize# (proxy# @(g _))
  {-# INLINE gConstSize# #-}

instance (GSerializePut f, GSerializePut g) => GSerializePut (f G.:*: g) where
  gPut (x G.:*: y) = gPut x <> gPut y
  {-# INLINE gPut #-}

instance (GSerializeGet f, GSerializeGet g) => GSerializeGet (f G.:*: g) where
  gGet = (G.:*:) <$> gGet <*> gGet
  {-# INLINE gGet #-}

instance (FitsInByte (SumArity (f G.:+: g)), GSerializeSizeSum 0 (f G.:+: g)) => GSerializeSize (f G.:+: g) where
  gSize# x = sizeOf## @Word8 +# gSizeSum# x (proxy# @0)
  {-# INLINE gSize# #-}

instance GSerializeConstSize (f G.:+: g) where
  gConstSize# _ = VarSize#
  {-# INLINE gConstSize# #-}

class KnownNat n => GSerializeSizeSum n f where
  gSizeSum# :: f a -> Proxy# n -> Int#

class KnownNat n => GSerializePutSum n f where
  gputSum# :: f a -> Proxy# n -> Put

class KnownNat n => GSerializeGetSum n f where
  ggetSum# :: Word8 -> Proxy# n -> Get (f a)

instance (GSerializeSizeSum n f, GSerializeSizeSum (n + SumArity f) g, KnownNat n) => GSerializeSizeSum n (f G.:+: g) where
  gSizeSum# (G.L1 l) _ = gSizeSum# l (proxy# @n)
  gSizeSum# (G.R1 r) _ = gSizeSum# r (proxy# @(n + SumArity f))
  {-# INLINE gSizeSum# #-}

instance (GSerializePutSum n f, GSerializePutSum (n + SumArity f) g, KnownNat n) => GSerializePutSum n (f G.:+: g) where
  gputSum# (G.L1 l) _ = gputSum# l (proxy# @n)
  gputSum# (G.R1 r) _ = gputSum# r (proxy# @(n + SumArity f))

instance (GSerializeSize f, KnownNat n) => GSerializeSizeSum n (G.C1 c f) where
  gSizeSum# x _ = gSize# x
  {-# INLINE gSizeSum# #-}

instance (GSerializePut f, KnownNat n) => GSerializePutSum n (G.C1 c f) where
  gputSum# x _ = put tag <> gPut x
    where
      tag = fromInteger @Word8 (natVal' (proxy# @n))

type SumArity :: (Type -> Type) -> Nat
type family SumArity a where
  SumArity (G.C1 c a) = 1
  SumArity (x G.:+: y) = SumArity x + SumArity y

type FitsInByte n = FitsInByteResult (n <=? 255)

type FitsInByteResult :: Bool -> Constraint
type family FitsInByteResult b where
  FitsInByteResult True = ()
  FitsInByteResult False = TypeError (Text "Generic deriving of Serialize instances can only be used on datatypes with fewer than 256 constructors.")

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Int) where
  size# _ = sizeOf## @Int
  constSize# _ = ConstSize# (sizeOf## @Int)
  put = put . fromIntegral @(ByteOrder.Fixed b Int) @(ByteOrder.Fixed b Int64)
  get = fromIntegral @(ByteOrder.Fixed b Int64) @(ByteOrder.Fixed b Int) <$> get
  {-# INLINE size# #-}
  {-# INLINE constSize# #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Word) where
  size# _ = sizeOf## @Word
  constSize# _ = ConstSize# (sizeOf## @Word)
  put = put . fromIntegral @(ByteOrder.Fixed b Word) @(ByteOrder.Fixed b Word64)
  get = fromIntegral @(ByteOrder.Fixed b Word64) @(ByteOrder.Fixed b Word) <$> get
  {-# INLINE size# #-}
  {-# INLINE constSize# #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

deriveSerializePrim (Word8)
deriveSerializePrim (Word16)
deriveSerializePrim (Word32)
deriveSerializePrim (Word64)
deriveSerializePrimLE (Word)

deriveSerializePrim (Int8)
deriveSerializePrim (Int16)
deriveSerializePrim (Int32)
deriveSerializePrim (Int64)
deriveSerializePrimLE (Int)

instance Serialize a => Serialize [a] where
  size# xs = case constSize## @a of
    ConstSize# sz# -> unI# (length xs) *# sz#
    _ -> unI# $ sum $ unsafeSize <$> xs

  constSize# _ = VarSize#
  {-# INLINE constSize# #-}

  put xs = putFoldableWith (length xs) xs

  get = do
    size :: Int <- fromIntegral <$> get @Word64
    xs <- foldM (\xs _ -> do x <- get; pure $ x : xs) [] [1 .. size]
    pure $ reverse xs

putFoldableWith :: (Serialize a, Foldable f) => Int -> f a -> Put
putFoldableWith !size xs = put (fromIntegral size :: Word64) <> foldMap put xs
{-# INLINE putFoldableWith #-}

bruh :: [Word64] -> Put
bruh = put

bruhGet :: Get [Word64]
bruhGet = get

unsafeReinterpretCast :: forall a b. (Prim a, Prim b) => a -> b
unsafeReinterpretCast x = runST $ do
  marr <- Primitive.newPrimArray @_ @a 1
  Primitive.writePrimArray marr 0 x
  let marr' = coerce @_ @(MutablePrimArray _ b) marr
  Primitive.readPrimArray marr' 0
{-# INLINE unsafeReinterpretCast #-}

putPrim :: forall a. (Prim a, PrimUnaligned a) => a -> Put
putPrim x = Put# \e# ps@(PS# i#) s# -> case writeEnv# e# i# x s# of
  s# -> (# s#, incPS# (sizeOf## @a) ps #)
{-# INLINE putPrim #-}

getPrim :: forall a. (Prim a, PrimUnaligned a) => Get a
getPrim = Get# \bs# gs@(GS# i#) -> case indexBS# i# bs# of
  x -> GR# (incGS# (sizeOf## @a) gs) x
{-# INLINE getPrim #-}

-- unroll :: (Integral a, Bits a) => a -> [Word8]
-- unroll = unfoldr step
--   where
--     step 0 = Nothing
--     step i = Just (fromIntegral i, i `unsafeShiftR` 8)
-- {-# INLINE unroll #-}

-- roll :: (Integral a, Bits a) => [Word8] -> a
-- roll = foldr unstep 0
--   where
--     unstep a b = fromIntegral a .|. b `unsafeShiftL` 8
-- {-# INLINE roll #-}

-- unI# :: Int -> Int#
-- unI# (I# i#) = i#
-- {-# INLINE unI# #-}

-- data SList a = SList
--   { sList :: [a],
--     sSize :: !Int
--   }
--   deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable, Data)
