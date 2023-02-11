{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo -O2 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Serialize.Internal where

import Control.Monad (foldM)
import Data.Int
import Data.Kind (Type)
import Data.Primitive (Prim, sizeOf#)
import Data.Primitive.ByteArray.Unaligned
import Data.Word
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Generics qualified as G
import GHC.TypeLits
import Serialize.Internal.Get
import Serialize.Internal.Prim
import Serialize.Internal.Put
import Serialize.Internal.Util
import System.ByteOrder

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
  gput :: f a -> Put

class GSerializeGet f where
  gget :: Get (f a)

instance GSerializeSize f => GSerializeSize (G.M1 i c f) where
  gSize# (G.M1 x) = gSize# x
  {-# INLINE gSize# #-}

instance GSerializeConstSize f => GSerializeConstSize (G.M1 i c f) where
  gConstSize# _ = gConstSize# (proxy# @(f _))
  {-# INLINE gConstSize# #-}

instance GSerializePut f => GSerializePut (G.M1 i c f) where
  gput (G.M1 x) = gput x
  {-# INLINE gput #-}

instance GSerializeGet f => GSerializeGet (G.M1 i c f) where
  gget = fmap G.M1 gget
  {-# INLINE gget #-}

instance Serialize f => GSerializeSize (G.K1 i f) where
  gSize# (G.K1 x) = size# x
  {-# INLINE gSize# #-}

instance Serialize f => GSerializeConstSize (G.K1 i f) where
  gConstSize# _ = constSize# (proxy# @f)
  {-# INLINE gConstSize# #-}

instance Serialize f => GSerializePut (G.K1 i f) where
  gput (G.K1 x) = put x
  {-# INLINE gput #-}

instance Serialize f => GSerializeGet (G.K1 i f) where
  gget = fmap G.K1 get
  {-# INLINE gget #-}

instance GSerializeSize G.U1 where
  gSize# G.U1 = 0#
  {-# INLINE gSize# #-}

instance GSerializeConstSize G.U1 where
  gConstSize# _ = ConstSize# 0#
  {-# INLINE gConstSize# #-}

instance GSerializePut G.U1 where
  gput _ = mempty
  {-# INLINE gput #-}

instance GSerializeGet G.U1 where
  gget = pure G.U1
  {-# INLINE gget #-}

instance GSerializeSize G.V1 where
  gSize# = \case {}
  {-# INLINE gSize# #-}

instance GSerializePut G.V1 where
  gput = \case {}
  {-# INLINE gput #-}

instance (GSerializeSize f, GSerializeSize g) => GSerializeSize (f G.:*: g) where
  gSize# (x G.:*: y) = gSize# x +# gSize# y
  {-# INLINE gSize# #-}

instance (GSerializeConstSize f, GSerializeConstSize g) => GSerializeConstSize (f G.:*: g) where
  gConstSize# _ = gConstSize# (proxy# @(f _)) <># gConstSize# (proxy# @(g _))
  {-# INLINE gConstSize# #-}

instance (GSerializePut f, GSerializePut g) => GSerializePut (f G.:*: g) where
  gput (x G.:*: y) = gput x <> gput y
  {-# INLINE gput #-}

instance (GSerializeGet f, GSerializeGet g) => GSerializeGet (f G.:*: g) where
  gget = (G.:*:) <$> gget <*> gget
  {-# INLINE gget #-}

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
  gputSum# x _ = put tag <> gput x
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

instance FixedOrdering b => Serialize (Fixed b Int) where
  size# _ = sizeOf## @Int
  constSize# _ = ConstSize# (sizeOf## @Int)
  put = put . fromIntegral @(Fixed b Int) @(Fixed b Int64)
  get = fromIntegral @(Fixed b Int64) @(Fixed b Int) <$> get
  {-# INLINE size# #-}
  {-# INLINE constSize# #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance FixedOrdering b => Serialize (Fixed b Word) where
  size# _ = sizeOf## @Word
  constSize# _ = ConstSize# (sizeOf## @Word)
  put = put . fromIntegral @(Fixed b Word) @(Fixed b Word64)
  get = fromIntegral @(Fixed b Word64) @(Fixed b Word) <$> get
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
    pure $! reverse xs

putFoldableWith :: (Serialize a, Foldable f) => Int -> f a -> Put
putFoldableWith !size xs = put (fromIntegral size :: Word64) <> foldMap put xs
{-# INLINE putFoldableWith #-}

bruh :: [Word64] -> Put
bruh = put

-- putLE :: Serialize (LittleEndian a) => a -> Put ()
-- putLE = put . LittleEndian
-- {-# INLINE putLE #-}

-- putBE :: Serialize (BigEndian a) => a -> Put ()
-- putBE = put . BigEndian
-- {-# INLINE putBE #-}

-- getLE :: forall a. Serialize (LittleEndian a) => Get a
-- getLE = coerce @(Get (LittleEndian a)) get
-- {-# INLINE getLE #-}

-- getBE :: forall a. Serialize (BigEndian a) => Get a
-- getBE = coerce @(Get (BigEndian a)) get
-- {-# INLINE getBE #-}

-- unsafeReinterpretCast :: forall a b. (Prim a, Prim b) => a -> b
-- unsafeReinterpretCast x = runST $ do
--   marr <- newPrimArray @_ @a 1
--   writePrimArray marr 0 x
--   let marr' = coerce @_ @(MutablePrimArray _ b) marr
--   readPrimArray marr' 0
-- {-# INLINE unsafeReinterpretCast #-}

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
