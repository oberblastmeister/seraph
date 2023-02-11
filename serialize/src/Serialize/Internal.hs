{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo -O2 #-}

{-# HLINT ignore "Redundant bracket" #-}

module Serialize.Internal where

import Control.Monad (foldM, (<$!>))
import Control.Monad.ST (runST)
import Data.Int
import Data.Monoid (Dual (..), First (..), Last (..), Product (..), Sum (..))
import Data.Primitive (MutablePrimArray, Prim, sizeOf#)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray.Unaligned
import Data.Tree (Tree)
import Data.Word
import GHC.Generics (Generic)
import GHC.Generics qualified as G
import GHC.TypeLits
import Serialize.Internal.Exts
import Serialize.Internal.Get
import Serialize.Internal.Put
import Serialize.Internal.Util
import System.ByteOrder qualified as ByteOrder
import Data.ByteString (ByteString)
import GHC.ST (ST(..))
import qualified Data.ByteString.Short as SBS

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
  default constSize# :: (GSerializeConstSize (G.Rep a)) => Proxy# a -> ConstSize#
  constSize# _ = gConstSize# (proxy# @((G.Rep a) _))
  {-# INLINE constSize# #-}

  put :: a -> Put
  default put :: (Generic a, GSerializePut (G.Rep a)) => a -> Put
  put = gPut . G.from

  get :: Get a
  default get :: (Generic a, GSerializeGet (G.Rep a)) => Get a
  get = G.to <$!> gGet

constSize## :: forall a. Serialize a => ConstSize#
constSize## = constSize# (proxy# @a)
{-# INLINE constSize## #-}

size :: Serialize a => a -> Int
size x = I# (size# x)
{-# INLINE size #-}

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
  gGet = (G.:*:) <$!> gGet <*> gGet
  {-# INLINE gGet #-}

instance (FitsInByte (SumArity (f G.:+: g)), GSerializeSizeSum 0 (f G.:+: g)) => GSerializeSize (f G.:+: g) where
  gSize# x = sizeOf## @Word8 +# gSizeSum# x (proxy# @0)
  {-# INLINE gSize# #-}

instance (FitsInByte (SumArity (f G.:+: g)), GSerializePutSum 0 (f G.:+: g)) => GSerializePut (f G.:+: g) where
  gPut x = gPutSum x (proxy# @0)
  {-# INLINE gPut #-}

instance (FitsInByte (SumArity (f G.:+: g)), GSerializeGetSum 0 (f G.:+: g)) => GSerializeGet (f G.:+: g) where
  gGet = do
    tag <- get @Word8
    gGetSum# (unW# (fromIntegral @Word8 @Word tag)) (proxy# @0)
  {-# INLINE gGet #-}

instance GSerializeConstSize (f G.:+: g) where
  gConstSize# _ = VarSize#
  {-# INLINE gConstSize# #-}

class KnownNat n => GSerializeSizeSum n f where
  gSizeSum# :: f a -> Proxy# n -> Int#

class KnownNat n => GSerializePutSum n f where
  gPutSum :: f a -> Proxy# n -> Put

class KnownNat n => GSerializeGetSum n f where
  gGetSum# :: Word# -> Proxy# n -> Get (f a)

instance (GSerializeSizeSum n f, GSerializeSizeSum (n + SumArity f) g) => GSerializeSizeSum n (f G.:+: g) where
  gSizeSum# (G.L1 l) _ = gSizeSum# l (proxy# @n)
  gSizeSum# (G.R1 r) _ = gSizeSum# r (proxy# @(n + SumArity f))
  {-# INLINE gSizeSum# #-}

instance (GSerializePutSum n f, GSerializePutSum (n + SumArity f) g) => GSerializePutSum n (f G.:+: g) where
  gPutSum (G.L1 l) _ = gPutSum l (proxy# @n)
  gPutSum (G.R1 r) _ = gPutSum r (proxy# @(n + SumArity f))
  {-# INLINE gPutSum #-}

instance (GSerializeGetSum n f, GSerializeGetSum (n + SumArity f) g) => GSerializeGetSum n (f G.:+: g) where
  gGetSum# tag# p#
    | W# tag# < sizeL = G.L1 <$!> gGetSum# tag# p#
    | otherwise = G.R1 <$!> gGetSum# tag# (proxy# @(n + SumArity f))
    where
      sizeL = fromInteger (natVal' (proxy# @(n + SumArity f)))
  {-# INLINE gGetSum# #-}

instance (GSerializeSize f, KnownNat n) => GSerializeSizeSum n (G.C1 c f) where
  gSizeSum# x _ = gSize# x
  {-# INLINE gSizeSum# #-}

instance (GSerializePut f, KnownNat n) => GSerializePutSum n (G.C1 c f) where
  gPutSum x _ = put tag <> gPut x
    where
      tag = fromInteger @Word8 (natVal' (proxy# @n))

instance (GSerializeGet f, KnownNat n) => GSerializeGetSum n (G.C1 c f) where
  gGetSum# tag _
    | W# tag == cur = gGet
    | W# tag > cur = error "Sum tag invalid"
    | otherwise = error "Implementation error"
    where
      cur = fromInteger @Word (natVal' (proxy# @n))

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
  get = fromIntegral @(ByteOrder.Fixed b Int64) @(ByteOrder.Fixed b Int) <$!> get
  {-# INLINE size# #-}
  {-# INLINE constSize# #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Word) where
  size# _ = sizeOf## @Word
  constSize# _ = ConstSize# (sizeOf## @Word)
  put = put . fromIntegral @(ByteOrder.Fixed b Word) @(ByteOrder.Fixed b Word64)
  get = fromIntegral @(ByteOrder.Fixed b Word64) @(ByteOrder.Fixed b Word) <$!> get
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

deriveSerializeNewtype (Dual)
deriveSerializeNewtype (Sum)
deriveSerializeNewtype (Product)

instance Serialize Ordering

instance Serialize a => Serialize (Tree a)

instance (Serialize a, Serialize b) => Serialize (a, b)

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b, c)

instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a, b, c, d)

instance Serialize a => Serialize (Maybe a)

instance (Serialize a, Serialize b) => Serialize (Either a b)

instance Serialize a => Serialize [a] where
  size# xs = case constSize## @a of
    ConstSize# sz# -> unI# (length xs) *# sz#
    _ -> unI# $ sum $ size <$!> xs

  put xs = putFoldableWith (length xs) xs

  get = do
    size :: Int <- fromIntegral <$!> get @Word64
    xs <- foldM (\xs _ -> do x <- get; pure $ x : xs) [] [1 .. size]
    pure $ reverse xs

putFoldableWith :: (Serialize a, Foldable f) => Int -> f a -> Put
putFoldableWith !size xs = put (fromIntegral size :: Word64) <> foldMap put xs
{-# INLINE putFoldableWith #-}

bruh :: Either Word64 Word16 -> Put
bruh = put

bruhGet :: Get (Either Word64 Word16)
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
  !x -> GR# (incGS# (sizeOf## @a) gs) x
{-# INLINE getPrim #-}

encode :: Serialize a => a -> ByteString
encode x = runST $ do
  marr@(Primitive.MutableByteArray marr#) <- Primitive.newPinnedByteArray sz
  ST \s# -> case runPut# (put x) (Env# marr# (unI# sz)) (PS# 0#) s# of
    (# s#, _ #) -> (# s#, () #)
  Primitive.ByteArray arr# <- Primitive.unsafeFreezeByteArray marr
  pure (SBS.fromShort (SBS.SBS arr#))
  where
    sz = size x

decode :: Serialize a => ByteString -> a
decode bs = case runGet# get (BS# arr# l#) (GS# i#) of
  GR# _ x -> x
  where
    !(# arr#, i#, l# #) = unpackByteString# bs
