{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Serialize.Internal where

import Data.Data (Data)
import Data.Int
import Data.Kind (Type)
import Data.Primitive (Prim, sizeOf#)
import Data.Word
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Generics qualified as G
import GHC.TypeLits
import Serialize.Internal.Get
import Serialize.Internal.Prim
import Serialize.Internal.Put
import Serialize.Internal.Util

#include "serialize.h"

type ConstSize# = (# Int# | (# #) #)

pattern ConstSize# :: Int# -> ConstSize#
pattern ConstSize# i# = (# i# | #)

pattern VarSize# :: ConstSize#
pattern VarSize# = (# | (# #) #)

(<>#) :: ConstSize# -> ConstSize# -> ConstSize#
ConstSize# i# <># ConstSize# j# = ConstSize# (i# +# j#)
_ <># _ = VarSize#
{-# INLINE (<>#) #-}

constSizeAdd# :: Int# -> ConstSize# -> ConstSize#
constSizeAdd# i# (ConstSize# j#) = ConstSize# (i# +# j#)
constSizeAdd# _ VarSize# = VarSize#
{-# INLINE constSizeAdd# #-}

{-# COMPLETE ConstSize#, VarSize# #-}

-- instance Semigroup ConstSize where
--   VarSize <> _ = VarSize
--   _ <> VarSize = VarSize
--   ConstSize i <> ConstSize j = ConstSize $ i + j
--   {-# INLINE (<>) #-}

-- instance Monoid ConstSize where
--   mempty = VarSize
--   {-# INLINE mempty #-}
--   mappend = (<>)
--   {-# INLINE mappend #-}

class Serialize a where
  size# :: a -> Int#
  default size# :: (Generic a, GSerializeSize (G.Rep a)) => a -> Int#
  size# x = gSize# (G.from x)
  {-# INLINE size# #-}
  constSize# :: Proxy# a -> ConstSize#
  constSize# _ = VarSize#
  put :: a -> Put
  get :: Get a

unsafeSize :: Serialize a => a -> Int
unsafeSize x = I# (size# x)
{-# INLINE unsafeSize #-}

-- checkOffset :: Int -> Int -> IO ()
-- checkOffset o l
--   | o < 0 = Exception.throwIO $ PutException o $ "encode offset was negative"
--   | o > l = Exception.throwIO $ PutException o $ "encode overshot end of " ++ show l ++ " byte long buffer"
--   | o < l = Exception.throwIO $ PutException o $ "encode undershot end of " ++ show l ++ " byte long buffer"
--   | otherwise = pure ()
-- {-# INLINE checkOffset #-}

-- | Exception thrown while running 'poke'. Note that other types of
-- exceptions could also be thrown. Invocations of 'fail' in the 'Put'
-- monad causes this exception to be thrown.
--
-- 'PutException's are not expected to occur in ordinary circumstances,
-- unsafeand usually indicate a programming error.
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

newtype BigEndian a = BigEndian {unBE :: a}
  deriving (Data, Functor, Foldable, Traversable)
  deriving (Show, Read, Eq, Ord, Generic, Prim, Num, Bounded) via a

newtype LittleEndian a = LittleEndian {unLE :: a}
  deriving (Data, Functor, Foldable, Traversable)
  deriving (Show, Read, Eq, Ord, Generic, Prim, Num, Bounded) via a

#ifdef WORDS_BIGENDIAN
type HostEndian = BigEndian
#else
type HostEndian = LittleEndian
#endif

data SList a = SList
  { sList :: [a],
    sSize :: !Int
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable, Data)

deriveSerializePrim(Word8)
deriveSerializePrim(Word16)
deriveSerializePrim(Word32)
deriveSerializePrim(Word64)
deriveSerializePrim(Word)

deriveSerializePrim(Int8)
deriveSerializePrim(Int16)
deriveSerializePrim(Int32)
deriveSerializePrim(Int64)
deriveSerializePrim(Int)

deriveSerializePrim(Float)
deriveSerializePrim(Double)
deriveSerializePrim(Char)
deriveSerializePrim(Ptr a)
-- instance Serialize a => Serialize [a] where
--   unsafeSize# xs = case unsafeConstSize @a of
--     ConstSize sz -> unI# $ (length xs) * sz
--     _ -> unI# $ sum (unsafeSize <$> xs)
--   {-# INLINE unsafeSize# #-}
--   unsafeConstSize = VarSize
--   {-# INLINE unsafeConstSize #-}
--   put xs = putFoldableWith (length xs) xs
--   {-# INLINE put #-}
--   get = do
--     size :: Int <- fromIntegral <$> get @Word64
--     xs <- foldM (\xs _ -> do x <- get; pure $ x : xs) [] [1 .. size]
--     pure $! reverse xs
--   {-# INLINE get #-}

-- putFoldableWith :: (Serialize a, Foldable f) => Int -> f a -> Put ()
-- putFoldableWith size xs = do
--   put (fromIntegral size :: Word64)
--   mapM_ put xs
-- {-# INLINE putFoldableWith #-}

-- deriveSerializePrimWith (Word8, put8, get8)

-- deriveSerializePrimWith (Word16, putLE, getLE)
-- deriveSerializePrimWith (LittleEndian Word16, put16LE . coerce, coerce get16LE)
-- deriveSerializePrimWith (BigEndian Word16, put16BE . coerce, coerce get16BE)

-- deriveSerializePrimWith (Word32, putLE, getLE)
-- deriveSerializePrimWith (LittleEndian Word32, put32LE . coerce, coerce get32LE)
-- deriveSerializePrimWith (BigEndian Word32, put32BE . coerce, coerce get32BE)

-- deriveSerializePrimWith (Word64, putLE, getLE)
-- deriveSerializePrimWith (LittleEndian Word64, put64LE . coerce, coerce get64LE)
-- deriveSerializePrimWith (BigEndian Word64, put64BE . coerce, coerce get64BE)

-- deriveSerializePrimWith (LittleEndian Int16, put . fmap (fromIntegral @_ @Word16), fmap (fromIntegral @Word16) <$!> get)
-- deriveSerializePrimWith (BigEndian Int16, put . fmap (fromIntegral @_ @Word16), fmap (fromIntegral @Word16) <$!> get)

-- deriveSerializePrimWith (LittleEndian Int32, put . fmap (fromIntegral @_ @Word32), fmap (fromIntegral @Word32) <$!> get)
-- deriveSerializePrimWith (BigEndian Int32, put . fmap (fromIntegral @_ @Word32), fmap (fromIntegral @Word32) <$!> get)

-- deriveSerializePrimWith (LittleEndian Int64, put . fmap (fromIntegral @_ @Word64), fmap (fromIntegral @Word64) <$!> get)
-- deriveSerializePrimWith (BigEndian Int64, put . fmap (fromIntegral @_ @Word64), fmap (fromIntegral @Word64) <$!> get)

-- deriveSerializePrimWith (LittleEndian Int, put . fmap (fromIntegral @_ @Word64), fmap (fromIntegral @Word64) <$!> get)
-- deriveSerializePrimWith (BigEndian Int, put . fmap (fromIntegral @_ @Word64), fmap (fromIntegral @Word64) <$!> get)

-- deriveSerializePrimWith (Int, putLE, getLE)

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
