{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Serialize.Internal where

import Control.Exception qualified as Exception
import Control.Monad (foldM, (<$!>))
import Control.Monad.ST (runST)
import Data.Bits (Bits, unsafeShiftL, unsafeShiftR, (.|.))
import Data.Data (Data)
import Data.Int
import Data.Kind (Type)
import Data.List (unfoldr)
import Data.Primitive (Prim, sizeOf, sizeOf#)
import Data.Primitive.PrimArray
import Data.Word
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Generics qualified as G
import GHC.TypeLits
import Serialize.Internal.Prim
import Serialize.Internal.Types

#include "serialize.h"

data ConstSize = ConstSize !Int | VarSize

instance Semigroup ConstSize where
  VarSize <> _ = VarSize
  _ <> VarSize = VarSize
  ConstSize i <> ConstSize j = ConstSize $ i + j
  {-# INLINE (<>) #-}

instance Monoid ConstSize where
  mempty = VarSize
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

class Serialize a where
  unsafeSize# :: a -> Int#
  unsafeConstSize :: ConstSize
  poke :: a -> Poke ()
  peek :: Peek a

unsafeSize :: Serialize a => a -> Int
unsafeSize x = I# (unsafeSize# x)
{-# INLINE unsafeSize #-}

checkOffset :: Int -> Int -> IO ()
checkOffset o l
  | o < 0 = Exception.throwIO $ PokeException o $ "encode offset was negative"
  | o > l = Exception.throwIO $ PokeException o $ "encode overshot end of " ++ show l ++ " byte long buffer"
  | o < l = Exception.throwIO $ PokeException o $ "encode undershot end of " ++ show l ++ " byte long buffer"
  | otherwise = pure ()
{-# INLINE checkOffset #-}

-- | Exception thrown while running 'poke'. Note that other types of
-- exceptions could also be thrown. Invocations of 'fail' in the 'Poke'
-- monad causes this exception to be thrown.
--
-- 'PokeException's are not expected to occur in ordinary circumstances,
-- and usually indicate a programming error.
class GSerializeSize f where
  gunsafeSize# :: f a -> Int#

class GSerializePoke f where
  gpoke :: f a -> Poke ()

class GSerializePeek f where
  gpeek :: Peek (f a)

instance GSerializeSize f => GSerializeSize (G.M1 i c f) where
  gunsafeSize# (G.M1 x) = gunsafeSize# x
  {-# INLINE gunsafeSize# #-}

instance GSerializePoke f => GSerializePoke (G.M1 i c f) where
  gpoke (G.M1 x) = gpoke x
  {-# INLINE gpoke #-}

instance GSerializePeek f => GSerializePeek (G.M1 i c f) where
  gpeek = fmap G.M1 gpeek
  {-# INLINE gpeek #-}

instance Serialize f => GSerializeSize (G.K1 i f) where
  gunsafeSize# (G.K1 x) = unsafeSize# x
  {-# INLINE gunsafeSize# #-}

instance Serialize f => GSerializePoke (G.K1 i f) where
  gpoke (G.K1 x) = poke x
  {-# INLINE gpoke #-}

instance Serialize f => GSerializePeek (G.K1 i f) where
  gpeek = fmap G.K1 peek
  {-# INLINE gpeek #-}

instance GSerializeSize G.U1 where
  gunsafeSize# G.U1 = 0#
  {-# INLINE gunsafeSize# #-}

instance GSerializePoke G.U1 where
  gpoke _ = pure ()
  {-# INLINE gpoke #-}

instance GSerializePeek G.U1 where
  gpeek = pure G.U1
  {-# INLINE gpeek #-}

instance GSerializeSize G.V1 where
  gunsafeSize# = \case {}
  {-# INLINE gunsafeSize# #-}

instance GSerializePoke G.V1 where
  gpoke = \case {}
  {-# INLINE gpoke #-}

instance GSerializePeek G.V1 where
  gpeek = undefined
  {-# INLINE gpeek #-}

instance (GSerializeSize f, GSerializeSize g) => GSerializeSize (f G.:*: g) where
  gunsafeSize# (x G.:*: y) = gunsafeSize# x +# gunsafeSize# y
  {-# INLINE gunsafeSize# #-}

instance (GSerializePoke f, GSerializePoke g) => GSerializePoke (f G.:*: g) where
  gpoke (x G.:*: y) = gpoke x >> gpoke y
  {-# INLINE gpoke #-}

instance (GSerializePeek f, GSerializePeek g) => GSerializePeek (f G.:*: g) where
  gpeek = (G.:*:) <$> gpeek <*> gpeek
  {-# INLINE gpeek #-}

instance (FitsInByte (SumArity (f G.:+: g)), GSerializeSizeSum 0 (f G.:+: g)) => GSerializeSize (f G.:+: g) where
  gunsafeSize# x = sizeOf# (undefined :: Word8) +# gunsafeSizeSum# x (proxy# @0)
  {-# INLINE gunsafeSize# #-}

class KnownNat n => GSerializeSizeSum n f where
  gunsafeSizeSum# :: f a -> Proxy# n -> Int#

class KnownNat n => GSerializePokeSum n f where
  gpokeSum# :: f a -> Proxy# n -> Poke ()

class KnownNat n => GSerializePeekSum n f where
  gpeekSum# :: Word8 -> Proxy# n -> Peek (f a)

instance (GSerializeSizeSum n f, GSerializeSizeSum (n + SumArity f) g, KnownNat n) => GSerializeSizeSum n (f G.:+: g) where
  gunsafeSizeSum# (G.L1 l) _ = gunsafeSizeSum# l (proxy# @n)
  gunsafeSizeSum# (G.R1 r) _ = gunsafeSizeSum# r (proxy# @(n + SumArity f))
  {-# INLINE gunsafeSizeSum# #-}

instance (GSerializePokeSum n f, GSerializePokeSum (n + SumArity f) g, KnownNat n) => GSerializePokeSum n (f G.:+: g) where
  gpokeSum# (G.L1 l) _ = gpokeSum# l (proxy# @n)
  gpokeSum# (G.R1 r) _ = gpokeSum# r (proxy# @(n + SumArity f))

instance (GSerializeSize f, KnownNat n) => GSerializeSizeSum n (G.C1 c f) where
  gunsafeSizeSum# x _ = gunsafeSize# x
  {-# INLINE gunsafeSizeSum# #-}

instance (GSerializePoke f, KnownNat n) => GSerializePokeSum n (G.C1 c f) where
  gpokeSum# x _ = poke tag >> gpoke x
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

instance Serialize () where
  unsafeSize# _ = 0#
  {-# INLINE unsafeSize# #-}
  unsafeConstSize = ConstSize 0
  {-# INLINE unsafeConstSize #-}
  poke _ = pure ()
  {-# INLINE poke #-}
  peek = pure ()
  {-# INLINE peek #-}

instance Serialize a => Serialize [a] where
  unsafeSize# xs = case unsafeConstSize @a of
    ConstSize sz -> unI# $ (length xs) * sz
    _ -> unI# $ sum (unsafeSize <$> xs)
  {-# INLINE unsafeSize# #-}
  unsafeConstSize = VarSize
  {-# INLINE unsafeConstSize #-}
  poke xs = pokeFoldableWith (length xs) xs
  {-# INLINE poke #-}
  peek = do
    size :: Int <- fromIntegral <$> peek @Word64
    xs <- foldM (\xs _ -> do x <- peek; pure $ x : xs) [] [1 .. size]
    pure $! reverse xs
  {-# INLINE peek #-}

pokeFoldableWith :: (Serialize a, Foldable f) => Int -> f a -> Poke ()
pokeFoldableWith size xs = do
  poke (fromIntegral size :: Word64)
  mapM_ poke xs
{-# INLINE pokeFoldableWith #-}

deriveSerializePrimWith (Word8, poke8, peek8)

deriveSerializePrimWith (Word16, pokeLE, peekLE)
deriveSerializePrimWith (LittleEndian Word16, poke16LE . coerce, coerce peek16LE)
deriveSerializePrimWith (BigEndian Word16, poke16BE . coerce, coerce peek16BE)

deriveSerializePrimWith (Word32, pokeLE, peekLE)
deriveSerializePrimWith (LittleEndian Word32, poke32LE . coerce, coerce peek32LE)
deriveSerializePrimWith (BigEndian Word32, poke32BE . coerce, coerce peek32BE)

deriveSerializePrimWith (Word64, pokeLE, peekLE)
deriveSerializePrimWith (LittleEndian Word64, poke64LE . coerce, coerce peek64LE)
deriveSerializePrimWith (BigEndian Word64, poke64BE . coerce, coerce peek64BE)

deriveSerializePrimWith (LittleEndian Int16, poke . fmap (fromIntegral @_ @Word16), fmap (fromIntegral @Word16) <$!> peek)
deriveSerializePrimWith (BigEndian Int16, poke . fmap (fromIntegral @_ @Word16), fmap (fromIntegral @Word16) <$!> peek)

deriveSerializePrimWith (LittleEndian Int32, poke . fmap (fromIntegral @_ @Word32), fmap (fromIntegral @Word32) <$!> peek)
deriveSerializePrimWith (BigEndian Int32, poke . fmap (fromIntegral @_ @Word32), fmap (fromIntegral @Word32) <$!> peek)

deriveSerializePrimWith (LittleEndian Int64, poke . fmap (fromIntegral @_ @Word64), fmap (fromIntegral @Word64) <$!> peek)
deriveSerializePrimWith (BigEndian Int64, poke . fmap (fromIntegral @_ @Word64), fmap (fromIntegral @Word64) <$!> peek)

deriveSerializePrimWith (LittleEndian Int, poke . fmap (fromIntegral @_ @Word64), fmap (fromIntegral @Word64) <$!> peek)
deriveSerializePrimWith (BigEndian Int, poke . fmap (fromIntegral @_ @Word64), fmap (fromIntegral @Word64) <$!> peek)

deriveSerializePrimWith (Int, pokeLE, peekLE)

pokeLE :: Serialize (LittleEndian a) => a -> Poke ()
pokeLE = poke . LittleEndian
{-# INLINE pokeLE #-}

pokeBE :: Serialize (BigEndian a) => a -> Poke ()
pokeBE = poke . BigEndian
{-# INLINE pokeBE #-}

peekLE :: forall a. Serialize (LittleEndian a) => Peek a
peekLE = coerce @(Peek (LittleEndian a)) peek
{-# INLINE peekLE #-}

peekBE :: forall a. Serialize (BigEndian a) => Peek a
peekBE = coerce @(Peek (BigEndian a)) peek
{-# INLINE peekBE #-}

unsafeReinterpretCast :: forall a b. (Prim a, Prim b) => a -> b
unsafeReinterpretCast x = runST $ do
  marr <- newPrimArray @_ @a 1
  writePrimArray marr 0 x
  let marr' = coerce @_ @(MutablePrimArray _ b) marr
  readPrimArray marr' 0
{-# INLINE unsafeReinterpretCast #-}

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `unsafeShiftR` 8)
{-# INLINE unroll #-}

roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldr unstep 0
  where
    unstep a b = fromIntegral a .|. b `unsafeShiftL` 8
{-# INLINE roll #-}

unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}
