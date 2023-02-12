{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

{-# HLINT ignore "Redundant bracket" #-}

module Serialize.Internal where

import Control.Exception qualified as Exception
import Control.Monad ((<$!>))
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Foldable (foldMap', foldlM)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Monoid (Dual (..), Product (..), Sum (..))
import Data.Primitive (sizeOf#)
import Data.Primitive qualified as Primitive
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Array qualified
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Internal qualified
import Data.Tree (Tree)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign qualified
import GHC.Float qualified
import GHC.Generics (Generic)
import GHC.Generics qualified as G
import GHC.IO (IO (..))
import GHC.TypeLits (KnownNat, Nat, type (+), type (<=?))
import GHC.TypeLits qualified as TypeLits
import Serialize.Internal.Exts
import Serialize.Internal.Get
import Serialize.Internal.Put
import Serialize.Internal.Util
import System.ByteOrder qualified as ByteOrder
import System.IO.Unsafe qualified as IO.Unsafe

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
  constSize# :: Proxy# a -> ConstSize#
  put :: a -> Put
  get :: Get a

  default size# :: (Generic a, GSerializeSize (G.Rep a)) => a -> Int#
  size# x = gSize# (G.from x)
  {-# INLINE size# #-}

  constSize# _ = VarSize#
  {-# INLINE constSize# #-}

  default put :: (Generic a, GSerializePut (G.Rep a)) => a -> Put
  put = gPut . G.from

  default get :: (Generic a, GSerializeGet (G.Rep a)) => Get a
  get = G.to <$!> gGet

gConstSize :: forall a. GSerializeConstSize (G.Rep a) => Proxy# a -> ConstSize#
gConstSize _ = gConstSize# (proxy# @((G.Rep a) _))
{-# INLINE gConstSize #-}

primConstSize# :: forall a. Serialize a => Int#
primConstSize# = case constSize# (proxy# @a) of
  ConstSize# i# -> i#
  VarSize# -> error "Was not ConstSize#"
{-# INLINE primConstSize# #-}

primConstSize :: forall a. Serialize a => Int
primConstSize = I# (primConstSize# @a)
{-# INLINE primConstSize #-}

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
  gPut x = gPutSum# x (proxy# @0)
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
  gPutSum# :: f a -> Proxy# n -> Put

class KnownNat n => GSerializeGetSum n f where
  gGetSum# :: Word# -> Proxy# n -> Get (f a)

instance (GSerializeSizeSum n f, GSerializeSizeSum (n + SumArity f) g) => GSerializeSizeSum n (f G.:+: g) where
  gSizeSum# (G.L1 l) _ = gSizeSum# l (proxy# @n)
  gSizeSum# (G.R1 r) _ = gSizeSum# r (proxy# @(n + SumArity f))
  {-# INLINE gSizeSum# #-}

instance (GSerializePutSum n f, GSerializePutSum (n + SumArity f) g) => GSerializePutSum n (f G.:+: g) where
  gPutSum# (G.L1 l) _ = gPutSum# l (proxy# @n)
  gPutSum# (G.R1 r) _ = gPutSum# r (proxy# @(n + SumArity f))
  {-# INLINE gPutSum# #-}

instance (GSerializeGetSum n f, GSerializeGetSum (n + SumArity f) g) => GSerializeGetSum n (f G.:+: g) where
  gGetSum# tag# p#
    | W# tag# < sizeL = G.L1 <$!> gGetSum# tag# p#
    | otherwise = G.R1 <$!> gGetSum# tag# (proxy# @(n + SumArity f))
    where
      sizeL = fromInteger (TypeLits.natVal' (proxy# @(n + SumArity f)))
  {-# INLINE gGetSum# #-}

instance (GSerializeSize f, KnownNat n) => GSerializeSizeSum n (G.C1 c f) where
  gSizeSum# x _ = gSize# x
  {-# INLINE gSizeSum# #-}

instance (GSerializePut f, KnownNat n) => GSerializePutSum n (G.C1 c f) where
  gPutSum# x _ = put tag <> gPut x
    where
      tag = fromInteger @Word8 (TypeLits.natVal' (proxy# @n))

instance (GSerializeGet f, KnownNat n) => GSerializeGetSum n (G.C1 c f) where
  gGetSum# tag _
    | W# tag == cur = gGet
    | W# tag > cur = Exception.throw (InvalidSumTag cur (W# tag))
    | otherwise = error "Implementation error"
    where
      cur = fromInteger @Word (TypeLits.natVal' (proxy# @n))

type SumArity :: (Type -> Type) -> Nat
type family SumArity a where
  SumArity (G.C1 c a) = 1
  SumArity (x G.:+: y) = SumArity x + SumArity y

type FitsInByte n = FitsInByteResult (n <=? 255)

type FitsInByteResult :: Bool -> Constraint
type family FitsInByteResult b where
  FitsInByteResult True = ()
  FitsInByteResult False = TypeLits.TypeError (TypeLits.Text "Generic deriving of Serialize instances can only be used on datatypes with fewer than 256 constructors.")

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Int) where
  size# _ = sizeOf## @Int64
  constSize# _ = ConstSize# (sizeOf## @Int64)
  put = put . fromIntegral @(ByteOrder.Fixed b Int) @(ByteOrder.Fixed b Int64)
  get = fromIntegral @(ByteOrder.Fixed b Int64) @(ByteOrder.Fixed b Int) <$!> get
  {-# INLINE size# #-}
  {-# INLINE constSize# #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Word) where
  size# _ = sizeOf## @Word64
  constSize# _ = ConstSize# (sizeOf## @Word64)
  put = put . fromIntegral @(ByteOrder.Fixed b Word) @(ByteOrder.Fixed b Word64)
  get = fromIntegral @(ByteOrder.Fixed b Word64) @(ByteOrder.Fixed b Word) <$!> get
  {-# INLINE size# #-}
  {-# INLINE constSize# #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Float) where
  size# _ = sizeOf## @Word32
  constSize# _ = ConstSize# (sizeOf## @Word32)
  put = put . ByteOrder.Fixed @b #. GHC.Float.castFloatToWord32 .# ByteOrder.getFixed @b
  get = (ByteOrder.Fixed @b #. GHC.Float.castWord32ToFloat .# ByteOrder.getFixed @b) <$!> get
  {-# INLINE size# #-}
  {-# INLINE constSize# #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Double) where
  size# _ = sizeOf## @Word64
  constSize# _ = ConstSize# (sizeOf## @Word64)
  put = put . ByteOrder.Fixed @b #. GHC.Float.castDoubleToWord64 .# ByteOrder.getFixed @b
  get = (ByteOrder.Fixed @b #. GHC.Float.castWord64ToDouble .# ByteOrder.getFixed @b) <$!> get
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

deriveSerializePrimLE (Float)
deriveSerializePrimLE (Double)
deriveSerializeNewtype (Dual)
deriveSerializeNewtype (Sum)
deriveSerializeNewtype (Product)

instance Serialize Bool

instance Serialize Ordering

instance Serialize a => Serialize (Tree a)

instance (Serialize a, Serialize b) => Serialize (a, b)

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b, c)

instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a, b, c, d)

instance Serialize a => Serialize (Maybe a)

instance (Serialize a, Serialize b) => Serialize (Either a b)

instance Serialize a => Serialize [a] where
  size# = sizeFoldable
  put = putFoldable
  get = reverse <$!> foldGet (:) []

instance Serialize a => Serialize (Seq a) where
  size# = sizeFoldable
  put = putFoldable
  get = foldGet (flip (Seq.|>)) Seq.empty

instance Serialize IntSet where
  size# is = sizeOf## @Word32 +# (primConstSize# @Int) *# unI# (IntSet.size is)
  put is =
    put (fromIntegral @Int @Word32 $ IntSet.size is)
      <> IntSet.foldr (mappend . put) mempty is
  get = foldGet IntSet.insert IntSet.empty

instance Serialize a => Serialize (IntMap a) where
  size# xs =
    sizeOf## @Word32 +# case constSize# (proxy# @a) of
      ConstSize# i# -> (primConstSize# @Int +# i#) *# unI# (IntMap.size xs)
      VarSize# -> unI# $ getSum $ foldMap' (\x -> Sum (size x + primConstSize @Int)) xs
  put im =
    put (fromIntegral @Int @Word32 (IntMap.size im))
      <> IntMap.foldMapWithKey (\i x -> put i <> put x) im
  get = foldGet2 IntMap.insert IntMap.empty

-- instance (Serialize a, Serialize b) => Serialize (Map a b) where
--   size# xs = sizeOf## @Word32

instance Serialize Primitive.ByteArray where
  size# bs = unI# $ Primitive.sizeofByteArray bs
  {-# INLINE size# #-}
  put bs = putByteArray 0 (Primitive.sizeofByteArray bs) bs
  get = getByteArray

instance Serialize Text where
  size# (Data.Text.Internal.Text _arr _off (I# len#)) = sizeOf## @Word32 +# len#
  {-# INLINE size# #-}
  put (Data.Text.Internal.Text (Data.Text.Array.ByteArray (Primitive.ByteArray -> arr)) off len) =
    putByteArray off len arr
  get = do
    size <- fromIntegral @Word32 @Int <$> get
    unsafeWithGet size \arr i -> do
      -- can't use getByteArray here because we need to ensure it is UTF-8
      -- wait for text to allow decoding UTF-8 from ByteArray
      pure $! Text.Encoding.decodeUtf8 $! pinnedToByteString i size arr

instance Serialize ShortByteString where
  size# bs = sizeOf## @Word32 +# unI# (SBS.length bs)
  {-# INLINE size# #-}
  put (SBS.SBS arr#) = put (Primitive.ByteArray arr#)
  get = (\(Primitive.ByteArray arr#) -> SBS.SBS arr#) <$> get

instance Serialize ByteString where
  size# bs = sizeOf## @Word32 +# unI# (B.length bs)
  {-# INLINE size# #-}
  put (B.Internal.PS fp off len) =
    put (fromIntegral @Int @Word32 len) <> unsafeWithPut len \marr i ->
      Foreign.withForeignPtr fp \p -> do
        let p' :: Primitive.Ptr Word8 = p `Foreign.plusPtr` off
        Primitive.copyPtrToMutableByteArray marr i p' len
  get = do
    size <- fromIntegral @Word32 @Int <$> get
    unsafeWithGet size \arr i ->
      pure $! B.copy $! pinnedToByteString i size arr

putByteArray :: Int -> Int -> Primitive.ByteArray -> Put
putByteArray off len arr =
  put (fromIntegral @Int @Word32 len) <> unsafeWithPut len \marr i ->
    Primitive.copyByteArray marr i arr off len
{-# INLINE putByteArray #-}

getByteArray :: Get Primitive.ByteArray
getByteArray = do
  len <- fromIntegral @Word32 @Int <$> get
  unsafeWithGet len \arr i -> do
    marr <- Primitive.newByteArray len
    Primitive.copyByteArray marr 0 arr i len
    Primitive.unsafeFreezeByteArray marr
{-# INLINE getByteArray #-}

sizeFoldable :: forall a f. (Serialize a, Foldable f) => f a -> Int#
sizeFoldable xs =
  sizeOf## @Word32 +# case constSize## @a of
    ConstSize# sz# -> unI# (length xs) *# sz#
    _ -> unI# $ getSum $ foldMap' (Sum #. size) xs
{-# INLINE sizeFoldable #-}

-- sizeMap :: forall a b m. (Serialize a, Serialize b) => (m -> [(a, b)]) -> (m -> Int) -> m -> Int#
-- sizeMap toList length m = 
--   sizeOf## @Word32 +# case (# constSize## @a, constSize## @b #) of
--     (# ConstSize# sz#, ConstSize# sz'# #) -> (sz# +# sz'#) *# unI# (length m)
--     (# VarSize#, ConstSize# sz# #) -> foldMap' toList m
-- sizeBifoldable :: forall a b f. (Serialize a, Serialize b, Bifoldable f, Foldable (f a)) => f a -> Int#
-- sizeBifoldable xs =
-- {-# INLINE sizeBifoldable #-}

putFoldableWith :: (Serialize a, Foldable f) => Int -> f a -> Put
putFoldableWith len xs = put (fromIntegral @Int @Word32 len) <> foldMap put xs
{-# INLINE putFoldableWith #-}

putFoldable :: (Serialize a, Foldable f) => f a -> Put
putFoldable xs = putFoldableWith (length xs) xs
{-# INLINE putFoldable #-}

putBifoldable :: (Serialize a, Serialize b, Bifoldable f, Foldable (f a)) => f a b -> Put
putBifoldable xs = putBifoldableWith (length xs) xs
{-# INLINE putBifoldable #-}

putBifoldableWith :: (Serialize a, Serialize b, Bifoldable f) => Int -> f a b -> Put
putBifoldableWith len xs =
  put (fromIntegral @Int @Word32 len)
    <> bifoldMap put put xs
{-# INLINE putBifoldableWith #-}

foldGet :: (Serialize a) => (a -> b -> b) -> b -> Get b
foldGet f z = do
  size <- fromIntegral @Word32 @Int <$> get
  foldlM (\xs _ -> do x <- get; pure $! f x xs) z [1 .. size]
{-# INLINE foldGet #-}

foldGet2 :: (Serialize a, Serialize b) => (a -> b -> c -> c) -> c -> Get c
foldGet2 f z = do
  size <- fromIntegral @Word32 @Int <$> get
  foldlM (\xs _ -> do x <- get; y <- get; pure $! f x y xs) z [1 .. size]
{-# INLINE foldGet2 #-}

encodeIO :: Serialize a => a -> IO ByteString
encodeIO x = do
  marr@(Primitive.MutableByteArray marr#) <- Primitive.newPinnedByteArray sz
  IO \s# -> case runPut# (put x) (PE# marr# (unI# sz)) (PS# 0#) s# of
    PR# s# _ -> (# s#, () #)
  pinnedToByteString 0 sz <$!> Primitive.unsafeFreezeByteArray marr
  where
    sz = size x

encode :: Serialize a => a -> ByteString
encode = IO.Unsafe.unsafeDupablePerformIO . encodeIO

decodeIO :: Serialize a => ByteString -> IO a
decodeIO bs = do
  IO \s# -> case runGet# get (GE# arr# l#) (GS# i#) s# of
    GR# s# _ x -> (# s#, x #)
  where
    !(# arr#, i#, l# #) = unpackByteString# bs

decode :: Serialize a => ByteString -> Either GetException a
decode = IO.Unsafe.unsafeDupablePerformIO . Exception.try . decodeIO

decode' :: Serialize a => ByteString -> a
decode' bs = case decode bs of
  Left e -> Exception.throw e
  Right x -> x
