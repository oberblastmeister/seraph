{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use if" #-}

module Serialize.Internal where

import Control.Exception qualified as Exception
import Control.Monad ((<$!>))
import Control.Monad.ST (runST)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Char qualified as Char
import Data.Foldable (foldMap', foldlM)
import Data.Functor ((<&>))
import Data.HashMap.Internal qualified as HashMap.Internal
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Primitive (sizeOf)
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

data SBool :: Bool -> Type where
  STrue :: SBool True
  SFalse :: SBool False

class KnownBool (b :: Bool) where
  boolSing :: SBool b

instance KnownBool True where
  boolSing = STrue
  {-# INLINE boolSing #-}

instance KnownBool False where
  boolSing = SFalse
  {-# INLINE boolSing #-}

isPrim :: forall a. (Serialize a) => SBool (IsConstSize a)
isPrim = boolSing @(IsConstSize a)
{-# INLINE isPrim #-}

type family ConstSize b a = r | r -> b where
  ConstSize True _ = Int
  ConstSize False a = a -> Int

class KnownBool (IsConstSize a) => Serialize a where
  type IsConstSize a :: Bool
  type IsConstSize a = False

  size :: ConstSize (IsConstSize a) a
  theSize :: a -> Int
  put :: a -> Put
  get :: Get a

  default size :: (Generic a, GSerializeSize (G.Rep a), IsConstSize a ~ False) => ConstSize (IsConstSize a) a
  size x = gSize# (G.from x)

  theSize x = case isPrim @a of
    STrue -> size @a
    SFalse -> size x
  {-# INLINE theSize #-}

  default put :: (Generic a, GSerializePut (G.Rep a)) => a -> Put
  put = gPut . G.from

  default get :: (Generic a, GSerializeGet (G.Rep a)) => Get a
  get = G.to <$> gGet

class GSerializeSize f where
  gSize# :: f a -> Int

class GSerializeConstSize f where
  gConstSize# :: Proxy# (f a) -> Int

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
  gSize# (G.K1 x) = theSize x
  {-# INLINE gSize# #-}

instance (Serialize a, IsConstSize a ~ True) => GSerializeConstSize (G.K1 i a) where
  gConstSize# _ = size @a
  {-# INLINE gConstSize# #-}

instance Serialize f => GSerializePut (G.K1 i f) where
  gPut (G.K1 x) = put x
  {-# INLINE gPut #-}

instance Serialize f => GSerializeGet (G.K1 i f) where
  gGet = fmap G.K1 get
  {-# INLINE gGet #-}

instance GSerializeSize G.U1 where
  gSize# G.U1 = 0
  {-# INLINE gSize# #-}

instance GSerializeConstSize G.U1 where
  gConstSize# _ = 0
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
  gSize# (x G.:*: y) = gSize# x + gSize# y
  {-# INLINE gSize# #-}

instance (GSerializeConstSize f, GSerializeConstSize g) => GSerializeConstSize (f G.:*: g) where
  gConstSize# _ = gConstSize# (proxy# @(f _)) + gConstSize# (proxy# @(g _))
  {-# INLINE gConstSize# #-}

instance (GSerializePut f, GSerializePut g) => GSerializePut (f G.:*: g) where
  gPut (x G.:*: y) = gPut x <> gPut y
  {-# INLINE gPut #-}

instance (GSerializeGet f, GSerializeGet g) => GSerializeGet (f G.:*: g) where
  gGet = (G.:*:) <$> gGet <*> gGet
  {-# INLINE gGet #-}

instance (FitsInByte (SumArity (f G.:+: g)), GSerializeSizeSum 0 (f G.:+: g)) => GSerializeSize (f G.:+: g) where
  gSize# x = size @Word8 + gSizeSum# x (proxy# @0)
  {-# INLINE gSize# #-}

instance (FitsInByte (SumArity (f G.:+: g)), GSerializePutSum 0 (f G.:+: g)) => GSerializePut (f G.:+: g) where
  gPut x = gPutSum# x (proxy# @0)
  {-# INLINE gPut #-}

instance (FitsInByte (SumArity (f G.:+: g)), GSerializeGetSum 0 (f G.:+: g)) => GSerializeGet (f G.:+: g) where
  gGet = do
    tag <- get @Word8
    gGetSum# tag (proxy# @0)
  {-# INLINE gGet #-}

class KnownNat n => GSerializeSizeSum n f where
  gSizeSum# :: f a -> Proxy# n -> Int

class KnownNat n => GSerializePutSum n f where
  gPutSum# :: f a -> Proxy# n -> Put

class KnownNat n => GSerializeGetSum n f where
  gGetSum# :: Word8 -> Proxy# n -> Get (f a)

instance (GSerializeSizeSum n f, GSerializeSizeSum (n + SumArity f) g) => GSerializeSizeSum n (f G.:+: g) where
  gSizeSum# (G.L1 l) _ = gSizeSum# l (proxy# @n)
  gSizeSum# (G.R1 r) _ = gSizeSum# r (proxy# @(n + SumArity f))
  {-# INLINE gSizeSum# #-}

instance (GSerializePutSum n f, GSerializePutSum (n + SumArity f) g) => GSerializePutSum n (f G.:+: g) where
  gPutSum# (G.L1 l) _ = gPutSum# l (proxy# @n)
  gPutSum# (G.R1 r) _ = gPutSum# r (proxy# @(n + SumArity f))
  {-# INLINE gPutSum# #-}

instance (GSerializeGetSum n f, GSerializeGetSum (n + SumArity f) g) => GSerializeGetSum n (f G.:+: g) where
  gGetSum# tag p#
    | tag < sizeL = G.L1 <$> gGetSum# tag p#
    | otherwise = G.R1 <$> gGetSum# tag (proxy# @(n + SumArity f))
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
  {-# INLINE gPutSum# #-}

instance (GSerializeGet f, KnownNat n) => GSerializeGetSum n (G.C1 c f) where
  gGetSum# tag _
    | tag == cur = gGet
    | tag > cur = Exception.throw (InvalidSumTag (fromIntegral cur) (fromIntegral tag))
    | otherwise = error "Implementation error"
    where
      cur = fromInteger @Word8 (TypeLits.natVal' (proxy# @n))
  {-# INLINE gGetSum# #-}

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
  type IsConstSize _ = True
  size = sizeOf' @Int64
  put = put . fromIntegral @(ByteOrder.Fixed b Int) @(ByteOrder.Fixed b Int64)
  get = fromIntegral @(ByteOrder.Fixed b Int64) @(ByteOrder.Fixed b Int) <$!> get
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Word) where
  type IsConstSize _ = True
  size = sizeOf' @Word64
  put = put . fromIntegral @(ByteOrder.Fixed b Word) @(ByteOrder.Fixed b Word64)
  get = fromIntegral @(ByteOrder.Fixed b Word64) @(ByteOrder.Fixed b Word) <$!> get
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

-- TODO: optimize this so that we don't have to do ffi call when same endianness
instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Float) where
  type IsConstSize _ = True
  size = sizeOf' @Word32
  put = put . ByteOrder.Fixed @b #. GHC.Float.castFloatToWord32 .# ByteOrder.getFixed @b
  get = (ByteOrder.Fixed @b #. GHC.Float.castWord32ToFloat .# ByteOrder.getFixed @b) <$!> get
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b Double) where
  type IsConstSize _ = True
  size = sizeOf' @Word64
  put = put . ByteOrder.Fixed @b #. GHC.Float.castDoubleToWord64 .# ByteOrder.getFixed @b
  get = (ByteOrder.Fixed @b #. GHC.Float.castWord64ToDouble .# ByteOrder.getFixed @b) <$!> get
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance Serialize Char where
  type IsConstSize _ = True
  size = size @Int
  put = put . Char.ord
  get = Char.chr <$!> get
  {-# INLINE size #-}
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

-- deriveSerializeNewtype (Dual)
-- deriveSerializeNewtype (Sum)
-- deriveSerializeNewtype (Product)

instance Serialize Bool where
  type IsConstSize _ = True
  size = size @Word8
  put x = put @Word8 $ case x of
    True -> 0
    False -> 1
  get =
    get @Word8 <&> \case
      0 -> True
      1 -> False
      _ -> undefined
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance Serialize Ordering where
  type IsConstSize _ = True
  size = size @Word8
  put o = put @Word8 $ case o of
    LT -> 0
    EQ -> 1
    GT -> 2
  get =
    get @Word8 <&> \case
      0 -> LT
      1 -> EQ
      2 -> GT
      _ -> undefined
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance Serialize a => Serialize (Tree a)

instance (Serialize a, Serialize b) => Serialize (a, b)

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b, c)

instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a, b, c, d)

instance Serialize a => Serialize (Maybe a) where
  size m = size @Word8 + maybe 0 theSize m
  put m = case m of
    Nothing -> put @Word8 0
    Just x -> put @Word8 1 <> put x
  get =
    get @Word8 >>= \case
      0 -> pure Nothing
      1 -> Just <$> get
      _ -> undefined

instance (Serialize a, Serialize b) => Serialize (Either a b) where
  size e = size @Word8 + either theSize theSize e
  put e = case e of
    Left x -> put @Word8 0 <> put x
    Right x -> put @Word8 1 <> put x
  get =
    get @Word8 >>= \case
      0 -> Left <$> get
      1 -> Right <$> get
      _ -> undefined

instance Serialize a => Serialize [a] where
  size = sizeFoldable
  put = putFoldable
  get = reverse <$!> foldGet (:) []

instance Serialize a => Serialize (Seq a) where
  size = sizeFoldable
  put = putFoldable
  get = foldGet (flip (Seq.|>)) Seq.empty

instance Serialize IntSet where
  size is = size @Int + size @Int * IntSet.size is
  put is =
    putSize (IntSet.size is)
      <> IntSet.foldr (mappend . put) mempty is
  get = foldGet IntSet.insert IntSet.empty

instance Serialize a => Serialize (IntMap a) where
  size xs =
    size @Int + case isPrim @a of
      STrue -> (size @Int + size @a) * IntMap.size xs
      SFalse -> getSum $ foldMap' (\x -> Sum (size @Int + size x)) xs
  put im =
    putSize (IntMap.size im)
      <> IntMap.foldMapWithKey (\i x -> put i <> put x) im
  get = foldGet2 IntMap.insert IntMap.empty

instance (Ord a, Serialize a, Serialize b) => Serialize (Map a b) where
  size m =
    size @Int + case (isPrim @a, isPrim @b) of
      (STrue, STrue) -> (size @a + size @b) * Map.size m
      (_, _) -> Map.foldlWithKey' (\s k x -> s + theSize @a k + theSize @b x) 0 m
  put m = putSize (Map.size m) <> Map.foldMapWithKey (\k x -> put k <> put x) m
  get = foldGet2 Map.insert Map.empty

instance (Hashable a, Serialize a, Serialize b) => Serialize (HashMap a b) where
  size m =
    size @Int + case (isPrim @a, isPrim @b) of
      (STrue, STrue) -> (size @a + size @b) * HashMap.size m
      (_, _) -> HashMap.foldlWithKey' (\s k x -> s + theSize @a k + theSize @b x) 0 m
  put m = put @Int (HashMap.size m) <> HashMap.foldMapWithKey (\k x -> put k <> put x) m
  get = foldGet2 HashMap.Internal.insert HashMap.empty

instance Serialize Primitive.ByteArray where
  size bs = size @Int + Primitive.sizeofByteArray bs
  {-# INLINE size #-}
  put bs = putByteArray 0 (Primitive.sizeofByteArray bs) bs
  get = getByteArray

instance Serialize Text where
  size (Data.Text.Internal.Text _arr _off len) = size @Int + len
  {-# INLINE size #-}
  put (Data.Text.Internal.Text (Data.Text.Array.ByteArray (Primitive.ByteArray -> arr)) off len) =
    putByteArray off len arr
  get = do
    size <- getSize
    unsafeWithGet size \arr i -> do
      -- can't use getByteArray here because we need to ensure it is UTF-8
      -- wait for text to allow decoding UTF-8 from ByteArray
      Text.Encoding.decodeUtf8 $! pinnedToByteString i size arr

instance Serialize ShortByteString where
  size bs = size @Int + (SBS.length bs)
  {-# INLINE size #-}
#if MIN_VERSION_bytestring(0,11,1)
  put (SBS.SBS arr#) = put (Primitive.ByteArray arr#)
  get = (\(Primitive.ByteArray arr#) -> SBS.SBS arr#) <$!> get
#else
  put = put . SBS.fromShort
  get = SBS.toShort <$!> get
#endif

instance Serialize ByteString where
  size bs = size @Int + (B.length bs)
  {-# INLINE size #-}
  put (B.Internal.PS fp off len) =
    putSize len <> unsafeWithPutIO len \marr i ->
      Foreign.withForeignPtr fp \p -> do
        let p' :: Primitive.Ptr Word8 = p `Foreign.plusPtr` off
        Primitive.copyPtrToMutableByteArray marr i p' len
  get = do
    size <- getSize
    unsafeWithGet size \arr i ->
      B.copy $! pinnedToByteString i size arr

-- instance Serialize BigNum

putByteArray :: Int -> Int -> Primitive.ByteArray -> Put
putByteArray off len arr =
  putSize len <> unsafeWithPut len \marr i ->
    Primitive.copyByteArray marr i arr off len
{-# INLINE putByteArray #-}

getByteArray :: Get Primitive.ByteArray
getByteArray = do
  len <- getSize
  unsafeWithGet len \arr i -> runST $ do
    marr <- Primitive.newByteArray len
    Primitive.copyByteArray marr 0 arr i len
    Primitive.unsafeFreezeByteArray marr
{-# INLINE getByteArray #-}

sizeFoldable :: forall a f. (Serialize a, Foldable f) => f a -> Int
sizeFoldable xs =
  size @Int + case isPrim @a of
    STrue -> size @a * length xs
    SFalse -> getSum $ foldMap' (Sum #. size) xs
{-# INLINE sizeFoldable #-}

putSize :: Int -> Put
putSize = put
{-# INLINE putSize #-}

getSize :: Get Int
getSize = get
{-# INLINE getSize #-}

putFoldableWith :: (Serialize a, Foldable f) => Int -> f a -> Put
putFoldableWith len xs = putSize len <> foldMap put xs
{-# INLINE putFoldableWith #-}

putFoldable :: (Serialize a, Foldable f) => f a -> Put
putFoldable xs = putFoldableWith (length xs) xs
{-# INLINE putFoldable #-}

putBifoldable :: (Serialize a, Serialize b, Bifoldable f, Foldable (f a)) => f a b -> Put
putBifoldable xs = putBifoldableWith (length xs) xs
{-# INLINE putBifoldable #-}

putBifoldableWith :: (Serialize a, Serialize b, Bifoldable f) => Int -> f a b -> Put
putBifoldableWith len xs = putSize len <> bifoldMap put put xs
{-# INLINE putBifoldableWith #-}

foldGet :: (Serialize a) => (a -> b -> b) -> b -> Get b
foldGet f z = do
  size <- getSize
  foldlM (\xs _ -> do x <- get; pure $! f x xs) z [1 .. size]
{-# INLINE foldGet #-}

foldGet2 :: (Serialize a, Serialize b) => (a -> b -> c -> c) -> c -> Get c
foldGet2 f z = do
  size <- getSize
  foldlM (\xs _ -> do x <- get; y <- get; pure $! f x y xs) z [1 .. size]
{-# INLINE foldGet2 #-}

encodeIO :: Serialize a => a -> IO ByteString
encodeIO x = do
  marr@(Primitive.MutableByteArray marr#) <- Primitive.newPinnedByteArray sz
  IO \s# -> case runPut# (put x) (PE# marr# (unI# sz)) (PS# 0#) s# of
    PR# s# _ -> (# s#, () #)
  pinnedToByteString 0 sz <$!> Primitive.unsafeFreezeByteArray marr
  where
    sz = theSize x

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
