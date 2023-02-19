{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant lambda" #-}

module Seraph.Internal where

import Control.Exception qualified as Exception
import Control.Monad ((<$!>))
import Control.Monad qualified as Monad
import Control.Monad.ST (ST, runST)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
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
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Primitive (sizeOf)
import Data.Primitive qualified as Primitive
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Internal qualified
import Data.Tree (Tree)
import Data.Vector qualified as VB
import Data.Vector.Mutable qualified as VBM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Word qualified as Word
import Foreign qualified
import GHC.Exts (Proxy#, proxy#)
import GHC.Float qualified
import GHC.Generics (Generic)
import GHC.Generics qualified as G
import GHC.IO (IO (..))
import GHC.TypeLits (KnownNat, Nat, type (+), type (<=?))
import GHC.TypeLits qualified as TypeLits
import Seraph.Internal.Get
import Seraph.Internal.Put
import Seraph.Internal.Util
import System.ByteOrder qualified as ByteOrder
import System.IO.Unsafe qualified as IO.Unsafe
import Unsafe.Coerce qualified

type DefaultEndian = ByteOrder.LittleEndian

#include "seraph.h"

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

-- Check is the type has a constant size, returning evidence of the check.
-- You will need to use TypeApplications for this.
isConstSize :: forall a. (Serialize a) => SBool (IsConstSize a)
isConstSize = boolSing @(IsConstSize a)
{-# INLINE isConstSize #-}

type family ConstSize b a = r | r -> b where
  ConstSize True _ = Int
  ConstSize False a = a -> Int

class KnownBool (IsConstSize a) => Serialize a where
  -- | Describes whether the type has a constant type.
  -- This is 'False' by default, but is set to 'True' for primitve types such as 'Int'
  type IsConstSize a :: Bool

  type IsConstSize a = False

  -- | Get the size in bytes. The type of this depends on the 'IsConstSize' type family.
  -- If 'IsConstSize' is 'True', then this will be of type 'Int'.
  -- Otherwise, this will be of type @a -> Int@
  --
  -- If the size is not enough to fit the type,
  -- an exception will be thrown when serializing.
  -- An invalid size will __not__ cause undefined behavior
  -- because serialization will still do bounds checking. However, it
  -- is considered a programmer error.
  size :: ConstSize (IsConstSize a) a

  -- | Get the size in bytes.
  -- The type of this value does not depend on the 'IsConstSize' type family.
  -- Use this if we have a value of a type and we don't want to check if the type has a constant size or not,
  -- because we already have a value of it.
  theSize :: a -> Int

  -- | Serialize a value in the 'Put' monoid.
  put :: a -> Put

  -- | Deserialize a value in the 'Get' monad.
  get :: Get a

  default size :: (Generic a, GSerializeSize (G.Rep a), IsConstSize a ~ False) => ConstSize (IsConstSize a) a
  size x = gSize# (G.from x)

  theSize x = case isConstSize @a of
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
    | tag > cur = throwGet $ InvalidSumTag (fromIntegral tag)
    | otherwise = impossible
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

instance Serialize (ByteOrder.Fixed ByteOrder.LittleEndian Float) where
  type IsConstSize _ = True
  size = sizeOf' @Word32
  put = case ByteOrder.targetByteOrder of
    ByteOrder.LittleEndian -> putPrim .# ByteOrder.getFixed
    ByteOrder.BigEndian -> put . GHC.Float.castFloatToWord32 .# ByteOrder.getFixed
  get = case ByteOrder.targetByteOrder of
    ByteOrder.LittleEndian -> coerce @(Get Float) @(Get (ByteOrder.Fixed ByteOrder.LittleEndian Float)) getPrim
    ByteOrder.BigEndian -> (ByteOrder.Fixed #. GHC.Float.castWord32ToFloat . Word.byteSwap32) <$!> get
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance Serialize (ByteOrder.Fixed ByteOrder.BigEndian Float) where
  type IsConstSize _ = True
  size = sizeOf' @Word32
  put = case ByteOrder.targetByteOrder of
    ByteOrder.BigEndian -> putPrim .# ByteOrder.getFixed
    ByteOrder.LittleEndian -> put . GHC.Float.castFloatToWord32 .# ByteOrder.getFixed
  get = case ByteOrder.targetByteOrder of
    ByteOrder.BigEndian -> coerce @(Get Float) @(Get (ByteOrder.Fixed ByteOrder.BigEndian Float)) getPrim
    ByteOrder.LittleEndian -> (ByteOrder.Fixed #. GHC.Float.castWord32ToFloat . Word.byteSwap32) <$!> get
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance Serialize (ByteOrder.Fixed ByteOrder.LittleEndian Double) where
  type IsConstSize _ = True
  size = sizeOf' @Word64
  put = case ByteOrder.targetByteOrder of
    ByteOrder.LittleEndian -> putPrim .# ByteOrder.getFixed
    ByteOrder.BigEndian -> put . GHC.Float.castDoubleToWord64 .# ByteOrder.getFixed
  get = case ByteOrder.targetByteOrder of
    ByteOrder.LittleEndian -> coerce @(Get Double) @(Get (ByteOrder.Fixed ByteOrder.LittleEndian Double)) getPrim
    ByteOrder.BigEndian -> (ByteOrder.Fixed #. GHC.Float.castWord64ToDouble . Word.byteSwap64) <$!> get
  {-# INLINE size #-}
  {-# INLINE put #-}
  {-# INLINE get #-}

instance Serialize (ByteOrder.Fixed ByteOrder.BigEndian Double) where
  type IsConstSize _ = True
  size = sizeOf' @Word64
  put = case ByteOrder.targetByteOrder of
    ByteOrder.BigEndian -> putPrim .# ByteOrder.getFixed
    ByteOrder.LittleEndian -> put . GHC.Float.castDoubleToWord64 .# ByteOrder.getFixed
  get = case ByteOrder.targetByteOrder of
    ByteOrder.BigEndian -> coerce @(Get Double) @(Get (ByteOrder.Fixed ByteOrder.BigEndian Double)) getPrim
    ByteOrder.LittleEndian -> (ByteOrder.Fixed #. GHC.Float.castWord64ToDouble . Word.byteSwap64) <$!> get
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
deriveSerializePrimDefault (Word)

deriveSerializePrim (Int8)
deriveSerializePrim (Int16)
deriveSerializePrim (Int32)
deriveSerializePrim (Int64)
deriveSerializePrimDefault (Int)

deriveSerializePrimDefault (Float)
deriveSerializePrimDefault (Double)

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

instance Serialize Ordering

instance Serialize a => Serialize (Tree a)

instance (Serialize a, Serialize b) => Serialize (a, b)

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b, c)

instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a, b, c, d)

instance Serialize a => Serialize (Maybe a)

instance (Serialize a, Serialize b) => Serialize (Either a b)

instance Serialize a => Serialize [a] where
  size = sizeFoldable
  put = putFoldable
  get = Foldable.foldr' (:) [] <$!> get @(Primitive.Array a)

instance {-# OVERLAPPING #-} Serialize String where
  size = size . T.pack
  put = put . T.pack
  get = T.unpack <$!> get

instance Serialize a => Serialize (NonEmpty a)

instance Serialize a => Serialize (Seq a) where
  size = sizeFoldable
  put s = putFoldableWith (Seq.length s) (Foldable.toList s)
  get = foldGet (flip (Seq.|>)) Seq.empty

instance Serialize IntSet where
  size = sizeFoldableWith IntSet.size IntSet.foldl'
  put is = putFoldableWith (IntSet.size is) (IntSet.toList is)
  get = foldGet IntSet.insert IntSet.empty

instance Serialize a => Serialize (IntMap a) where
  size xs =
    size @Int + case isConstSize @a of
      STrue -> (size @Int + size @a) * IntMap.size xs
      SFalse -> getSum $ Foldable.foldMap' (\x -> Sum (size @Int + size x)) xs
  put im = putFoldableWith (IntMap.size im) (IntMap.toList im)
  get = foldGet2 IntMap.insert IntMap.empty

instance (Ord a, Serialize a) => Serialize (Set a) where
  size = sizeFoldable
  put = putFoldable
  get = foldGet Set.insert Set.empty

instance (Ord a, Serialize a, Serialize b) => Serialize (Map a b) where
  size m =
    size @Int + case (isConstSize @a, isConstSize @b) of
      (STrue, STrue) -> (size @a + size @b) * Map.size m
      (_, _) -> Map.foldlWithKey' (\s k x -> s + theSize @a k + theSize @b x) 0 m
  put m = putFoldableWith (Map.size m) (Map.toList m)
  get = foldGet2 Map.insert Map.empty

instance (Hashable a, Serialize a, Serialize b) => Serialize (HashMap a b) where
  size m =
    size @Int + case (isConstSize @a, isConstSize @b) of
      (STrue, STrue) -> (size @a + size @b) * HashMap.size m
      (_, _) -> HashMap.foldlWithKey' (\s k x -> s + theSize @a k + theSize @b x) 0 m

  -- put m = put @Int (HashMap.size m) <> HashMap.foldMapWithKey (\k x -> put k <> put x) m
  put m = putFoldableWith (HashMap.size m) (HashMap.toList m)
  get = foldGet2 HashMap.Internal.unsafeInsert HashMap.empty

instance Serialize a => Serialize (VB.Vector a) where
  size = sizeFoldable
  put = putFoldable
  get = getArray $ ArrayOps VBM.new VBM.write VB.unsafeFreeze

instance Serialize a => Serialize (Primitive.Array a) where
  size = sizeFoldable
  put = putFoldable
  get = getArray $ ArrayOps (`Primitive.newArray` undefined) Primitive.writeArray Primitive.unsafeFreezeArray

instance Serialize Primitive.ByteArray where
  size bs = size @Int + Primitive.sizeofByteArray bs
  put bs = putByteArray 0 (Primitive.sizeofByteArray bs) bs
  get = getByteArray

instance Serialize Text where
  size (Data.Text.Internal.Text _arr _off len) = size @Int + len
  put (Data.Text.Internal.Text (Data.Text.Array.ByteArray (Primitive.ByteArray -> arr)) off len) =
    putByteArray off len arr
  get = do
    size <- getSize
    unsafeWithSizeGet size \arr i -> do
      -- can't use getByteArray here because we need to ensure it is UTF-8
      -- wait for text to allow decoding UTF-8 from ByteArray
      Text.Encoding.decodeUtf8 $! pinnedToByteString i size arr

instance Serialize ShortByteString where
  size bs = size @Int + SBS.length bs
  {-# INLINE size #-}

#if MIN_VERSION_bytestring(0,11,1)
  put (SBS.SBS arr#) = put (Primitive.ByteArray arr#)
  get = (\(Primitive.ByteArray arr#) -> SBS.SBS arr#) <$!> get
#else
  put = put . SBS.fromShort
  get = SBS.toShort <$!> get
#endif

instance Serialize ByteString where
  size bs = size @Int + B.length bs
  {-# INLINE size #-}
  put (B.Internal.PS fp off len) =
    putSize len <> unsafeWithSizedPutIO len \marr i ->
      Foreign.withForeignPtr fp \p -> do
        let p' :: Primitive.Ptr Word8 = p `Foreign.plusPtr` off
        Primitive.copyPtrToMutableByteArray marr i p' len
  get = do
    size <- getSize
    unsafeWithSizeGet size \arr i ->
      B.copy $! pinnedToByteString i size arr

putByteArray :: Int -> Int -> Primitive.ByteArray -> Put
putByteArray off len arr =
  putSize len <> unsafeWithSizedPut len \marr i ->
    Primitive.copyByteArray marr i arr off len
{-# INLINE putByteArray #-}

getByteArray :: Get Primitive.ByteArray
getByteArray = do
  len <- getSize
  unsafeWithSizeGet len \arr i -> runST $ do
    marr <- Primitive.newByteArray len
    Primitive.copyByteArray marr 0 arr i len
    Primitive.unsafeFreezeByteArray marr
{-# INLINE getByteArray #-}

type Foldl s a = forall b. (b -> a -> b) -> b -> s -> b

sizeFoldable :: forall a f. (Serialize a, Foldable f) => f a -> Int
sizeFoldable = sizeFoldableWith length Foldable.foldl'
{-# INLINE sizeFoldable #-}

sizeFoldableWith :: forall a s. (Serialize a) => (s -> Int) -> Foldl s a -> s -> Int
sizeFoldableWith length foldl' = \xs ->
  size @Int + case isConstSize @a of
    STrue -> size @a * length xs
    SFalse -> foldl' (\z x -> z + size x) 0 xs
{-# INLINE sizeFoldableWith #-}

putSize :: Int -> Put
putSize = put
{-# INLINE putSize #-}

getSize :: Get Int
getSize = get
{-# INLINE getSize #-}

type FoldM s a = forall m b. (Monad m) => (b -> a -> m b) -> b -> s -> m b

putFoldableWith' :: (Serialize a) => Int -> FoldM s a -> s -> Put
putFoldableWith' len foldM xs =
  put len <> Put# \marr i ->
    foldM (\i x -> runPut# (put x) marr i) i xs
{-# INLINE putFoldableWith' #-}

putFoldableWith :: (Serialize a, Foldable f) => Int -> f a -> Put
putFoldableWith i = putFoldableWith' i Foldable.foldlM
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

data ArrayOps :: Type -> Type -> Type -> Type where
  ArrayOps ::
    (Int -> ST s r) ->
    (r -> Int -> a -> ST s ()) ->
    (r -> ST s v) ->
    ArrayOps s v a

getArray :: Serialize a => (forall s. ArrayOps s v a) -> Get v
getArray (ArrayOps new write freeze) = do
  size <- get @Int
  v <- unsafeLiftST $ new size
  Foldable.forM_ [1 .. size] \i -> get >>= unsafeLiftST . write v (i - 1)
  unsafeLiftST $ freeze v
{-# INLINE getArray #-}

foldGet :: (Serialize a) => (a -> b -> b) -> b -> Get b
foldGet f z = do
  size <- getSize
  Foldable.foldlM (\xs _ -> do x <- get; pure $! f x xs) z [1 .. size]
{-# INLINE foldGet #-}

foldGet2 :: (Serialize a, Serialize b) => (a -> b -> c -> c) -> c -> Get c
foldGet2 f z = do
  size <- getSize
  Foldable.foldlM (\xs _ -> do x <- get; y <- get; pure $! f x y xs) z [1 .. size]
{-# INLINE foldGet2 #-}

-- | Same as 'encode' but inside the 'IO' monad.
-- This is useful to throw exceptions on evaluation of the 'IO' action instead of evaluation of the inner value.
-- See 'evaluate' for more info.
encodeIO :: Serialize a => a -> IO ByteString
encodeIO x = do
  marr <- Primitive.newPinnedByteArray sz
  Monad.void $ runPut# (put x) marr 0
  pinnedToByteString 0 sz <$!> Primitive.unsafeFreezeByteArray marr
  where
    sz = theSize x

-- | Encode a value into a 'ByteString'.
encode :: Serialize a => a -> ByteString
encode = IO.Unsafe.unsafeDupablePerformIO . encodeIO

-- | Same as 'decode'' but in the 'IO' monad.
-- This is useful to throw exceptions on evaluation of the 'IO' action instead of evaluation of the inner value.
-- See 'evaluate' for more info.
decodeIO :: Serialize a => ByteString -> IO a
decodeIO bs = do
  GR _ x <- runGet# get (GE arr l) i
  pure x
  where
    !(arr, i, l) = unpackByteString bs

-- | Decode a value from a 'ByteString'.
-- If the 'ByteString' is invalid, this will return 'Left' with some 'GetException'.
decode :: Serialize a => ByteString -> Either GetException a
decode = IO.Unsafe.unsafeDupablePerformIO . Exception.try . decodeIO

-- | Same as 'decode', but will throw an exception if the 'Either' is 'Left'
decode' :: Serialize a => ByteString -> a
decode' bs = case decode bs of
  Left e -> Exception.throw e
  Right x -> x
