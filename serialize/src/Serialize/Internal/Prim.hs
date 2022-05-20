{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Serialize.Internal.Prim where

import Control.Monad (join)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.|.))
import Data.Primitive (Prim, sizeOf#)
import Data.Primitive qualified as Primitive
import Data.Primitive.Ptr qualified as Primitive.Ptr
import Data.Primitive.Ptr qualified as Ptr
import Data.Word
import Foreign qualified
import GHC.Exts
import GHC.Ptr (alignPtr, plusPtr)
import Serialize.Internal.Types
import Data.Int

pokePrim :: forall a. Prim a => a -> Poke ()
#ifdef ALIGNED_MEMORY
pokePrim x = join $
  Poke $ \ps# s# -> do
    let addr# = pokeStateAddr# ps#
        ptr = Ptr addr#
        alignAddr# = pokeStateAlignAddr# ps#
        bufStart = Ptr alignAddr#
        alignStart = alignPtr bufStart align
        sz# = sizeOf# (undefined :: a)
        sz = I# sz#
        align = Primitive.alignment x
        ps'# = setPokeStateAddr# (addr# `plusAddr#` sizeOf# (undefined :: a)) ps#
    (#
      s#,
      ps'#,
      do
        if ptr == alignPtr ptr (Primitive.alignment x)
          then -- If we luck out and the output is already aligned, just poke it directly.
            unsafeLiftPoke'# (Primitive.writeOffAddr# (pokeStateAddr# ps#) 0# x)
          else
            unsafeLiftIOPoke $
              if (alignStart `plusPtr` sz) < (bufStart `plusPtr` alignBufferSize)
                then do
                  Primitive.Ptr.writeOffPtr alignStart 0 x
                  Primitive.Ptr.copyPtr ptr alignStart 1
                else do
                  Foreign.allocaBytesAligned sz align $ \tempPtr -> do
                    Primitive.Ptr.writeOffPtr tempPtr 0 x
                    Primitive.Ptr.copyPtr ptr tempPtr 1
      #)

#else
pokePrim x = Poke $ \ps# s# -> do
  let addr# = pokeStateAddr# ps#
      sz# = sizeOf# (undefined :: a)
      ps'# = setPokeStateAddr# (addr# `plusAddr#` sz#) ps#
  case (Primitive.writeOffAddr# (pokeStateAddr# ps#) 0# x s#) of
    s'#  -> (# s'#, ps'#, () #)
#endif
{-# INLINE pokePrim #-}

peekPrim :: forall a. Prim a => Peek a
#ifdef ALIGNED_MEMORY
peekPrim = do
  ensurePrim (proxy# @a)
  join $
    Peek $ \_end# ps# s# -> do
      let addr# = peekStateAddr# ps#
          ptr = Ptr addr#
          alignAddr# = peekStateAlignAddr# ps#
          bufStart = Ptr alignAddr#
          alignStart = alignPtr bufStart align
          sz# = sizeOf# (undefined :: a)
          sz = I# sz#
          align = Primitive.alignment (undefined :: a)
          ps'# = setPeekStateAddr# (addr# `plusAddr#` sizeOf# (undefined :: a)) ps#
      (#
        s#,
        ps'#,
        unsafeLiftIOPeek $ do
          if ptr == alignStart
            then Ptr.readOffPtr ptr 0
            else
              if (alignStart `plusPtr` sz) < (bufStart `plusPtr` alignBufferSize)
                then do
                  Ptr.copyPtr alignStart ptr sz
                  Ptr.readOffPtr alignStart 0
                else do
                  Foreign.allocaBytesAligned sz align $ \tempPtr -> do
                    Ptr.copyPtr tempPtr ptr sz
                    Ptr.readOffPtr tempPtr 0
        #)
#else
peekPrim = do
  ensurePrim (proxy# @a)
  join $
    Peek $ \_end# ps# s# -> do
      let addr# = peekStateAddr# ps#
          sz# = sizeOf# (undefined :: a)
          ps'# = setPeekStateAddr# (addr# `plusAddr#` sz#) ps#
      (#
        s#,
        ps'#,
        do
          unsafeLiftPeek# (Primitive.readOffAddr# addr# 0#)
        #)

#endif

poke16LE :: Word16 -> Poke ()
poke32LE :: Word32 -> Poke ()
poke64LE :: Word64 -> Poke ()
{-# INLINE poke16LE #-}

{-# INLINE poke32LE #-}

{-# INLINE poke64LE #-}

poke16BE :: Word16 -> Poke ()
poke32BE :: Word32 -> Poke ()
poke64BE :: Word64 -> Poke ()
{-# INLINE poke16BE #-}

{-# INLINE poke32BE #-}

{-# INLINE poke64BE #-}

peek16LE :: Peek Word16
peek32LE :: Peek Word32
peek64LE :: Peek Word64
{-# INLINE peek16LE #-}

{-# INLINE peek32LE #-}

{-# INLINE peek64LE #-}

peek16BE :: Peek Word16
peek32BE :: Peek Word32
peek64BE :: Peek Word64
{-# INLINE peek16BE #-}

{-# INLINE peek32BE #-}

{-# INLINE peek64BE #-}

#ifdef ALIGNED_MEMORY
pokeByte :: Integral a => a -> Poke ()
pokeByte x = Poke $ \ps# s# -> do
  let addr# = pokeStateAddr# ps#
      ps'# = setPokeStateAddr# (addr# `plusAddr#` 1#) ps#
  case Primitive.writeOffAddr# addr# 0# (fromIntegral @_ @Word8 x) s# of
    s'# -> (# s'#, ps'#, () #)
{-# INLINE pokeByte #-}

peekByte :: Integral a => Peek a
peekByte = Peek $ \_end# ps# s# -> do
  let addr# = peekStateAddr# ps#
      ps'# = setPeekStateAddr# (addr# `plusAddr#` 1#) ps#
  case Primitive.readOffAddr# addr# 0# s# of
    (# s'#, x #) -> (# s'#, ps'#, fromIntegral @Word8 x #)
{-# INLINE peekByte #-}

poke16LE y = do
  pokeByte y
  pokeByte $ y `unsafeShiftR` 8

poke16BE y = do
  pokeByte $ y `unsafeShiftR` 8
  pokeByte y

poke32LE y = do
  pokeByte $ y
  pokeByte $ y `unsafeShiftR` 8
  pokeByte $ y `unsafeShiftR` 16
  pokeByte $ y `unsafeShiftR` 24

poke32BE y = do
  pokeByte $ y `unsafeShiftR` 24
  pokeByte $ y `unsafeShiftR` 16
  pokeByte $ y `unsafeShiftR` 8
  pokeByte $ y

poke64LE y = do
  pokeByte y
  pokeByte $ y `unsafeShiftR` 8
  pokeByte $ y `unsafeShiftR` 16
  pokeByte $ y `unsafeShiftR` 24
  pokeByte $ y `unsafeShiftR` 32
  pokeByte $ y `unsafeShiftR` 40
  pokeByte $ y `unsafeShiftR` 48
  pokeByte $ y `unsafeShiftR` 56

poke64BE y = do
  pokeByte $ y `unsafeShiftR` 56
  pokeByte $ y `unsafeShiftR` 48
  pokeByte $ y `unsafeShiftR` 40
  pokeByte $ y `unsafeShiftR` 32
  pokeByte $ y `unsafeShiftR` 24
  pokeByte $ y `unsafeShiftR` 16
  pokeByte $ y `unsafeShiftR` 8
  pokeByte $ y

peek16LE = do
  ensurePrim (proxy# @Word16)
  !x0 <- peekByte @Word16
  !x1 <- peekByte @Word16
  pure $
    x1 `unsafeShiftL` 8
      .|. x0

peek16BE = do
  ensurePrim (proxy# @Word16)
  !x0 <- peekByte @Word16
  !x1 <- peekByte @Word16
  pure $
    x0 `unsafeShiftL` 8
      .|. x1

peek32LE = do
  ensurePrim (proxy# @Word32)
  !x0 <- peekByte @Word32
  !x1 <- peekByte @Word32
  !x2 <- peekByte @Word32
  !x3 <- peekByte @Word32
  pure $
    x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0

peek32BE = do
  ensurePrim (proxy# @Word32)
  !x0 <- peekByte @Word32
  !x1 <- peekByte @Word32
  !x2 <- peekByte @Word32
  !x3 <- peekByte @Word32
  pure $
    x0 `unsafeShiftL` 24
      .|. x1 `unsafeShiftL` 16
      .|. x2 `unsafeShiftL` 8
      .|. x3

peek64LE = do
  ensurePrim (proxy# @Word64)
  !x0 <- peekByte @Word64
  !x1 <- peekByte @Word64
  !x2 <- peekByte @Word64
  !x3 <- peekByte @Word64
  !x4 <- peekByte @Word64
  !x5 <- peekByte @Word64
  !x6 <- peekByte @Word64
  !x7 <- peekByte @Word64
  pure $
    x7 `unsafeShiftL` 56
      .|. x6 `unsafeShiftL` 48
      .|. x5 `unsafeShiftL` 40
      .|. x4 `unsafeShiftL` 32
      .|. x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0

peek64BE = do
  ensurePrim (proxy# @Word64)
  !x0 <- peekByte @Word64
  !x1 <- peekByte @Word64
  !x2 <- peekByte @Word64
  !x3 <- peekByte @Word64
  !x4 <- peekByte @Word64
  !x5 <- peekByte @Word64
  !x6 <- peekByte @Word64
  !x7 <- peekByte @Word64
  pure $
    x0 `unsafeShiftL` 56
      .|. x1 `unsafeShiftL` 48
      .|. x2 `unsafeShiftL` 40
      .|. x3 `unsafeShiftL` 32
      .|. x4 `unsafeShiftL` 24
      .|. x5 `unsafeShiftL` 16
      .|. x6 `unsafeShiftL` 8
      .|. x7

#else
poke16LE p = poke (castPtr @_ @Word16 p) . toLE16
poke32LE p = poke (castPtr @_ @Word32 p) . toLE32
poke64LE p = poke (castPtr @_ @Word64 p) . toLE64

poke16BE p = poke (castPtr @_ @Word16 p) . toBE16
poke32BE p = poke (castPtr @_ @Word32 p) . toBE32
poke64BE p = poke (castPtr @_ @Word64 p) . toBE64

peek16LE p = fromLE16 <$!> peek (castPtr @_ @Word16 p)
peek32LE p = fromLE32 <$!> peek (castPtr @_ @Word32 p)
peek64LE p = fromLE64 <$!> peek (castPtr @_ @Word64 p)

peek16BE p = fromBE16 <$!> peek (castPtr @_ @Word16 p)
peek32BE p = fromBE32 <$!> peek (castPtr @_ @Word32 p)
peek64BE p = fromBE64 <$!> peek (castPtr @_ @Word64 p)
#endif

poke8 :: Word8 -> Poke ()
poke8 = pokeByte
{-# INLINE poke8 #-}

peek8 :: Peek Word8
peek8 = peekByte
{-# INLINE peek8 #-}

pokeInt32 :: Int32 -> Poke ()
pokeInt32 = poke32LE . fromIntegral
