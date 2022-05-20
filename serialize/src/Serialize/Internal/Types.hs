{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Serialize.Internal.Types
  ( Poke (..),
    Peek (..),
    PokeException (..),
    PeekException (..),
    pokeStateAddr#,
    setPokeStateAddr#,
#ifdef ALIGNED_MEMORY
    pokeStateAlignAddr#,
    alignBufferSize,
    peekStateAlignAddr#,
#endif
    unsafeWithPokeState#,
    pokeStatePtr#,
    putPokeState#,
    unsafeLiftPoke#,
    unsafeLiftIOPoke,
    unsafeLiftPoke'#,
    peekStateAddr#,
    setPeekStateAddr#,
    unsafeLiftIOPeek,
    unsafeLiftPeek# ,
    unsafeLiftPeek'#,
    throwTooManyBytes,
    getPeekState#,
    setPeekState#,
    getEnd#,
    getEnd,
    ensurePeek,
    ensurePrim
  )
where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as ByteString.Internal
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Exts
import GHC.IO (IO (..))
import Control.Monad (join, when)
import qualified Foreign
import Data.Primitive (Prim)
import qualified Data.Primitive as Primitive

type S# = State# RealWorld

newtype Poke a where
  Poke ::
    { runPoke# ::
        PokeState# ->
        State# RealWorld ->
        (# State# RealWorld, PokeState#, a #)
    } ->
    Poke a

#ifdef ALIGNED_MEMORY
newtype PokeState# = PokeState# (# Addr#, Addr# #)

pokeStateAddr# :: PokeState# -> Addr#
pokeStateAddr# (PokeState# (# addr#, _ #)) = addr#
{-# INLINE pokeStateAddr# #-}

setPokeStateAddr# :: Addr# -> PokeState# -> PokeState#
setPokeStateAddr# addr# (PokeState# (# _, alignAddr# #)) = PokeState# (# addr#, alignAddr# #)
{-# INLINE setPokeStateAddr# #-}

pokeStateAlignAddr# :: PokeState# -> Addr#
pokeStateAlignAddr# (PokeState# (# _, alignAddr# #)) = alignAddr#
{-# INLINE pokeStateAlignAddr# #-}

unsafeWithPokeState# :: Int -> (PokeState# -> IO ()) -> ByteString
unsafeWithPokeState# l f = ByteString.Internal.unsafeCreate l $ \(Ptr addr#) ->
  Foreign.allocaBytesAligned alignBufferSize 8 $ \(Ptr alignAddr#) ->
    f (PokeState# (# addr#, alignAddr# #))
{-# INLINE unsafeWithPokeState# #-}

alignBufferSize :: Int
alignBufferSize = 32
#else
newtype PokeState# = PokeState# (# Addr# #)

pokeStateAddr# :: PokeState# -> Addr#
pokeStateAddr# (PokeState# (# addr# #)) = addr#
{-# INLINE pokeStateAddr# #-}

setPokeStateAddr# :: Addr# -> PokeState# -> PokeState#
setPokeStateAddr# addr# _ = PokeState# (# addr# #)
{-# INLINE setPokeStateAddr# #-}

unsafeWithPokeState# :: Int -> (PokeState# -> IO ()) -> ByteString
unsafeWithPokeState# l f = ByteString.Internal.unsafeCreate l $ \(Ptr addr#) ->
  f (PokeState# (# addr# #))
{-# INLINE unsafeWithPokeState# #-}
#endif

pokeStatePtr# :: PokeState# -> Ptr Word8
pokeStatePtr# ps# = Ptr (pokeStateAddr# ps#)
{-# INLINE pokeStatePtr# #-}

putPokeState# :: PokeState# -> Poke ()
putPokeState# ps# = Poke $ \_ps# s# -> (# s#, ps#, () #)
{-# INLINE putPokeState# #-}

instance Functor Poke where
  fmap f m = Poke $ \ps# s# ->
    case runPoke# m ps# s# of
      (# s'#, ps'#, x #) -> (# s'#, ps'#, f x #)
  {-# INLINE fmap #-}

instance Applicative Poke where
  pure x = Poke $ \ps s# -> (# s#, ps, x #)
  {-# INLINE pure #-}

  Poke mf <*> Poke mx = Poke $ \ps# s# -> case mf ps# s# of
    (# s'#, ps'#, f #) -> case mx ps'# s'# of
      (# s''#, ps''#, x #) -> (# s''#, ps''#, f x #)
  {-# INLINE (<*>) #-}

instance Monad Poke where
  return = pure
  {-# INLINE return #-}

  Poke mx >>= fm = Poke $ \ps# s# -> case mx ps# s# of
    (# s'#, ps'#, x #) -> runPoke# (fm x) ps'# s'#
  {-# INLINE (>>=) #-}

unsafeLiftIOPoke :: IO a -> Poke a
unsafeLiftIOPoke (IO f) = unsafeLiftPoke# f
{-# INLINE unsafeLiftIOPoke #-}

unsafeLiftPoke# :: (S# -> (# S#, a #)) -> Poke a
unsafeLiftPoke# f = Poke $ \ps# s# -> case f s# of
  (# s'#, x #) -> (# s'#, ps#, x #)
{-# INLINE unsafeLiftPoke# #-}

unsafeLiftPoke'# :: (S# -> S#) -> Poke ()
unsafeLiftPoke'# f = Poke $ \ps# s# -> case f s# of
   s'# -> (# s'#, ps#, () #)
{-# INLINE unsafeLiftPoke'# #-}

#ifdef ALIGNED_MEMORY
newtype PeekState# = PeekState# (# Addr#, Addr# #)

peekStateAddr# :: PeekState# -> Addr#
peekStateAddr# (PeekState# (# addr#, _ #)) = addr#
{-# INLINE peekStateAddr# #-}

peekStateAlignAddr# :: PeekState# -> Addr#
peekStateAlignAddr# (PeekState# (# _, alignAddr# #)) = alignAddr#
{-# INLINE peekStateAlignAddr# #-}

setPeekStateAddr# :: Addr# -> PeekState# -> PeekState#
setPeekStateAddr# addr# (PeekState# (# _ , alignAddr# #)) = PeekState# (# addr#, alignAddr# #)
{-# INLINE setPeekStateAddr# #-}
#else
newtype PeekState# = PeekState# (# Addr# #)

peekStateAddr# :: PeekState# -> Addr#
peekStateAddr# (PeekState# (# addr# #)) = addr#
{-# INLINE peekStateAddr# #-}

setPeekStateAddr# :: Addr# -> PeekState# -> PeekState#
setPeekStateAddr# addr# (PeekState# (# _ #)) = PeekState# (# addr# #)
{-# INLINE setPeekStateAddr# #-}
#endif

newtype Peek a = Peek
  { runPeek# ::
      Addr# ->
      PeekState# ->
      State# RealWorld ->
      (# State# RealWorld, PeekState#, a #)
  }

instance Functor Peek where
  fmap f m = Peek $ \end# ps# s# -> case runPeek# m end# ps# s# of
    (# s'#, ps'#, x #) -> (# s'#, ps'#, f x #)
  {-# INLINE fmap #-}

instance Applicative Peek where
  pure x = Peek $ \_end# ps# s# -> (# s#, ps#, x #)
  {-# INLINE pure #-}

  Peek mf <*> Peek mx = Peek $ \end# ps# s# -> case mf end# ps# s# of
    (# s'#, ps'#, f #) -> case mx end# ps'# s'# of
      (# s''#, ps''#, x #) -> (# s''#, ps''#, f x #)
  {-# INLINE (<*>) #-}

instance Monad Peek where
  return = pure
  {-# INLINE return #-}

  Peek mx >>= fm = Peek $ \end# ps# s# -> case mx end# ps# s# of
    (# s'#, ps'#, x #) -> runPeek# (fm x) end# ps'# s'#
  {-# INLINE (>>=) #-}

unsafeLiftIOPeek :: IO a -> Peek a
unsafeLiftIOPeek (IO f) = unsafeLiftPeek# f
{-# INLINE unsafeLiftIOPeek #-}

unsafeLiftPeek# :: (S# -> (# S#, a #)) -> Peek a
unsafeLiftPeek# f = Peek $ \_end# ps# s# -> case f s# of
  (# s'#, x #) -> (# s'#, ps#, x #)
{-# INLINE unsafeLiftPeek# #-}

unsafeLiftPeek'# :: (S# -> S#) -> Peek ()
unsafeLiftPeek'# f = unsafeLiftPeek# (\s# -> case f s# of
  s'# -> (# s'#, () #))
{-# INLINE unsafeLiftPeek'# #-}

getPeekState# :: (PeekState# -> Peek a) -> Peek a
getPeekState# f = join $ Peek $ \_end# ps# s# -> (# s#, ps#, f ps# #)
{-# INLINE getPeekState# #-}

setPeekState# :: PeekState# -> Peek ()
setPeekState# ps# = Peek $ \_end# _ps# s# -> (# s#, ps#, () #)
{-# INLINE setPeekState# #-}

getEnd# :: (Addr# -> Peek a) -> Peek a
getEnd# f = join $ Peek $ \end# ps# s# -> (# s#, ps#, f end# #)
{-# INLINE getEnd# #-}

getEnd :: Peek (Ptr Word8)
getEnd = Peek $ \end# ps# s# -> (# s#, ps#, Ptr end# #)
{-# INLINE getEnd #-}

ensurePeek :: Int -> Peek ()
ensurePeek sz = do
  end <- getEnd
  getPeekState# $ \ps# -> do
    let ptr = Ptr (peekStateAddr# ps#)
        remaining = end `Foreign.minusPtr` ptr
    unsafeLiftIOPeek $ when (sz > remaining) $ throwTooManyBytes sz remaining "some type" 
{-# INLINE ensurePeek #-}

ensurePrim :: forall a. Prim a => Proxy# a -> Peek ()
ensurePrim _  = ensurePeek (Primitive.sizeOf (undefined :: a))
{-# INLINE ensurePrim #-}

data PokeException = PokeException
  { pokeExByteIndex :: !Int,
    pokeExMessage :: String
  }
  deriving (Eq, Show, Typeable)

instance Exception PokeException where
  displayException (PokeException offset msg) =
    "Exception while poking, at byte index "
      ++ show offset
      ++ " : "
      ++ msg

-- | Exception thrown while running 'peek'. Note that other types of
-- exceptions can also be thrown. Invocations of 'fail' in the 'Poke'
-- monad causes this exception to be thrown.
--
-- 'PeekException' is thrown when the data being decoded is invalid.
data PeekException = PeekException
    { peekExBytesFromEnd :: !Int
    , peekExMessage :: String
    } deriving (Eq, Show, Typeable)

instance Exception PeekException where
    displayException (PeekException offset msg) =
        "Exception while peeking, " ++
        show offset ++
        " bytes from end: " ++
        msg

-- | Throws a 'PeekException' about an attempt to read too many bytes.
throwTooManyBytes :: Int -> Int -> String -> IO void
throwTooManyBytes needed remaining ty =
  Exception.throwIO $
    PeekException remaining $
      "Attempted to read too many bytes for "
        ++ ty
        ++ ". Needed "
        ++ show needed
        ++ ", but only "
        ++ show remaining
        ++ " remain."
