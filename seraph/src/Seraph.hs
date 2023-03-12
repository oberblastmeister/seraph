-- | This library contains types and functions for serializing data.
-- Undefined behavior cannot be caused using this library because of bounds checking which
-- should have a negligible performance impact.
-- Indeed, this library outperforms other Haskell serialization libraries such
-- as store and flat despite doing bounds checking.
-- In addition, the library is consistent, in that serialization is a bijective mapping between objects and their binary representations.
-- This is in contrast with some libraries that will produce different output because of differences in endianness.
--
-- = Gotchas
--
-- * seraph does not protect from malicious input.
--
-- * seraph uses 'ByteArray' internally which enables use of ghc primops for unaligned reads.
--   However, this means that the 'ByteString' to be 'decode'd must be managed by the Haskell heap.
--   If it is not, it will be copied onto the Haskell heap.
--   This should not be a problem because most 'ByteString's will be allocated on the Haskell heap.
module Seraph
  ( -- * How to use this library
    -- $use

    -- * Writing instances by hand
    -- $manual

    -- * Main encoding and decoding functions
    encode,
    encodeIO,
    decodeIO,
    decode',
    decode,

    -- * Run and lift functions
    runPutIO,
    runPutST,
    runPutPure,
    runGetIO,
    runGetST,
    runGetPure,
    liftPut,
    liftGet,

    -- * Main class
    Serialize (..),

    -- * Main types
    Get,
    Put,

    -- * Transformer types
    GetT,
    PutT,
    PureMode,
    STMode,
    IOMode,

    -- * Types related to endianness.
    Fixed,
    ByteOrder (..),
    DefaultEndian,
  )
where

import Seraph.Internal
import Seraph.Internal.Get
import Seraph.Internal.Put
import Seraph.Internal.Util
import System.ByteOrder

-- $use
--
-- If we have a data type:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > data Person = Person Text Int
-- >   deriving (Generic)
--
-- The @LANGUAGE@ pragma and 'Generic' instance let us write empty
-- 'Serialize' instances for which the compiler will
-- generate sensible default implementations.
--
-- @
-- instance Serialize Person where
--     \-- No need to provide a 'size', 'get', or 'put' implementation.
-- @
--
-- We can now encode a value:
--
-- > encode (Person "Joe" 12)

-- $manual
--
-- When necessary, we can write 'Serialize' instances by hand.
-- This is useful when we want more predictable performance. Instances generated
-- using 'Generic' are usually slower on recursive types, so it may be good to write
-- those instances by hand.
--
-- Going back to the @Person@ example, we remove the 'Generic' instance.
--
-- > data Person = Person Text Int
--
-- Now we need to define a 'Serialize' instance.
--
-- @
-- instance Serialize Person where
--   size (Person name age) = theSize name + theSize age
--   put (Person name age) = put name <> put age
--   get = do
--     name <- get
--     age <- get
--     pure $ Person name age
-- @
