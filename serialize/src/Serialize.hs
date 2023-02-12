module Serialize
  ( Serialize (..),
    Get,
    Put,
    putFoldable,
    sizeFoldable,
    foldGet,
    gConstSize,
    encode,
    encodeIO,
    decodeIO,
    decode',
    decode,
  )
where

import Serialize.Internal
import Serialize.Internal.Get
import Serialize.Internal.Put
