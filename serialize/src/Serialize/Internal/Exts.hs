module Serialize.Internal.Exts (module X) where

import Data.Kind as X (Constraint, Type)
import GHC.Exts as X
  ( Int (..),
    Int#,
    Proxy#,
    State#,
    coerce,
    proxy#,
    (*#),
    (+#),
    (<=#),
    (>=#),
  )
