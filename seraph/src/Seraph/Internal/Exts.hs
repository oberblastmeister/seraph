module Seraph.Internal.Exts (module X) where

import Data.Kind as X (Constraint, Type)
import GHC.Exts as X
  ( Int (..),
    Int#,
    Proxy#,
    RealWorld,
    State#,
    Word (..),
    Word#,
    coerce,
    proxy#,
    (*#),
    (+#),
    (<#),
    (<=#),
    (>#),
    (>=#),
  )
