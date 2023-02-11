module Serialize.QualifiedDo ((>>)) where

import Serialize.Internal.Put (Put)
import Prelude hiding ((>>))

(>>) :: Put -> Put -> Put
(>>) = (<>)
{-# INLINE (>>) #-}
