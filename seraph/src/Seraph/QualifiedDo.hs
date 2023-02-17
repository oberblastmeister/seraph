module Seraph.QualifiedDo ((>>)) where

import Seraph.Internal.Put (Put)
import Prelude hiding ((>>))

(>>) :: Put -> Put -> Put
(>>) = (<>)
{-# INLINE (>>) #-}
