{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Serialize where

import GHC.Exts

newtype Put a = Put {runPut# :: Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, a #)}

newtype Get a = Get {runGet# :: Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, a #)}

type GetResult# a = (# (# #)| (# Addr#, a #) #)

class Serialize a where
  serialize :: a -> Put ()
  deserialize :: Get a
