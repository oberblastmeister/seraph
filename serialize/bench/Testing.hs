{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo
-dverbose-core2core
#-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Testing where

import Serialize
import GHC.Exts (oneShot, inline)
import Serialize.Internal.Put
import Prelude hiding (mapM_)


weird :: forall a. Serialize a => [Int] -> Get a
weird = foldr (\x z -> get @a >> z) get

weird2 :: forall a. Serialize a => [Int] -> Get ()
weird2 = foldr (\x z -> get @a >> z) (pure ())

huh :: forall a. Serialize a => [a] -> Put
huh = foldr (\x z -> 
  Put# \marr i -> runPut# (put x) marr i >>= \i -> runPut# z marr i
  ) mempty
  
huh2 :: Monoid m => (a -> m) -> [a] -> m
huh2 f = foldr c mempty
  where
    c x z = f x <> z
    {-# INLINE c #-}

bruh :: forall a. Serialize a => [a] -> Put
bruh = huh2 put

-- bruh_bruh :: forall a. Serialize a => [a] -> IO r

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ f = foldr c (return ())
  -- See Note [List fusion and continuations in 'c']
  where c x k = f x >> k
        {-# INLINE c #-}

no :: forall a. Show a => a -> IO ()
no = (print)
{-# NOINLINE no #-}

bruh2 :: forall a. Show a => [a] -> IO ()
bruh2 = mapM_ no

multiple :: forall a b c d. (Serialize a, Serialize b, Serialize c, Serialize d) => a -> b -> c -> d -> Put
multiple a b c d = put a <> put b <> put c <> put d

class Opaque a where
  opaque :: a -> IO ()
  
multipleIO :: (Opaque a, Opaque b, Opaque c, Opaque d) => a -> b -> c -> d -> IO ()
multipleIO a b c d = opaque a >> opaque b >> opaque c >> opaque d

newtype M1 = M1 {runM1 :: Int -> IO Int}

instance Semigroup M1 where
  M1 f1 <> M1 f2 = M1 \i -> f1 i >>= f2
instance Monoid M1 where
  mempty = M1 pure

newtype M2 a = M2 {runM2 :: Int -> IO (Int, a)}

instance Functor M2 where
  fmap f (M2 g) = M2 \i -> do
    (i, x) <- g i
    pure (i, f x)
    
instance Applicative M2 where
  pure x = M2 \i -> pure (i, x)

  M2 f <*> M2 g = M2 \i -> do
    (i, f) <- f i
    (i, x) <- g i
    pure (i, f x)
    
instance Monad M2 where
  (M2 f) >>= g = M2 \i -> do
    (i, x) <- f i
    runM2 (g x) i

class C1 a where
  c1 :: a -> M1

class C2 a where
  c2 :: a -> M2 ()
  
f1 :: (C1 a, C1 b, C1 c) => a -> b -> c -> M1
f1 x y z = c1 x <> c1 y <> c1 z

f2 :: (C2 a, C2 b, C2 c) => a -> b -> c -> M2 ()
f2 x y z = c2 x >> c2 y >> c2 z

f3 :: forall a. C1 a => [a] -> M1
f3 = foldMap c1

f4 :: forall a. C2 a => [a] -> M2 ()
f4 = mapM_ c2
