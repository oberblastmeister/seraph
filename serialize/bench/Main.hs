{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.DeepSeq
import BinTree
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Store qualified as S
import Data.Typeable (Typeable)
import Dataset
import Flat qualified as F
import Criterion.Main
import Serialize
import Test.QuickCheck

main :: IO ()
main = do
  tree <- (generateBalancedTree 21 :: IO (BinTree Direction))
  let directionTree = ("BinTree Direction", tree)
  !carsDataset <- ("Cars" :: String, ) <$> carsData
  let tests = benchs carsDataset ++ benchs ("Iris Data", irisData) ++ benchs directionTree
  defaultMain tests

type C a =
  ( Eq a,
    Typeable a,
    NFData a,
    F.Flat a,
    Serialize a,
    S.Store a,
    Read a,
    Show a
  )

benchs :: (C a) => (String, a) -> [Benchmark]
benchs (name, obj) =
  let nm pkg = concat [name, "-", pkg]
   in -- env (return obj) $ \sobj -> bgroup ("serialization (mSecs)") $ map (\(pkg,s,_) -> bench (nm pkg) (nfIO (s sobj))) pkgs
      [ bgroup "serialization (time)" $
          map (\(pkg, s, _) -> env (pure obj) \obj -> bench (nm pkg) (nf s obj)) pkgs,
        -- NOTE: the benchmark time includes the comparison of the deserialised obj with the original
        bgroup "deserialization (time)" $
          map
            ( \(pkg, s, d) ->
                env (pure $ s obj) (\bs -> bench (nm pkg) $ nf d bs)
            )
            pkgs
      ]

pkgs :: (C a) => [(String, a -> ByteString, ByteString -> a)]
pkgs =
  [ 
    ("serialize", encode, decode'),
    ("store", S.encode, fromRight' . S.decode),
    ("flat", F.flat, fromRight' . F.unflat)
  ]

fromRight' :: Either a b -> b
fromRight' (Left _) = undefined
fromRight' (Right x) = x

generateBalancedTree :: (Arbitrary a1) => Int -> IO (BinTree a1)
generateBalancedTree = generateBalancedTree_ (generate arbitrary)
  where
    generateBalancedTree_ r 0 = Leaf <$> r
    generateBalancedTree_ r n =
      Tree
        <$> generateBalancedTree_ r (n - 1)
        <*> generateBalancedTree_ r (n - 1)
