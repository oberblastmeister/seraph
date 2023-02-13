{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Store qualified as S
import Data.Typeable (Typeable)
import Dataset
import Flat qualified as F
import GHC.Generics (Generic)
import Criterion.Main
import Serialize
import Test.QuickCheck

data BinTree a
  = Tree
      (BinTree a)
      (BinTree a)
  | Leaf a
  deriving (Show, Read, Eq, Typeable, Generic, NFData)

data Direction
  = North
  | South
  | Center
  | East
  | West
  deriving (Show, Read, Eq, Typeable, Generic, NFData)

instance Arbitrary Direction where
  arbitrary = elements [North, South, Center, East, West]

-- General instances
instance F.Flat a => F.Flat (BinTree a)

instance F.Flat Direction

instance S.Store a => S.Store (BinTree a)

instance S.Store Direction

instance Serialize a => Serialize (BinTree a)

instance Serialize Direction

main :: IO ()
main = do
  tree <- (generateBalancedTree 21 :: IO (BinTree Direction))
  let directionTree = ("BinTree Direction", tree)
  !carsDataset <- ("Cars" :: String, ) <$> carsData
  defaultMain (benchs carsDataset ++ benchs ("Iris Data", irisData) ++ benchs directionTree)

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
          map (\(pkg, s, _) -> env (pure obj) \obj -> bench (nm pkg) (nf BS.length (s obj))) pkgs,
        -- NOTE: the benchmark time includes the comparison of the deserialised obj with the original
        bgroup "deserialization (time)" $
          map
            ( \(pkg, s, d) ->
                env (pure $ s obj) (\bs -> bench (nm pkg) $ whnf (obj ==) (d bs))
            )
            pkgs
      ]

pkgs :: (C a) => [(String, a -> ByteString, ByteString -> a)]
pkgs =
  [ 
    ("serialize", encode, decode'),
    ("flat", F.flat, fromRight' . F.unflat),
    ("store", S.encode, fromRight' . S.decode)
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
