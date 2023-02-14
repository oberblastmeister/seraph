{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Dataset where

import Control.DeepSeq
import Data.Store qualified as S
import Flat qualified as F
import Numeric.Datasets.Car
import Serialize
import Numeric.Datasets
import Numeric.Datasets.Iris
import Network.HTTP.Req
import qualified Flat.Decoder as F

{- ORMOLU_DISABLE -}
instance NFData Count
instance F.Flat Count
instance S.Store Count
instance Serialize Count

instance NFData RelScore
instance F.Flat RelScore
instance S.Store RelScore
instance Serialize RelScore

instance NFData RelSize
instance F.Flat RelSize
instance S.Store RelSize
instance Serialize RelSize

instance NFData Acceptability
instance F.Flat Acceptability
instance S.Store Acceptability
instance Serialize Acceptability

deriving instance Eq Car
instance NFData Car
instance F.Flat Car
instance S.Store Car
instance Serialize Car

instance NFData IrisClass
instance F.Flat IrisClass
instance S.Store IrisClass
instance Serialize IrisClass

deriving instance Eq Iris
instance NFData Iris
instance F.Flat Iris
instance S.Store Iris
instance Serialize Iris
{- ORMOLU_ENABLE -}

serializeSize :: Car -> Int
serializeSize = size

flatDecode :: F.Get Car
flatDecode = F.decode

serializeDecode :: Get Car
serializeDecode = get

storeDecode :: S.Peek Car
storeDecode = S.peek

serializeDecode2 :: Get Iris
serializeDecode2 = get

storeDecode2 :: S.Peek Iris
storeDecode2 = S.peek

by :: Int -> [a] -> [a]
by n = concat . replicate n

carsData :: IO [Car]
carsData = by 1000 <$> getDataset car {source = URL $ uciMLDB /: "car" /: "car.data"}

irisData :: [Iris]
irisData = by 2000 iris
