{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module Dataset where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Store qualified as S
import Flat qualified as F
import Flat.Decoder qualified as F
import Network.HTTP.Req
import Numeric.Datasets
import Numeric.Datasets.Car
import Numeric.Datasets.Iris
import Serialize

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

serializeEncode :: [Car] -> ByteString
serializeEncode = encode

storeEncode :: [Car] -> ByteString
storeEncode = S.encode

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

-- -- | The UCI machine learning database
-- uciMLDB :: Url 'Https
-- uciMLDB = https "archive.ics.uci.edu" /: "ml" /: "machine-learning-databases"

-- url :: String
-- url = "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"

-- carsData :: IO [Car]
-- carsData = by 1000 <$> getDataset car {source = URL url}

carsData :: IO [Car]
carsData = by 1000 <$> getDataset car {source = URL $ uciMLDB /: "car" /: "car.data"}

irisData :: [Iris]
irisData = by 2000 iris
