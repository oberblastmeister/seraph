{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cars where
import GHC.Generics (Generic)
import Generic.Random
import Test.QuickCheck
import Control.DeepSeq
import Seraph
import qualified Data.Store as S
import qualified Flat as F
import Test.QuickCheck.Instances ()

data RelScore = Low | Med | High | VeryHigh
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving Arbitrary via GenericArbitraryU RelScore

data RelSize = Small | Medium | Big
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving Arbitrary via GenericArbitraryU RelSize

data Acceptability = Unacceptable | Acceptable | Good | VeryGood
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving Arbitrary via GenericArbitraryU Acceptability


data Count = N Int | NOrMore Int | More
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving Arbitrary via GenericArbitraryU Count

data Car = Car
  { buying :: RelScore
  , maintenance :: RelScore
  , doors :: Count
  , persons :: Count
  , luggageBoot :: RelSize
  , safety :: RelScore
  , acceptability :: Acceptability
  }
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving Arbitrary via GenericArbitrarySingle Car

seraphEncode :: Car -> Put
seraphEncode = put

seraphDecode :: Get Car
seraphDecode = get

storeDecode :: S.Peek Car
storeDecode = S.peek

storeEncode :: Car -> S.Poke ()
storeEncode = S.poke
