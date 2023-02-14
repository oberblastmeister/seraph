{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module Expr where

import Control.DeepSeq
import Data.Store qualified as S
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Serialize

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq, Generic, NFData)

data Expr
  = Bin Expr !BinOp Expr
  | App Expr [Expr]
  | Lam !Text Expr
  | Int !Int
  | Bool !Bool
  | String !Text
  | Not !Expr
  deriving (Show, Eq, Typeable, Generic, NFData)

instance Serialize BinOp

instance Serialize Expr

instance S.Store BinOp

instance S.Store Expr

serializeSize :: Expr -> Int
serializeSize = size

storeSize :: S.Size Expr
storeSize = S.size
