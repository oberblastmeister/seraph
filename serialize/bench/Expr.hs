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

data UnaryOp
  = Neg
  | Not
  deriving (Show, Eq, Generic, NFData)

data Lit
  = String !Text
  | Bool !Bool
  | Int !Int
  | Null
  deriving (Show, Eq, Generic, NFData)

data Expr
  = Bin Expr !BinOp Expr
  | Unary !UnaryOp Expr
  | App Expr [Expr]
  | Let [(Text, Expr)] Expr
  | Lam [Text] Expr
  | Lit !Lit
  deriving (Show, Eq, Typeable, Generic, NFData)

instance Serialize BinOp

instance Serialize Expr

instance Serialize UnaryOp

instance Serialize Lit

instance S.Store BinOp

instance S.Store Expr

instance S.Store UnaryOp

instance S.Store Lit

serializeEncode :: Expr -> Put
serializeEncode = put

storeEncode :: Expr -> S.Poke ()
storeEncode = S.poke

serializeDecode :: Get Expr
serializeDecode = get

storeDecode :: S.Peek Expr
storeDecode = S.peek

serializeSize :: Expr -> Int
serializeSize = size

storeSize :: S.Size Expr
storeSize = S.size
