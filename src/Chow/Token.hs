module Chow.Token where

import           Data.Functor.Identity
import           Text.Parsec

data Token
  = Ide String
  | Symbol String
  | Const Const
  deriving (Show, Eq)

data Const
  = IntNum Integer
  | FloatNum Double
  | Str String
  | Chr Char
  deriving (Show, Eq)

type TokenPos = (Token, SourcePos)

type TkParser = ParsecT [TokenPos] () Identity
