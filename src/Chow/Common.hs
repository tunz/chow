module Chow.Common where

import           Chow.Token
import           System.Exit
import           System.IO
import           Text.Parsec

data Options = Options
  { checkerType :: CheckerType
  , lang        :: Lang
  , paths       :: [String]
  }

data Lang
  = LangC
  | LangCpp
  | LangUnknown
  deriving (Eq)

instance Show Lang where
  show LangC       = "C"
  show LangCpp     = "C++"
  show LangUnknown = "Unknown"

data CheckerType
  = TypeNullDeref
  | TypeNone
  deriving (Eq)

instance Show CheckerType where
  show TypeNullDeref = "Null-ptr-deref"
  show TypeNone      = "Default"

type Report = (SourcePos, String)

type CheckFn a = ([TokenPos] -> [a])

failAndExit :: Show a => a -> IO b
failAndExit s = do
  print s
  exitWith (ExitFailure 1)

iterateCheck :: CheckFn a -> [TokenPos] -> IO [a]
iterateCheck fn tokens = iterateCheck' fn tokens []
  where
    iterateCheck' _ [] reports = return $ reverse reports
    iterateCheck' check (x:xs) reports =
      iterateCheck' check xs $! (check (x : xs) ++ reports)

readSourceCode :: FilePath -> IO String
readSourceCode path = do
  handle <- openBinaryFile path ReadMode
  hSetBinaryMode handle True
  hGetContents handle
