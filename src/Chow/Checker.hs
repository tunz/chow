module Chow.Checker (runChecker) where

import Text.ParserCombinators.Parsec

import Chow.Common
import Chow.Token
import qualified Chow.C.Checker as C

selectChecker :: Lang
              -> CheckerType
              -> CheckerType -> [String] -> IO [Report]
selectChecker lang ckType =
  case lang of
    LangC -> C.check
    LangUnknown -> notAvailable
  where
    notAvailable _ _ = failAndExit "unreachable"

runChecker :: Options -> IO [Report]
runChecker (Options checkerType lang paths) =
  selectChecker lang checkerType checkerType paths
