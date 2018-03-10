module Chow.C.Checker where

import Text.Parsec
import Data.Functor.Identity
import Text.ParserCombinators.Parsec

import Chow.Common
import Chow.Token
import Chow.C.Parser
import Chow.C.Ast
import Chow.C.NullDerefChecker

tokenize :: String -> String -> IO [TokenPos]
tokenize fname stream =
  case parse pTokens fname stream of
    Left err -> putStrLn "[-] Tokenization failed" >> print err >> return []
    Right xs -> return xs

check :: CheckerType -> [String] -> IO [Report]
check checkerType paths = check' checkerType paths []
  where
    check' checkerType [] acc = return acc
    check' checkerType (path:paths) acc = do
      result <- checkOne checkerType path
      check' checkerType paths $! result ++ acc

checkOne :: CheckerType -> String -> IO [Report]
checkOne checkerType path =
    readSourceCode path
    >>= tokenize path
    >>= iterateCheck check'
  where check' stream =
          case parse functionParser "" stream of
            Left err -> []
            Right fn -> checker checkerType $ parseWildcards fn

checker :: CheckerType -> S -> [Report]
checker _ = nullDerefChecker

parseWildcards :: S -> S
parseWildcards (Function pos id args body) =
  Function pos id parsedArgs parsedBody
    where
      parsedArgs = map tryDeclParser args
      parsedBody = parseExprsInStmt body
