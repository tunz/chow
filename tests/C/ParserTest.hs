module C.ParserTest (cParserTests) where

import Text.Parsec
import Test.Tasty
import Test.Tasty.HUnit
import Chow.Token
import Chow.C.Parser as P
import Chow.C.Ast as A

tokenize s =
  case parse pTokens "" s of
    Left err -> []
    Right xs -> xs

parseBlock :: String -> Stmt
parseBlock stream =
  case parse blockParser "" (tokenize stream) of
    Left err -> CompoundStmt []
    Right fn -> parseExprsInStmt fn

cParserTests = testGroup "Parser tests for C"
  [ blockParserTests ]

blockParserTests = testGroup "blockParser tests for C"
  [ testCase "if if else else if else" $
    case
      parseBlock ("if (ptr) " ++
                  "if (ptr2) return; else return;" ++
                  "else if (!ptr) return; else return;")
    of
      CompoundStmt [IfStmt _ _ (CompoundStmt [IfStmt _ _ _ (Just _)])
                               (Just (IfStmt _ _ _ (Just _)))] -> return ()
      result -> assertFailure $ "blockParser fail 1\ngot: " ++ show result
  ]
