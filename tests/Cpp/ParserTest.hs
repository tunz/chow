{-# LANGUAGE LambdaCase #-}

module Cpp.ParserTest
  ( cppParserTests
  ) where

import           Chow.Cpp.Ast     as A
import           Chow.Cpp.Parser  as P
import           Chow.Token
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec

tokenize s =
  case parse pTokens "" s of
    Left err -> []
    Right xs -> xs

parseAndAssert name parser stream isAns =
  case parse parser "" (tokenize stream) of
    Right result
      | isAns result -> return ()
    Right result -> assertFailure $ name ++ " fail\ngot: " ++ show result
    Left err -> assertFailure $ name ++ " fail\ngot err: " ++ show err

cppParserTests =
  testGroup
    "Parser tests for C++"
    [ testCase "if if else else if else" $
      parseAndAssert
        "blockParser"
        blockParser
        ("if (ptr) " ++
         "if (ptr2) return; else return;" ++
         "else if (!ptr) return; else return;")
        (\case
           (CompoundStmt [IfStmt _ _ (CompoundStmt [IfStmt _ _ _ (Just _)]) (Just (IfStmt _ _ _ (Just _)))]) ->
             True
           _ -> False)
    , testCase "Some::Function () { }" $
      parseAndAssert
        "functionParser"
        functionParser
        "A::ReturnVal * B::Func(C::As<Type> *arg) { return; }"
        (\case
           Function _ "Func" _ _ -> True
           _ -> False)
    , testCase "for (auto &x: xs) { }" $
      parseAndAssert
        "forRangeParser"
        forRangeParser
        "for (auto &x: xs) { printf(\"test\"); }"
        (\case
           ForRangeStmt _ _ (WildCard [(Ide "xs", _)]) CompoundStmt {} -> True
           _ -> False)
    , testCase "fn(fn2(a1, a2), a3)" $
      parseAndAssert
        "exprParser"
        exprParser
        "fn(fn2(a1, a2), a3)"
        (\case
           Call _ [Call _ _, _] -> True
           _ -> False)
    , typeParserTests
    ]

typeParserTests =
  testGroup
    "Type parser tests for C++"
    [ testCase "int" $
      parseAndAssert "typeDeclParser" typeDeclParser "int" (== IntType)
    , testCase "const ref namespace type" $
      parseAndAssert
        "typeDeclParser"
        typeDeclParser
        "const cc::SharedBitmapId& id"
        (== RefType (ConstType (ClassType "cc::SharedBitmapId")))
    , testCase "std::map<int, ReadbackRequestCallback>::iterator it" $
      parseAndAssert
        "declParser"
        declParser
        "std::map<int, ReadbackRequestCallback>::iterator it"
        (\case
           VarDecl (ClassType _) (Var _ "it") -> True
           _ -> False)
    , testCase "auto" $
      parseAndAssert "typeDeclParser" typeDeclParser "auto" (== AutoType)
    , testCase "auto &x" $
      parseAndAssert
        "declParser"
        declParser
        "auto &x"
        (\case
           VarDecl (RefType AutoType) (Var _ "x") -> True
           _ -> False)
    ]
