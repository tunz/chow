module C.NullDerefTest where

import qualified Data.Map as Map (fromList, map, empty)
import qualified Data.List as List (map)
import Text.Parsec
import Text.ParserCombinators.Parsec.Pos
import Test.Tasty
import Test.Tasty.HUnit
import Chow.Common
import Chow.Checker
import Chow.C.NullDerefChecker as NC
import Chow.C.Parser as P
import Chow.C.Ast as A

cNullDerefTests = testGroup "null-deref tests for C"
  [ systemTests, unitTests ]

systemTests = testGroup "null-deref system tests for C"
  [ testCase "null-deref: basic checks (case 1)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr1.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr1.c" 13 25 ] . map fst
  , testCase "null-deref: ptrs access in decls (case 2)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr2.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr2.c" 14 13 ] . map fst
  , testCase "null-deref: ptr deref in decl and check it in nested if (case 3)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr3.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr3.c" 14 42 ] . map fst
  , testCase "null-deref: ptr array deref  (case 4)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr4.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr4.c" 15 23 ] . map fst
  , testCase "null-deref: ptr member deref (case 5)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr5.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr5.c" 13 22 ] . map fst
  -- , testCase "null-deref: ptr add deref (case 6)" $
  --   runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr6.c"])
  --   >>= assertEqual "" [ newPos "examples/c/nullptr6.c" 16 23 ] . map fst
  , testCase "null-deref: deref within a logical expression (case 7)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr7.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr7.c" 24 32 ] . map fst
  , testCase "null-deref: check deref in switch stmt (case 8)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr8.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr8.c" 13 18 ] . map fst
  , testCase "null-deref: local variable decls (case 9)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr9.c"])
    >>= assertEqual "" [] . map fst
  , testCase "null-deref: ternary conditional expr (case 10)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr10.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr10.c" 14 18 ] . map fst
  , testCase "null-deref: FPs by mishandling of terminated paths (case 11)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr11.c"])
    >>= assertEqual "" [] . map fst
  , testCase "null-deref: nested access, but only null-dref-check of root (case 12)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr12.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr12.c" 13 18 ] . map fst
  , testCase "null-deref: ptr access in if condition (case 13)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr13.c"])
    >>= assertEqual "" [ newPos "examples/c/nullptr/nullptr13.c" 13 7 ] . map fst
  , testCase "null-deref: FPs by assumptions of a condition (case 14)" $
    runChecker (Options TypeNullDeref LangC ["examples/c/nullptr/nullptr14.c"])
    >>= assertEqual "" [] . map fst
  ]

toExpr s = P.tryExprParser $ WildCard (tokenize s)
  where tokenize s = case parse pTokens "" s of
                       Left err -> []
                       Right xs -> xs
deletePos = Map.map fst
callFindAssumptions b s = deletePos $ NC.findAssumptions (toExpr s) b Map.empty
genFAsTC expr b result =
  testCase ("null-deref-findAssumptions: (" ++ show b ++ ") " ++ expr) $
    assertEqual "" (Map.fromList (fillDummyPos result))
                   (callFindAssumptions b expr)
  where fillDummyPos = List.map (\(name, stat) -> ((name, dummyPos), stat))
        dummyPos = newPos "" 0 0

unitTests = testGroup "null-deref findAssumptions tests for C"
  [ genFAsTC "ptr" True [("ptr", IsNotNull)]
  , genFAsTC "ptr != NULL" True [("ptr", IsNotNull)]
  , genFAsTC "ptr != NULL && ptr2" True
             [("ptr", IsNotNull), ("ptr2", IsNotNull)]
  , genFAsTC "!ptr || !ptr2" True []
  , genFAsTC "!ptr || !ptr2" False
             [("ptr", IsNotNull), ("ptr2", IsNotNull)]
  , genFAsTC "(!ptr && !ptr2)" True
             [("ptr", IsNull), ("ptr2", IsNull)]
  , genFAsTC "(ptr && !ptr2)" False []
  , genFAsTC "(ptr && !ptr2) || ptr3" True []
  , genFAsTC "(ptr && !ptr2) || ptr3" False
             [("ptr3", IsNull)]
  , genFAsTC "(ptr || !ptr2) && ptr3" True
             [("ptr3", IsNotNull)]
  , genFAsTC "(ptr || !ptr2) && ptr3" False []
  , genFAsTC "!(ptr || ptr2) && ptr3" True
             [("ptr", IsNull), ("ptr2", IsNull), ("ptr3", IsNotNull)]
  , genFAsTC "!(ptr || ptr2) && ptr3" False []
  , genFAsTC "ptr && ptr->member" True
             [("ptr", IsNotNull), ("ptr->member", IsNotNull)]
  , genFAsTC "ptr && ptr->member" False []
  ]
