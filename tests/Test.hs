import Test.Tasty
import Test.Tasty.HUnit

import C.NullDerefTest
import C.ParserTest
import Cpp.ParserTest

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [cTests, cppTests]

cTests = testGroup "Tests for C" [cNullDerefTests, cParserTests]

cppTests = testGroup "Tests for C++" [cppParserTests]
