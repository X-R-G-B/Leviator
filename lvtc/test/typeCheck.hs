import Test.Tasty
import Test.Tasty.HUnit
import AST

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "typeCheck tests"
    [testReturn]

testReturn :: TestTree
testReturn = testGroup "testReturn" [
    testCase "testReturn" (True ==
      (typeCheck (("main", [], "Int"), ([Declaration (("a", "Int"), Integer 0), Return (Integer 69)]))))
]

shouldBeTrue :: [FuncDeclaration] -> Assertion
shouldBeTrue funcs = (typeCheck funcs) @?= True

shouldBeFalse :: [FuncDeclaration] -> Assertion
shouldBeFalse funcs = (typeCheck funcs) @?= False
