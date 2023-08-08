import Lexer.Lexer (tokenize)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Token (Token (..))

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [tokenizerTests]

tokenizerTests :: TestTree
tokenizerTests =
  testGroup
    "tokenizer"
    [ testCase "brackets" (assertEqual "should be [LBrace, RBrace, Attrib, LParen, RParen, EOF]" [LBrace, RBrace, LParen, RParen, EOF] (tokenize "{}()"))
    , testCase "Attrib and identifiers" (assertEqual "should be [Let, Ident \"x\", Assign, Ident \"y\", EOF]" [Let, Ident "x", Attrib, Ident "y", EOF] (tokenize "let x = y"))
    , testCase "math operators" (assertEqual "should be [Plus, Minus, Asterisk, Slash, EOF]" [Plus, Minus, Asterisk, Slash, EOF] (tokenize "+-*/"))
    , testCase "comparison operators" (assertEqual "should be [LessThan, GreaterThan, Equal, NotEqual, EOF]" [LessThan, GreaterThan, Equal, NotEqual, EOF] (tokenize "<>==!="))
    , testCase "comma and semicolon" (assertEqual "should be [Comma, Semicolon, EOF]" [Comma, Semicolon, EOF] (tokenize ",;"))
    , testCase "bang" (assertEqual "should be [Bang, EOF]" [Bang, EOF] (tokenize "!"))
    , testCase "integers" (assertEqual "should be [Int \"1\", Int \"2\", EOF]" [Int "12", EOF] (tokenize "12"))
    , testCase "booleans" (assertEqual "should be [TokTrue, TokFalse, EOF]" [TokTrue, TokFalse, EOF] (tokenize "true false"))
    , testCase "if else" (assertEqual "should be [If, Else, EOF]" [If, Else, EOF] (tokenize "if else"))
    , testCase "return"  (assertEqual "should be [Return, EOF]" [Return, EOF] (tokenize "return"))
    , testCase "function" (assertEqual "should be [Function, EOF]" [Function, EOF] (tokenize "fn"))
    ]
