import Eval.Eval (eval)
import Lexer.Lexer (tokenize)
import Lexer.Token (Token (..))
import Parser.Ast (Block (..), Expression (..), Identifier (..), Program (Program), Statement (..))
import Parser.Parser (parsing)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [tokenizerTests, parsingTests, evalTests]

tokenizerTests :: TestTree
tokenizerTests =
  testGroup
    "tokenizing"
    [ testCase "brackets" (assertEqual "should be [LBrace, RBrace, Attrib, LParen, RParen, EOF]" [LBrace, RBrace, LParen, RParen, EOF] (tokenize "{}()"))
    , testCase "Attrib and identifiers" (assertEqual "should be [Let, Ident \"x\", Assign, Ident \"y\", EOF]" [Let, Ident "x", Attrib, Ident "y", EOF] (tokenize "let x = y"))
    , testCase "math operators" (assertEqual "should be [Plus, Minus, Asterisk, Slash, EOF]" [Plus, Minus, Asterisk, Slash, EOF] (tokenize "+-*/"))
    , testCase "comparison operators" (assertEqual "should be [LessThan, GreaterThan, Equal, NotEqual, EOF]" [LessThan, GreaterThan, Equal, NotEqual, EOF] (tokenize "<>==!="))
    , testCase "comma and semicolon" (assertEqual "should be [Comma, Semicolon, EOF]" [Comma, Semicolon, EOF] (tokenize ",;"))
    , testCase "bang" (assertEqual "should be [Bang, EOF]" [Bang, EOF] (tokenize "!"))
    , testCase "integers" (assertEqual "should be [Int \"1\", Int \"2\", EOF]" [Int "12", EOF] (tokenize "12"))
    , testCase "booleans" (assertEqual "should be [TokTrue, TokFalse, EOF]" [TokTrue, TokFalse, EOF] (tokenize "true false"))
    , testCase "if else" (assertEqual "should be [If, Else, EOF]" [If, Else, EOF] (tokenize "if else"))
    , testCase "return" (assertEqual "should be [Return, EOF]" [Return, EOF] (tokenize "return"))
    , testCase "function" (assertEqual "should be [Function, EOF]" [Function, EOF] (tokenize "fn"))
    ]

parsingTests :: TestTree
parsingTests =
  testGroup
    "parsing"
    [ testCase "parse Attrib of Int value" (assertEqual "should be [Int \"1\", EOF]" (Program [LetStatement (Identifier "a") (IntLiteral 5)]) (parsing [Let, Ident "a", Attrib, Int "5", Semicolon, EOF]))
    , testCase "parse Attrib of Boolean value" (assertEqual "should be [TokTrue, EOF]" (Program [LetStatement (Identifier "a") (BooleanLiteral True)]) (parsing [Let, Ident "a", Attrib, TokTrue, Semicolon, EOF]))
    , testCase "parse Attrib of Identifier value" (assertEqual "should be [Ident \"a\", EOF]" (Program [LetStatement (Identifier "a") (IdentifierExpression (Identifier "b"))]) (parsing [Let, Ident "a", Attrib, Ident "b", Semicolon, EOF]))
    , testCase "parse if with else" (assertEqual "if with else with 5<10" (Program [ExpressionStatement (IfExpression (LessThanExpression (IntLiteral 5) (IntLiteral 10)) (Block [ReturnStatement (BooleanLiteral True)]) (Just (Block [ReturnStatement (BooleanLiteral False)])))]) (parsing [If, LParen, Int "5", LessThan, Int "10", RParen, LBrace, Return, TokTrue, Semicolon, RBrace, Else, LBrace, Return, TokFalse, Semicolon, RBrace, Semicolon, EOF]))
    , testCase "parse function" (assertEqual "literal function" (Program [LetStatement (Identifier "a") (FunctionExpression [Identifier "x", Identifier "y"] (Block [ReturnStatement (AddExpression (IdentifierExpression (Identifier "x")) (IdentifierExpression (Identifier "y")))]))]) (parsing [Let, Ident "a", Attrib, Function, LParen, Ident "x", Comma, Ident "y", RParen, LBrace, Return, Ident "x", Plus, Ident "y", Semicolon, RBrace, Semicolon, EOF]))
    , testCase "parse function call" (assertEqual "call literal function" (Program [ExpressionStatement (CallExpression (FunctionExpression [Identifier "x", Identifier "y"] (Block [ExpressionStatement (AddExpression (IdentifierExpression (Identifier "x")) (IdentifierExpression (Identifier "y")))])) [IntLiteral 2, IntLiteral 3])]) (parsing [Function, LParen, Ident "x", Comma, Ident "y", RParen, LBrace, Ident "x", Plus, Ident "y", Semicolon, RBrace, LParen, Int "2", Comma, Int "3", RParen, Semicolon, EOF]))
    , testCase "parse function with if" (assertEqual "arroz" (Program [ExpressionStatement (CallExpression (FunctionExpression [Identifier "x", Identifier "y"] (Block [ExpressionStatement (IfExpression (BooleanLiteral True) (Block [ExpressionStatement (IdentifierExpression (Identifier "x"))]) (Just (Block [ExpressionStatement (IdentifierExpression (Identifier "x"))])))])) [IntLiteral 1, IntLiteral 2])]) (parsing [Function, LParen, Ident "x", Comma, Ident "y", RParen, LBrace, If, LParen, TokTrue, RParen, LBrace, Ident "x", RBrace, Else, LBrace, Ident "x", RBrace, RBrace, LParen, Int "1", Comma, Int "2", RParen, Semicolon, EOF]))
    , testCase "parse arithmetic expression" (assertEqual "arithmetic" (Program [ExpressionStatement (AddExpression (IntLiteral 1) (DivExpression (MulExpression (IntLiteral 2) (IntLiteral 3)) (IntLiteral 4)))]) (parsing [Int "1", Plus, Int "2", Asterisk, Int "3", Slash, Int "4", Semicolon, EOF]))
    ]

interpret :: String -> String
interpret = show . eval . parsing . tokenize

evalTests :: TestTree
evalTests =
  testGroup
    "eval"
    [ testCase "eval integer" (assertEqual "should be 1" "1" (interpret "1;"))
    , testCase "eval + operator" (assertEqual "should be 3" "3" (interpret "1+2;"))
    , testCase "eval - operator" (assertEqual "should be 1" "1" (interpret "2-1;"))
    , testCase "eval * operator" (assertEqual "should be 6" "6" (interpret "2*3;"))
    , testCase "eval / operator" (assertEqual "should be 2" "2" (interpret "6/3;"))
    , testCase "eval arithmetic expression" (assertEqual "should be 7" "7" (interpret "1+2*3;"))
    , testCase "eval arithmetic expression" (assertEqual "should be 9" "9" (interpret "(1+2)*3;"))
    , testCase "eval arithmetic expression" (assertEqual "should be 16" "16" (interpret "(8/2)*(2+2);"))
    , testCase "eval true" (assertEqual "should be true" "true" (interpret "true;"))
    , testCase "eval false" (assertEqual "should be false" "false" (interpret "false;"))
    , testCase "eval < boolean expression" (assertEqual "should be true" "true" (interpret "1<2;"))
    , testCase "eval > boolean expression" (assertEqual "should be false" "false" (interpret "1>2;"))
    , testCase "eval == boolean expression" (assertEqual "should be true" "true" (interpret "1==1;"))
    , testCase "eval != boolean expression" (assertEqual "should be false" "false" (interpret "1!=1;"))
    , testCase "eval true == true boolean expression" (assertEqual "should be true" "true" (interpret "true==true;"))
    , testCase "eval true != true boolean expression" (assertEqual "should be false" "false" (interpret "true!=true;"))
    , testCase "eval true == false boolean expression" (assertEqual "should be false" "false" (interpret "true==false;"))
    , testCase "eval true != false boolean expression" (assertEqual "should be true" "true" (interpret "true!=false;"))
    , testCase "eval false == false boolean expression" (assertEqual "should be true" "true" (interpret "false==false;"))
    , testCase "eval false != false boolean expression" (assertEqual "should be false" "false" (interpret "false!=false;"))
    , testCase "eval if" (assertEqual "should be 1" "1" (interpret "if (true) {1;};"))
    , testCase "eval if" (assertEqual "should be null" "null" (interpret "if (false) {1;};"))
    , testCase "eval if else" (assertEqual "should be 1" "1" (interpret "if (true) {1;} else {2;};"))
    , testCase "eval if else" (assertEqual "should be 2" "2" (interpret "if (false) {1;} else {2;};"))
    , testCase "eval if else" (assertEqual "should be 1" "1" (interpret "if (2>1) {1;} else {2;};"))
    , testCase "eval if else" (assertEqual "should be 2" "2" (interpret "if (2<1) {1;} else {2;};"))
    ]
