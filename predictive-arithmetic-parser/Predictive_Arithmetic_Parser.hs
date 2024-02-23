module Parser where

main :: IO ()
main = do

   putStrLn $ testOutput parse "parse" test_value_1 expected_value_1
   putStrLn $ testOutput parse "parse" test_value_2 expected_value_2
   putStrLn $ testOutput parse "parse" test_value_3 expected_value_3
   putStrLn $ testOutput parse "parse" test_value_4 expected_value_4
   putStrLn $ testOutput parse "parse" test_value_5 expected_value_5
   putStrLn $ testOutput parse "parse" test_value_6 expected_value_6
   putStrLn $ testOutput parse "parse" test_value_7 expected_value_7
   putStrLn $ testOutput parse "parse" test_value_8 expected_value_8
   putStrLn $ testOutput parse "parse" test_value_9 expected_value_9
   putStrLn $ testOutput parse "parse" test_value_10 expected_value_10
   putStrLn $ testOutput parse "parse" test_value_11 expected_value_11
   putStrLn $ testOutput parse "parse" test_value_12 expected_value_12
   putStrLn $ testOutput parse "parse" test_value_13 expected_value_13
   putStrLn $ testOutput parse "parse" test_value_14 expected_value_14
   putStrLn $ testOutput parse "parse" test_value_15 expected_value_15
   putStrLn $ testOutput parse "parse" test_value_16 expected_value_16
   putStrLn $ testOutput parse "parse" test_value_17 expected_value_17
   where test_value_1 = [LeftPar,Value 5,Add,Value 3,RightPar,Multiply,Value 9,Subtract,Value 4] --Prof. Renet's example test 1
         expected_value_1 = True
         test_value_2 = [Value 5,Divide,LeftPar,Id,Multiply,Id,Subtract,Value 4,Multiply,Value 5,RightPar,Add,Value 3] --Prof. Renet's example test 2
         expected_value_2 = True
         test_value_3 = [LeftPar,LeftPar,LeftPar,Value 5,Add,Id,RightPar,Multiply,Value 3,RightPar,Subtract,Value 2,RightPar,Divide,Value 7] --Prof. Renet's example test 3
         expected_value_3 = True
         test_value_4 = [LeftPar,LeftPar,Value 5,Add,Id,RightPar,Multiply,Value 3,RightPar,RightPar,Subtract,Value 2,RightPar,Divide,Value 7] --Prof. Renet's example test 4
         expected_value_4 = False
         test_value_5 = [LeftPar,Value 5,Add,Multiply,Id,Id,RightPar,Add,Value 3,Divide,Value 7] --Prof. Renet's example test 5
         expected_value_5 = False
         test_value_6 = [LeftPar,LeftPar,LeftPar,LeftPar,Value 5,RightPar,RightPar,RightPar,RightPar] --Multiple parentheses layers
         expected_value_6 = True
         test_value_7 = [Value 5, Add, Value 9] --Simple addition to ensure basic function calls work as expected
         expected_value_7 = True
         test_value_8 = [Value 72382173] --Just a Value
         expected_value_8 = True
         test_value_9 = [] --The empty string (Special Case)
         expected_value_9 = True
         test_value_10 = [RightPar, Value 78, LeftPar] --Wrong parentheses order
         expected_value_10 = False
         test_value_11 = [Id] --Just an Id
         expected_value_11 = True
         test_value_12 = [Divide, Value 67] --Detecting faulty terms
         expected_value_12 = False
         test_value_13 = [Id, Multiply] --Detecting faulty terms pt. 2 (Special Case)
         expected_value_13 = False
         test_value_14 = [Add, Id] --Detecting faulty expressions
         expected_value_14 = False
         test_value_15 = [Value 67, Subtract] --Detecting faulty expressions pt. 2 (Special Case)
         expected_value_15 = False
         test_value_16 = [Value 67, Error, Divide, Id] --Test to make sure user can't pass Error as valid token
         expected_value_16 = False
         test_value_17 = [LeftPar, Value 8, Subtract, Value 1, Add, Value 3, RightPar, Multiply, Value 6, Subtract, LeftPar, LeftPar, Value 3, Add, Value 7, RightPar, Multiply, Value 2, RightPar] --Testing compound expressions
         expected_value_17 = True
{-
   --Tests for consume function
   putStrLn $ show (consume LeftPar test_value_1)
   putStrLn $ show expected_value_1
   putStrLn $ show (consume (Value 5) test_value_2)
   putStrLn $ show expected_value_2
   putStrLn $ show (consume Add test_value_3)
   putStrLn $ show expected_value_3
   putStrLn $ show (consume (Value 3) test_value_4)
   putStrLn $ show expected_value_4
   putStrLn $ show (consume RightPar test_value_5)
   putStrLn $ show expected_value_5
   putStrLn $ show (consume Multiply test_value_6)
   putStrLn $ show expected_value_6
   putStrLn $ show (consume (Value 9) test_value_7)
   putStrLn $ show expected_value_7
   putStrLn $ show (consume Subtract test_value_8)
   putStrLn $ show expected_value_8
   --putStrLn $ testOutput (consume) ("consume") (LeftPar test_value_1) (expected_value_1)

   where test_value_1 = [LeftPar,Value 5,Add,Value 3,RightPar,Multiply,Value 9,Subtract,Value 4]
         expected_value_1 = [Value 5,Add,Value 3,RightPar,Multiply,Value 9,Subtract,Value 4]
         test_value_2 = [Value 5,Add,Value 3,RightPar,Multiply,Value 9,Subtract,Value 4]
         expected_value_2 = [Add,Value 3,RightPar,Multiply,Value 9,Subtract,Value 4]
         test_value_3 = [Add,Value 3,RightPar,Multiply,Value 9,Subtract,Value 4]
         expected_value_3 = [Value 3,RightPar,Multiply,Value 9,Subtract,Value 4]
         test_value_4 = [Value 3,RightPar,Multiply,Value 9,Subtract,Value 4]
         expected_value_4 = [RightPar,Multiply,Value 9,Subtract,Value 4]
         test_value_5 = [RightPar,Multiply,Value 9,Subtract,Value 4]
         expected_value_5 = [Multiply,Value 9,Subtract,Value 4]
         test_value_6 = [Multiply,Value 9,Subtract,Value 4]
         expected_value_6 = [Value 9,Subtract,Value 4]
         test_value_7 = [Value 9,Subtract,Value 4]
         expected_value_7 = [Subtract,Value 4]
         test_value_8 = [Subtract,Value 4]
         expected_value_8 = [Value 4]
-}
--Token data type definition
data Token = Value Integer | Id | LeftPar | RightPar | Add | Subtract | Multiply | Divide | Error deriving (Eq, Show)

parse :: [Token] -> Bool
parse [] = True
parse ts
 | elem (last ts) [Add, Subtract, Multiply, Divide] = False --This handles special cases for tests 13 and 15
 | otherwise = null $ (expr ts)

consume :: Token -> [Token] -> [Token]
consume _ [] = []
consume tk (t:ts)
 | tk == t = ts
 | tk == Error = [Error] --Prevents 'Error' token from being parsed as valid token when it is a placeholder to prevent errors
 | otherwise = [Error] --Rewrites Token list to singleton list with [Error], which will be brought up the tree and evaluated to False

--Grammar rule 1: E -> T E'
expr :: [Token] -> [Token]
expr [Error] = [Error]
expr ts = (expr' . term) ts

--Grammar rule 2: E' -> + T E' | - T E' | ϵ
expr' :: [Token] -> [Token]
expr' [] = []
expr' [Error] = [Error]
expr' ts
 | head ts == Add = (expr' . term . consume Add) ts
 | head ts == Subtract = (expr' . term . consume Subtract) ts
 | otherwise = ts

--Grammar rule 3: T -> F T'
term :: [Token] -> [Token]
term [Error] = [Error]
term ts = (term' . factor) ts

--Grammar rule 4: T' -> * F T' | / F T' | ϵ
term' :: [Token] -> [Token]
term' [] = []
term' [Error] = [Error]
term' ts
 | head ts == Multiply = (term' . factor . consume Multiply) ts
 | head ts == Divide = (term' . factor . consume Divide) ts
 | otherwise = ts

--Grammar rule 5: F -> Value | Id | ( E )
factor :: [Token] -> [Token]
factor [] = []
factor [Error] = [Error]
factor ((Id):ts) = ts --Ad hoc consume for Id
factor ((Value _):ts) = ts --Ad hoc consume for Value
factor ts = (consume RightPar . expr . consume LeftPar) ts

-- A crude testing function
testOutput :: (Show a, Eq a, Show b, Eq b) => (a -> b) -> String -> a -> b -> String
testOutput f name input expected = concat [ name, " ", show input, " should be ", show expected, " --> ",
                                      let result = f input in 
                                          if result == expected
                  then "PASS"
                      else concat ["FAIL (actual: ", show result, ")"]]