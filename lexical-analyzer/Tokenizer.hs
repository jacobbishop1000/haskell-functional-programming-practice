module Tokenizer where
main :: IO ()
main = do
   putStrLn $ testOutput tokenize "tokenize" test_string_1 expected_value_1
   putStrLn $ testOutput tokenize "tokenize" test_string_2 expected_value_2
   putStrLn $ testOutput tokenize "tokenize" test_string_3 expected_value_3
   putStrLn $ testOutput tokenize "tokenize" test_string_4 expected_value_4
   putStrLn $ testOutput tokenize "tokenize" test_string_5 expected_value_5
   putStrLn $ testOutput tokenize "tokenize" test_string_6 expected_value_6
   putStrLn $ testOutput tokenize "tokenize" test_string_7 expected_value_7
   putStrLn $ testOutput tokenize "tokenize" test_string_8 expected_value_8
   putStrLn $ testOutput tokenize "tokenize" test_string_9 expected_value_9
   putStrLn $ testOutput tokenize "tokenize" test_string_10 expected_value_10
   putStrLn $ testOutput tokenize "tokenize" test_string_11 expected_value_11
   putStrLn $ testOutput tokenize "tokenize" test_string_12 expected_value_12
   putStrLn $ testOutput tokenize "tokenize" test_string_13 expected_value_13
   putStrLn $ testOutput tokenize "tokenize" test_string_14 expected_value_14
   putStrLn $ testOutput tokenize "tokenize" test_string_15 expected_value_15

   where test_string_1 = "3-10*25/SoG" --Prof. Renet's example test 1
         expected_value_1 = [("3",Value 3),("-",Subtract),("10",Value 10),("*",Multiply),("25",Value 25),("/",Divide),("SoG",Id)]
         test_string_2 = "(2+ x ) *\t40" --Prof. Renet's example test 2
         expected_value_2 = [("(",LeftPar),("2",Value 2),("+",Add),("x",Id),(")",RightPar),("*",Multiply),("40",Value 40)]
         test_string_3 = "35 + 10 -   \n  (4 / MonthlyResult )" --Prof. Renet's example test 3
         expected_value_3 = [("35",Value 35),("+",Add),("10",Value 10),("-",Subtract),("(",LeftPar),("4",Value 4),("/",Divide),("MonthlyResult",Id),(")",RightPar)]
         test_string_4 = "5++ */tomorrow(-3" --Prof. Renet's example test 4
         expected_value_4 = [("5",Value 5),("+",Add),("+",Add),("*",Multiply),("/",Divide),("tomorrow",Id),("(",LeftPar),("-",Subtract),("3",Value 3)]
         test_string_5 = "/*+-" --Arithmetic symbols
         expected_value_5 = [("/",Divide),("*",Multiply),("+",Add),("-", Subtract)]
         test_string_6 = "&12+9*12/450" --Illegal char in front
         expected_value_6 = [] :: [(String, Token)]
         test_string_7 = "12+9*12%/450" --Illegal char in middle
         expected_value_7 = [] :: [(String, Token)]
         test_string_8 = "12+9*12/450@" --Illegal char at end
         expected_value_8 = [] :: [(String, Token)]
         test_string_9 = "1234" --Just Value
         expected_value_9 = [("1234", Value 1234)]
         test_string_10 = "" -- empty string
         expected_value_10 = [] :: [(String, Token)]
         test_string_11 = "\n\n  \t   \n\t                        \n" -- just whitespace
         expected_value_11 = [] :: [(String, Token)]
         test_string_12 = "  pi qwertyUIOP chi delta " -- Just Ids and whitespace
         expected_value_12 = [("pi",Id),("qwertyUIOP",Id),("chi",Id),("delta",Id)]
         test_string_13 = "  )\t\t   \n   ))(   \t)\n(      " -- Just Parentheses and whitespace
         expected_value_13 = [(")", RightPar),(")", RightPar),(")", RightPar),("(", LeftPar),(")", RightPar),("(", LeftPar)]
         test_string_14 = "cPWAPdDtfCwdykMUoYxTpKwwML Gjb  VhcE/yrVbHDf WbFYVH\nazUC Au+fjgyEKu(j/UDOMjTqtljopAi+NskGxk+MPLX\tdlFRde" --Random 100 letters, operators with added whitespace values
         expected_value_14 = [("cPWAPdDtfCwdykMUoYxTpKwwML",Id),("Gjb",Id),("VhcE",Id),("/",Divide),("yrVbHDf",Id),("WbFYVH",Id),("azUC",Id),("Au",Id),("+",Add),("fjgyEKu",Id),("(",LeftPar),("j",Id),("/",Divide),("UDOMjTqtljopAi",Id),("+",Add),("NskGxk",Id),("+",Add),("MPLX",Id),("dlFRde",Id)]
         test_string_15 = "\n8t  )       S //(  Cub 9  X7 QUB z p9 ( P2  C \n  / T )a  \t\t   Enw G*  i)-TG   mH- Lo +H  1 \n  +/)C *az2" --The ultimate test with lots of tokens of all types
         expected_value_15 = [("8", Value 8),("t", Id),(")", RightPar),("S", Id),("/", Divide),("/", Divide),("(", LeftPar),("Cub", Id),("9", Value 9),("X", Id),("7", Value 7),("QUB", Id),("z", Id),("p", Id),("9", Value 9),("(", LeftPar),("P", Id),("2", Value 2),("C", Id),("/", Divide),("T", Id),(")", RightPar),("a", Id),("Enw", Id),("G", Id),("*", Multiply),("i", Id),(")", RightPar),("-", Subtract),("TG", Id),("mH", Id),("-", Subtract),("Lo", Id),("+", Add),("H", Id),("1", Value 1),("+", Add),("/", Divide),(")", RightPar),("C", Id),("*", Multiply),("az", Id),("2", Value 2)]



--Token data type
data Token = Id | Value Integer | Divide | Multiply | Subtract | Add | LeftPar | RightPar | Space deriving (Eq, Show)
--Tokenizer function
tokenize :: String -> [(String, Token)]
tokenize [] = []
tokenize tokens
 | (all (isValidChar) (tokens)) = filter (not . isTokenEmpty) (makeTokens tokens)
 | otherwise = []

--Auxiliary Functions
makeTokens :: String -> [(String, Token)]
makeTokens [] = []
makeTokens (tk:tokens)
 | tk == '/' = ([tk], Divide) : makeTokens tokens
 | tk == '+' = ([tk], Add) : makeTokens tokens
 | tk == '-' = ([tk], Subtract) : makeTokens tokens
 | tk == '*' = ([tk], Multiply) : makeTokens tokens
 | tk == '(' = ([tk], LeftPar) : makeTokens tokens
 | tk == ')' = ([tk], RightPar) : makeTokens tokens
 | isId tk = makeMulticharTokenTuple (takeWhile (isId) (tk:tokens)) : makeTokens (dropWhile (isId) (tokens))
 | isValue tk = makeMulticharTokenTuple (takeWhile (isValue) (tk:tokens)) : makeTokens (dropWhile (isValue) (tokens))
 | isSpace tk = ("",Space) : makeTokens (dropWhile (isSpace) (tokens))
 | otherwise =[]

isTokenEmpty :: (String, Token) -> Bool
isTokenEmpty (a, b) = if a == [] then True else False

isValidChar :: Char -> Bool
isValidChar x = if (isId x|| isValue x|| isSpace x || elem x "/+-*)(") then True else False

isId :: Char -> Bool
isId x = if (elem x ['A'..'Z'] || elem x ['a'..'z']) then True else False

isValue :: Char -> Bool
isValue x = if (('0' <= x && x <= '9')) then True else False

isSpace :: Char -> Bool
isSpace x = if (elem x "\n\t ") then True else False

makeMulticharTokenTuple :: String -> (String, Token)
makeMulticharTokenTuple token
 | isId (head token) = (token, Id)
 | isValue (head token) = (token, Value (read token :: Integer))

-- A crude testing function
testOutput :: (Show a, Eq a, Show b, Eq b) => (a -> b) -> String -> a -> b -> String
testOutput f name input expected = concat [ name, " ", show input, " should be ", show expected, " --> ",
                                      let result = f input in 
                                          if result == expected
                  then "PASS"
                      else concat ["FAIL (actual: ", show result, ")"]]