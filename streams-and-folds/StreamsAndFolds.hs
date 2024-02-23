-- CS431, Spring 2022
--nprenet@bsu.edu

module StreamsAndFolds where


main :: IO ()
main = do 
        -- testing partialSums
        putStrLn $ let twentySums = take 20 $ partialSums
                       expTwenty = [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190]
                       oneHundredSums = take 100 $ partialSums
                       expOneHundred = [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,
                                        190,210,231,253,276,300,325,351,378,406,435,465,496,528,561,
                                        595,630,666,703,741,780,820,861,903,946,990,1035,1081,1128,
                                        1176,1225,1275,1326,1378,1431,1485,1540,1596,1653,1711,1770,
                                        1830,1891,1953,2016,2080,2145,2211,2278,2346,2415,2485,2556,
                                        2628,2701,2775,2850,2926,3003,3081,3160,3240,3321,3403,3486,
                                        3570,3655,3741,3828,3916,4005,4095,4186,4278,4371,4465,4560,
                                        4656,4753,4851,4950]
                   in concat [
                           testOutputNoArg oneHundredSums "partialSums" expOneHundred,
                           testOutputNoArg twentySums "partialSums" expTwenty ]

        -- testing filter'
        putStrLn $ concat [ 
                    testOutput (filter' (`elem` ['A'..'Z'])) "filter'" "AbCdEf" "ACE",
                    testOutput (filter' (>4)) "filter'" [3,1,5,2,8,2,10,4,7] [5,8,10,7],
                    testOutput (filter' (elem 'a')) "filter'" ["anc", "def", "aad"] ["anc", "aad"],
                    testOutput (filter' (\x -> True)) "filter'" ([]::[Integer]) []]



-- Give an implicit (recursive) definition of partialSums,
-- whose output is the infinite series of the integer sums:
-- [0,(0+1),(0+1+2),...] = [0,1,3,6,10,...]
-- The function takes no argument.
partialSums :: (Integral a) => [a]
partialSums = 0 : (zipWith (+) [1..] partialSums)

-- Give a definition of 'filter' in terms of 'foldr'
-- Hint: the function h passed to 'foldr' accepts as parameters
--  + 'x', the current head of the list
--  + 'b', the value returned by the fold (from the right), i.e. a list
--  What should this function return, depending of the value (f x), where
--  'f' is the Boolean function passed to 'filter'? Define h either in
--  a lambda expression, or in a where clause.
filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = foldr (\x b -> if f x == True then [x]++b else b) [] l

 -- A crude testing function
testOutput :: (Show a, Eq a, Show b, Eq b) => (a -> b) -> String -> a -> b -> String
testOutput f name input expected = concat [ let result = f input in
                                             if result == expected
                                             then "."
                                             else concat ["\n", name, " ", show input, " should be ", show expected,
                                                        " --> FAIL (actual: ", show result, ")\n"]]


testOutputNoArg :: (Show a, Eq a) => a -> String -> a -> String
testOutputNoArg f name expected = concat [ let result = f in
                                             if result == expected
                                             then "."
                                             else concat ["\n", name, " should be ", show expected,
                                                        " --> FAIL (actual: ", show result, ")\n"]]


