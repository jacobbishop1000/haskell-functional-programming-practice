module Main where


main :: IO ()
main = do
 putStrLn $ show $ (Just' 5) == (Just' 4)
 putStrLn $ show $ (Just' 4) == (Just' 4)
 putStrLn $ show $ (Just' 3.1415926535) == (Just' 2.7182818)
 putStrLn $ show $ (Just' 3.1415926535) == (Just' 3.1415926535)
 --I'm not sure how to test equality with Nothing' since Haskell seems not to allow Nothing as an argument to the '==' operator
-- Exercise 7
data Maybe' a = Just' a | Nothing'
instance Eq a => Eq (Maybe' a) where
 Nothing' == Nothing' = True
 Just' x == Just' y = x == y
 _ == _ = False