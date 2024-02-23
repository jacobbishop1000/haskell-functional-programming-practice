module Main where


main :: IO ()
main = do
 -- 2 * 3
 putStrLn $ show $ mult (Succ (Succ Zero))  (Succ (Succ (Succ Zero)))

 putStrLn $ show $ occurs 7 tree
 putStrLn $ show $ occurs 5 tree
 putStrLn $ show $ occurs 8 tree
 putStrLn $ show $ binarySearch 7 tree
 putStrLn $ show $ binarySearch' 7 tree

 putStrLn $ show $ isBalanced balancedTree
 putStrLn $ show $ isBalanced unbalancedTree

 putStrLn $ show $ split [1,2,3,4,5]
 putStrLn $ show $ split [1]
 putStrLn $ show $ toTree [1,2,3,4,5,6,7]
 putStrLn $ show $ toTree [1,2,3,4,5,6,7,8]
 putStrLn $ show $ folde (*2) (+) expression -- 32
 putStrLn $ show $ folde (*3) (-) expression -- 24
 putStrLn $ show $ eval expression -- 32
 putStrLn $ show $ size expression -- 5

 putStrLn $ show $ (Just 5) == (Just 4)
 putStrLn $ show $ (Just 4) == (Just 4)

 where tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6)  7 (Leaf  9))
       balancedTree = Node' ( Node' (Node' (Leaf' 1) (Leaf' 2)) (Leaf' 3) ) (Node'  (Node' (Leaf' 4) (Leaf' 5)) (Node' (Leaf' 6)  (Leaf' 7 )))
       unbalancedTree = Node' ( Node' (Node' (Leaf' 1) (Leaf' 2)) (Leaf' 3) ) (Node'  (Node' ( Node' (Leaf' 4) (Leaf' 5)) (Leaf' 6)) (Node' (Leaf' 7)  (Leaf' 8 )))
       expression = Add (Add (Val 2) (Add (Val 1) (Val 5))) (Add (Val 3) (Val 5))

-- Exercise 1

data Nat = Zero | Succ Nat deriving (Show)

add:: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
--(m+1) n = (m+n)+1

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = (add (mult m n) n)

-- Exercise 2
 
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l k r) = x == k || (occurs x l) || (occurs x r)

binarySearch :: Ord a => a -> Tree a -> Bool
binarySearch x (Leaf y) = x == y
binarySearch x (Node l k r) | x == k = True
                            | x < k = binarySearch x l
                            | otherwise = binarySearch x r
-- use compare
binarySearch' :: Ord a => a -> Tree a -> Bool
binarySearch' x (Leaf y) = x == y
binarySearch' x (Node l k r) = case (compare x k) of
                             LT -> binarySearch' x l
                             GT -> binarySearch' x r
                             EQ -> True

-- Exercise 3

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Eq, Show, Ord)

leaves :: Tree' Integer -> Integer
leaves Leaf' _ = 1
leaves (Node' l r) = (leaves l) + (leaves r)

isBalanced :: Tree' Integer -> Bool
isBalanced = undefined

-- Exercise 4
split :: [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split s = ( take i s, drop i s)
          where i = (length s `div` 2) 

toTree :: [a] -> Tree' a
toTree = undefined

-- Exercise 5
data Expr = Val Int | Add Expr Expr  deriving (Show,Eq)

folde :: (Int -> a ) -> (a -> a -> a) -> Expr -> a
folde = undefined

-- Exercise 6
eval :: Expr -> Int
eval = undefined

size :: Expr -> Int
size = undefined

-- Exercise 7
data Maybe' a = Just' a | Nothing'
instance Eq a => Eq (Maybe' a) where
Nothing' == Nothing' = True

-- TO BE COMPLETED


