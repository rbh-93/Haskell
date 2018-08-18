> import Data.Bool
> import Data.List

Boolean Functions implemented using higher order functions

> allTrue :: [Bool] -> Bool 
> allTrue [] = True
> allTrue (x:xs) = foldr (&&) (True) xs

> allFalse :: [Bool] -> Bool 
> allFalse [] = False
> allFalse (x:xs) = not (foldr (&&) (True) xs)



> member :: (Eq a) => a ->[a] -> Bool 
> member a  [] = False
> member a (x:xs)= if a==x then True 
>                  else foldr (==) a xs


> smallest :: [Int] -> Int
> smallest [] = error "Empty List"
> smallest [x] = x
> smallest (x:xs)= foldr min maxBound (x:xs)

> largest :: [Int] -> Int
> largest [] = error "Empty List"
> largest [x] = x
> largest (x:xs)= foldr max minBound (x:xs)
