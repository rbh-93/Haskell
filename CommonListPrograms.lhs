> import Data.Bool


A function that determines whether every element of a list of Booleans is true.

> allTrue :: [Bool] -> Bool 
> allTrue [] = True
> allTrue (x:xs) = x && allTrue xs


A function that determines whether every element of a list of Booleans is false.

> allFalse :: [Bool] -> Bool 
> allFalse [] = False
> allFalse (x:xs) = not (x || allFalse xs)


A function that determines whether a given element is contained in a given list.

> member :: (Eq a) => a ->[a] -> Bool 
> member y  [] = False
> member a (y:ys)=if a==y then True
>                 else  member a  ys


A function that calculates the smallest value in a list of integers.

> smallest :: [Int] -> Int
> smallest [k] = k
> smallest (k:ks)=if k<=smallest(ks) then k
>                else smallest ks

A function that similarly calculates the largest value in a list of integers.

> largest :: [Int] -> Int
> largest [k] = k
> largest (k:ks)=if k>=largest(ks) then k
>                else largest ks