Caesar Cipher

Change only the standard alphabet letters in lower case (a to z) and upper case (A to Z).
Other symbols are simply returned again. 
Works for positive and negative integers.
The indexCheck makes sure that negative integers are made positive and then reduced (<26).


> module Char
> where
> import Data.Char
> import Data.List

> shift :: Int -> Char -> Char
> shift i x =
>   let 
>       uc = ['A'..'Z']
>       lc = ['a'..'z']
>       indexCheck = \r -> let o = (r `mod` 26) in if (o < 0) then ((o + 26) `mod` 26) else o
>       fu = \r -> uc!!(indexCheck (r + i))
>       fl = \r -> lc!!(indexCheck (r + i))
>   in maybe (maybe x fu (elemIndex x uc)) fl (elemIndex x lc)
