To check String equality ignoring case

> equals :: [Char] -> [Char] -> Bool
> equals x y = if (map toLower x) == (map toLower y) then True else False

To check if a String consists solely of digits

> isNumeral :: [Char] -> Bool
> isNumeral x = if and (map isDigit x) == True then True else False

To check if a String consists solely of whitespaces

> isBlank:: [Char]  -> Bool
> isBlank x = if  and (map isSpace x) == True then True else False