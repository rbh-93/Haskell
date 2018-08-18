Booleans in Haskell
Functions of type Bool -> Bool

> idBool :: Bool -> Bool
> idBool b = b

> negateBool :: Bool -> Bool
> negateBool b = not b

> alwaysTrue :: Bool -> Bool
> alwaysTrue b = True

> alwaysFalse :: Bool -> Bool
> alwaysFalse b = False

Functions of type (Bool, Bool) → Bool

AND 

> and' :: (Bool , Bool) -> Bool
> and' (x ,y) = if x == True && y == True then True else False

OR

> or' :: (Bool , Bool) -> Bool
> or' (True ,x)  = True
> or' (False, x) = x

XOR

> xor' :: (Bool , Bool) -> Bool
> xor' (True ,True)  = False
> xor' (True ,False)  = True
> xor' (False ,True)  = True
> xor' (False, False) = False

NAND

> nand' :: (Bool , Bool) -> Bool
> nand'(x, y)
>  |x == True && y == True  = False
>  |otherwise               = True


NOR

> nor' :: (Bool, Bool) -> Bool
> nor' (True, True) = False
> nor' (True, False) = False
> nor' (False, True) = False
> nor' (False, False) = True


XNOR

> xnor' :: (Bool, Bool) -> Bool
> xnor' (x, y) = case (x, y) of
>               (True, True) -> True
>               (True, False) -> False
>               (False, True) -> False
>               (False, False) -> True




