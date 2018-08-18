> module Calculus
> where
> import Data.Ratio

> data Primitive
>   =  Sin  -- trigonometric: sine
>   |  Cos  -- trigonometric: cos
>   |  Exp  -- exponential
>   |  Ln  -- logarithm
>   deriving (Show)
>

> data Function
>   =  Const Rational         -- constant function
>   |  Id                     -- identity
>   |  Prim Primitive         -- primitive function
>   |  Function :+: Function  -- addition of functions
>   |  Function :*: Function  -- multiplication of functions
>   |  Function :.: Function  -- composition of 
>   |  Function :-: Function  -- subtraction of functions
>   |  Function :/: Function  -- division of functions
>   |  Function :^: Function  -- power of functions
>   deriving (Show)

> (*^*) :: (RealFrac a, RealFrac b) => a -> b -> a
> x *^* y = realToFrac $ realToFrac x ** realToFrac y

> infixl 6 :+:
> infixl 6 :-:
> infixl 7 :*:
> infixl 7 :/:
> infixr 8 :^:
> infixr 9 :.:

--------------------------------------------------------------------------------
Function to apply different operations


> applyOp :: Function -> (Double -> Double)
> applyOp Id = \x -> x
> applyOp (Const y) = \x -> fromRational(y)
> applyOp (Prim Sin) = \x -> sin x
> applyOp (Prim Cos) = \x -> cos x
> applyOp (Prim Exp) = \x -> exp x
> applyOp (Prim Ln) = \x -> log x
> applyOp (f :+: g) = \x -> ((applyOp f) x) + ((applyOp g) x)
> applyOp (f :-: g) = \x -> ((applyOp f) x) - ((applyOp g) x)
> applyOp (f :*: g) = \x -> ((applyOp f) x) * ((applyOp g) x)
> applyOp (f :/: g) = \x -> ((applyOp f) x) * ((applyOp g) x)
> applyOp (f :^: g) = \x -> ((applyOp f) x) ** ((applyOp g) x)
> applyOp (f :.: g) = \x -> (applyOp f) ((applyOp g) x)

--------------------------------------------------------------------------------
Implementing derivatives

> derive :: Function -> Function
> derive Id = (Const 1)
> derive (Const y) = (Const 0)
> derive (Prim Sin) = (Prim Cos)
> derive (Prim Cos) = (Const 0) :-: (Prim Sin)
> derive (Prim Exp) = (Prim Exp)
> derive (Prim Ln) = (Const 1) :/: Id
> derive (f :+: g) = (derive f) :+: (derive g)
> derive (f :-: g) = (derive f) :-: (derive g)
> derive (f :*: g) = (f :*: (derive g)) :+: ((derive f) :*: g)
> derive (f :/: g) = (((derive f) :*: g) :-: (f :*: (derive g))) :/: (g :^: (Const 2)) 
> derive (f :^: g) = (f :^: g) :*: (((derive f) :*: (g :/: f)) :+: ((derive g) :*: ((Prim Ln) :.: f)))
> derive (f :.: g) = ((derive f) :.: g) :*: (derive g)


