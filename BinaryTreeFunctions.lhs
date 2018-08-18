> module BinaryTree
> where

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

> instance Functor Tree where
>   fmap _f Empty         =  Empty
>   fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

--------------------------------------------------------------------------------
Size of a BinaryTree

> size :: Tree elem -> Int
> size Empty = 0

> size (Node l a r) = size l + 1 + size r 

--------------------------------------------------------------------------------
Minimum and Maximum Height of a BinaryTree


> minHeight, maxHeight :: Tree elem -> Int
> minHeight Empty = 0
> minHeight (Node l a r) = (min (minHeight l) (minHeight r)) + 1


> maxHeight Empty = 0
> maxHeight (Node l a r) = max(maxHeight l) (maxHeight r) + 1
 

--------------------------------------------------------------------------------
Function to determine if an element is is a member of a BinaryTree


> member :: (Eq elem, Ord elem) => elem -> Tree elem -> Bool
> member k Empty = False
> member k (Node l a r)
>  | k < a = member k l
>  | k == a = True
>  | k > a = member k r

--------------------------------------------------------------------------------
Traversals in a BinaryTree in linear time

> postorder :: Tree elem -> [elem]
> postorder tree = traverse tree []
>    where
>       traverse Empty = id
>       traverse (Node l a r) = traverse l . traverse r . (a:)

> preorder :: Tree elem -> [elem]
> preorder tree = traverse tree []
>    where
>       traverse Empty = id
>       traverse (Node l a r) = (a:). traverse l . traverse r 

> inorder :: Tree elem -> [elem]
> inorder tree = traverse tree []
>    where
>       traverse Empty = id
>       traverse (Node l a r) =  traverse l . (a:). traverse r 


