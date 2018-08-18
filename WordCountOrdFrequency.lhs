A non-recursive program that computes the words of a given text, ordered by frequency of occurrence.


> module WordList
> where
> import Prelude hiding (Word)
> import Data.Char
> import Data.List
> import System.IO
> type Word  =  String

> wordList :: String -> [(Word, Int)]
> wordList str = sortOn snd (map (\l -> (head l, length l)) (group (sort (words (filter (\x -> isLower x || isSpace x)(map toLower  str) )))))