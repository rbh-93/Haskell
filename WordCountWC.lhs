Simplified verssion of wc command in Haskell

ghc --make WordCount.lhs

> module Main
> where
> import System.Environment

> main :: IO ()
> main = do
>   args <- getArgs
>   x <- mapM (\path -> do content <- readFile path; return (path, content)) args  
>   putStrLn $ wc x

> wc :: [(FilePath, String)] -> String
> wc [] = []
> wc ((path, content):files) = show(length (lines content)) ++ " " ++ show(length (words content)) ++ " " ++  show(length content) ++ " " ++ show (path) ++ "\n" ++ wc files   



   
