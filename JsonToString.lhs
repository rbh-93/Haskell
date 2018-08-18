JSON to String Program



> module JSON
> where

> data JValue = 
>    JString String
>    | JNumber Integer
>    | JObject JMembers
>    | JArray [JValue]
>    | JBool Bool
>    | JNull
>   deriving (Eq)

> instance Show JValue where
>   show (JString str) = show str
>   show (JNumber num) = show num
>   show (JBool bool) = if (bool) then "True" else "False"
>   show (JNull) = "Null"
>   show (JArray array) = show array
>   show (JObject x) = "{" ++ (show x) ++ "}"


> data JMembers = JMember [(String, JValue)]
>     deriving (Eq)

> instance Show JMembers where
>   show (JMember []) = ""
>   show (JMember ((a,b):[])) = ((show a) ++ " : " ++ (show b))
>   show (JMember ((a,b):xs)) = ((show a) ++ " : " ++ (show b) ++ " , " ++ (show (JMember xs)))


> format :: JValue -> String
> format x = show x

