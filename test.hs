data A = B Int | C Int String
data D = E D | F
data G a = H a | I Int


-- 4
countChar :: Char -> String -> Int
countChar c s = foldr (\ch rs -> if c == ch then 1 + rs else rs) 0 s

exists :: (a -> Bool) -> [a] -> Bool
exists f xs = foldr (\a rs -> if f a then True else rs) False xs

main = do putStrLn "Hi, I'm Summy"
          loop 0

loop s = do putStrLn ("Current sum = " ++ (show s))
            newval <- getLine
            loop (s + (read newval))

-- 5
data HTML = Text String | Link String String | Table [[HTML]]

class ToHTML a where
    toHTML :: a -> String

instance ToHTML HTML where
    toHTML (Text s) = s
    toHTML (Link url b) = "<a href = '" ++ url ++ "'>" ++ b ++ "</a>"
    toHTML (Table hs) = foldr (\a rs -> a ++ rs) [] (map wrapTR hs)

wrapTD :: HTML -> String
wrapTD h = "<td>" ++ (toHTML h) ++ "</td>"

wrapTR :: [HTML] -> String
wrapTR hs =  "<td>" ++ (foldr (\a rs -> a ++ rs) [] (map wrapTD hs)) ++ "</td>"

table = Table [[Link "http://www.haskell.org" "Haskell", Text "ist cool"], [Text "Auf der JVM mag ich", Link "http://www.scala-lang.org" "Scala"]]
testLink = Link "http://www.haskell.org" "Haskell"
testText = Text "ist cool"