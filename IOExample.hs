import Network.HTTP
import System.Environment

url = "http://www.omdbapi.com/?t="

buildUrl :: String -> String
buildUrl s = url ++ s

main = do args <- getArgs
          let request = buildUrl (head args)
          response <- simpleHTTP (getRequest request)
          body <- getResponseBody response
          putStrLn body