main = do putStrLn "Welcome to MiniCalc!"
          putStrLn "Please enter a first number:"
          first <- getLine
          putStrLn "Pleas enter an operator:"
          operator <- getLine
          putStrLn "Please enter a second number:"
          second <- getLine
          putStrLn (first ++ " " ++ operator ++ " " ++ second ++ " = " ++ show (opr operator (read first ::Double) (read second ::Double)))
          main

opr :: Fractional a => String -> a -> a -> a
opr "+" = (+)
opr "-" = (-)
opr "*" = (*)
opr "/" = (/)
opr _ = error "Invalid operator!"