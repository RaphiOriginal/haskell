data Student = Student { email :: String, grade :: Float } deriving Show

students = [(Student "raphael.brunner3@students.fhnw.ch" 5.2),
            (Student "peter.rudolfvonrohr@students.fhnw.ch" 4.2)]

-- High Order Functions 1
transform :: (a -> b) -> [a] -> [b]
transform _ [] = []
transform f (x:xs) = f x : transform f xs

squares :: [Int] -> [Int]
squares a = transform (\i -> i^2) a

emails :: [Student] -> [String]
emails a = transform email a


-- High Order Functions 2
keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep f (x:xs) | f x = x : keep f xs
              | otherwise = keep f xs

evens :: [Int] -> [Int]
evens a = keep even a

goodS :: [Student] -> [Student]
goodS a = keep (\x -> grade x > 5) a

-- High Order Functions 3
aggregate :: (a -> a -> a) -> a -> [a] -> a
aggregate _ a [] = a
aggregate f a (x:xs) = x `f` aggregate f a xs

sum' a = aggregate (+) 0 a

product' a = aggregate (*) 1 a

or' a = aggregate (||) False a

and' a = aggregate (&&) True a 