type Person = (String, String, Int)

name :: Person -> String
name (n, p, a) = n

phone :: Person -> String
phone (n, p, a) = p

age :: Person -> Int
age (n, p, a) = a

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

teacher :: Person
teacher = ("Daniel Kröni", "056 202 78 17", 35)

me :: Person
me = ("Raphael Brunner", "079 285 78 62", 26)