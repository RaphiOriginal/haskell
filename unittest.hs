import Test.HUnit

length' [] = 0
length' (_:xs) = 1 + length' xs

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = \f g -> g.f

lengthEmpty = TestCase (assertEqual "for: lengt' []" 0 (length' []))
lengthSingelton = TestCase (assertEqual "for: length [1]" 1 (length' [1]))
lengthTwoElements = TestCase (assertEqual "for: length [1,2]" 2 (length' [1,2]))

pipeTest = TestCase(assertEqual "for: pipe = (fst |> (*2) |> even" True ((fst |> (*2) |> even) (3,"Three")))

lengthTests = TestList [lengthEmpty, lengthSingelton, lengthTwoElements, pipeTest]

main = runTestTT lengthTests