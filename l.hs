multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

cmpHundred :: Int -> Ordering
cmpHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b ->c) -> (b -> a -> c)
flip' f x y = f y x

largestDiv :: Integer
largestDiv = head (filter p [99999,99998..])
    where p x = mod 3829 x == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3 + 1)

numLongChains :: Int
-- numLongChains = length [x | x <- [length (chain y) | y <- [1..100]], x > 15]
-- numLongChains = length (filter (>15) (map length [chain x | x <- [1..100]]))
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc y -> acc + y) 0 xs
