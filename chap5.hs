-- カリー化関数について
multThree :: Int -> Int -> Int -> Int
multThree x y z = x*y*z

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
flip' f = g
    where g x y = f y x

-- map関数
map' :: (a->a)->[a]->[a]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter関数
filter' :: (a->Bool)->[a]->[a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter f xs
    | otherwise = filter f xs

-- largest
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
    where p x = mod x 3829 == 0

-- collaz
collatz :: Int->[Int]
collatz 1 = [1]
collatz n
    | mod n 2 == 0 = n : collatz (div n 2)
    | mod n 2 /= 0 = n : collatz (3*n+1)

-- collaz + 15列
numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..100]))
    where isLong xs = length xs > 15

-- lambda
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- foldl
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> x + acc) 0 xs

-- carry and foldl
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- map foldr
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- map foldl
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- elem'
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- fold1
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

-- reverse
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- product
product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- filter
filter'' :: (a->Bool)->[a]->[a]
filter'' f = foldl (\acc a -> if f a then acc ++ [a] else acc) []

-- and
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- sqrt sum < 1000
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- chap5 last
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum.takeWhile (<10000) $ filter odd $ map (^2) [1..]



