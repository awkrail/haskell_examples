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