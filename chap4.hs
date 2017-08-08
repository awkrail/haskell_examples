maximum' :: (Ord a) => [a] -> a
maximum' [] = error "the list is empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> Int -> [Int]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n (x:xs)
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

--repeat' :: [a] -> [a]
--repeat' x = x : repeat' x

zip' :: [a] -> [a] -> [(a, a)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if x == a then True else False

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
	let smallerOrEqual = [a | a <- xs, a <= x]
	    greater = [a | a <- xs, a > x]
	in  quickSort smallerOrEqual ++ [x] ++ quickSort greater
