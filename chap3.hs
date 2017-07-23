--{-# OPTIONS -Wall -Werror #-}
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck, pal!"

-- between 1 and 5
sayMe :: Int->String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

-- 階乗
factorial :: Int->Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- tuple
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

-- tripleでのvectorのとりだし
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- リストでのパターンマッチ
xs = [(1, 3.5), (4.1, 3), (2, 4), (5, 3), (5, 6), (3, 1)]
addList :: [(Double, Double)] -> [Double]
addList lis = [a+b | (a,b) <- lis]

-- head'
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- tell
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two element: " ++ show x ++ " and" ++ show y
tell (x:y:_) = "The list is too long to show you. the first two are" ++ show x ++ "and" ++ show y

-- as pattern
firstLetter :: String -> String 
firstLetter "" = "Empty string, wow!"
firstLetter all@(x:xs) = "The first letter of" ++ all ++ "is" ++ [x] --文字列もリストだから, [x]としてリストの後ろに追加する

-- bmi guard
bmiTell :: Double -> String
bmiTell bmi
	| bmi <= 18.5 = "You're underweight, you emo, you!"
	| bmi <= 25.0 = "You're supposedly normal. \
						\ Pffft, I bet you're urgly!"
	| bmi <= 30.0 = "You're fat!"
	| otherwise = "You're whale!"

fibo :: Int->Int
fibo n
	| n < 0 = error "Wrong number"
fibo 0 = 1
fibo 1 = 1
fibo n = fibo(n-1)+fibo(n-2)

-- max and compare
max' :: (Ord a) => a->a->a
max' a b
	| a <= b 	=	b
	| otherwise =	a

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
	| a == b = EQ
	| a <= b = LT
	| otherwise = GT

-- bmi with where
bmiTell' :: Double -> Double -> String
bmiTell' weight height
	| bmi <= 18.5 = "You're underweight, you emo, you!"
	| bmi <= 25.0 = "you're supposedly normal"
	| bmi <= 30.0 = "You're fat"
	| otherwise = "You're whale"
	where bmi = weight / height ^ 2

-- パターンマッチとwhere
-- whereの束縛の中でもパターンマッチできる
--initials :: String -> String -> String
--initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
--	where (f:_) = firstname
--		  (l:_) = lastname

-- whereブロックの中の関数
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height^2





