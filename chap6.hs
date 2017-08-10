import Data.List
import Data.Char
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length.nub

wordsCount :: String -> [(String, Int)]
wordsCount = map (\ws -> (head ws, length ws)).group.sort.words

-- needle and haystack
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- シーザ暗号
encode :: Int -> String -> String
encode offset msg = map (\x -> chr $ ord x + offset) msg 

decode :: Int -> String -> String
decode offset msg = encode (negate offset) msg

-- 各々の桁の和
digitSum :: Int -> Int
digitSum = sum.map digitToInt.show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

-- 辞書型
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v):xs)
    | k == key = Just v
    | otherwise = findKey key xs

-- 畳み込みで書き直す。明らかな再帰の場合は, 畳み込みを利用して書く
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldl (\acc (k, v) -> if k == key then Just v else acc) Nothing xs

-- Data.Mapについて
phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938"),
     ("bonnie", "452-2928"),
     ("patsy", "493-2928"),
     ("lucille", "205-2928"),
     ("wendy", "939-8282"),
     ("penny", "853-2492")
    ]

-- 文字列->数値
string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit


-- fromListWith
-- キー重複は許さないが, 二つ目の値にどうするか決められる
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ " ," ++ number2

-- 値の部分をリストで持たせることで, よりわかりやすく。
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs






















