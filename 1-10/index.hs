
import Data.Bifunctor

-- Problem 1
myLast :: Eq a => [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
lastTwo :: [a] -> [a]
lastTwo [] = []
lastTwo [x] = [x]
lastTwo [x, y] = [x, y]
lastTwo (x:xs) = lastTwo xs

-- Problem 3
at' :: Int -> [a] -> Maybe a
at' _ [] = Nothing
at' 0 (x:_) = Just x
at' p (x:xs) = at' (p - 1) xs

-- Problem 4
length' :: [a] -> Int
length' = foldr (\_ acc -> acc + 1) 0

length'' :: [a] -> Int
length'' xs = go xs 0
  where
    go [] result = result
    go (x:xs) result = go xs (result + 1)

-- Problem 5
-- Reverse

-- Problem 6
-- Palindrome

data NestedList a = Elem a | List [NestedList a]

-- Problem 7
toFlat :: NestedList a -> [a]
toFlat (Elem a) = [a]
toFlat (List a) = foldr (\elem acc -> toFlat elem ++ acc) [] a

-- Problem 8
compress :: String -> String
compress [] = []
compress (a:b:rest) 
  | a == b = compress (b : rest)
  | otherwise = a : compress (b : rest)
compress (a:_) = [a]

compress' :: String -> String
compress' (x:xs@(y:_))
  | x == y = compress' xs
  | otherwise = x : compress' xs
compress' xs = xs

compress'' :: Eq a => [a] -> [a]
compress'' (x:xs@(y:_))
  | x == y = compress'' xs
  | otherwise = x : compress'' xs
compress'' xs = xs


-- Problem 9

type Group = String
type Rest = String

group' :: String -> (Group, Rest)
group' [] = ("", "")
group' [x] = ([x], "")
group' (x:xs@(y:_))
  | x == y = Data.Bifunctor.first (x:) (group' xs)
  | otherwise = ([x], xs)

pack' :: String -> [String]
pack' "" = []
pack' str = fst (group' str) : pack' (snd (group' str))

main :: IO ()
-- main = print (toFlat (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
main = print (pack' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e'])