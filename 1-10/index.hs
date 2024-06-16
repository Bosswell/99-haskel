
import Data.Bifunctor
import Data.ByteString (count)

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
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Problem 6
isPalindrome :: [Char] -> Bool
isPalindrome a = reverse' a == a

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

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

-- Problem 10
encode :: String -> [(Int, Char)]
encode str = fmap (\grouped -> (length grouped, head grouped)) (pack' str)

-- Problem 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: String -> [ListItem Char]
encodeModified = map encodeGroup . encode
  where 
    encodeGroup (1, b) = Single b
    encodeGroup (a, b) = Multiple a b

-- Problem 12
replicate' :: Int -> a -> [a]
replicate' 1 elem = [elem]
replicate' replicatesCount elem = elem:(replicate' (replicatesCount - 1)  elem)

decodeModified :: [ListItem Char] -> String
decodeModified xs = concat $ map unwrap xs
  where
    unwrap (Single b) = [b]
    unwrap (Multiple a b) = replicate' a b

main :: IO ()
main = print (decodeModified $ encodeModified ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])