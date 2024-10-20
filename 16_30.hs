import Data.Bifunctor;

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery list moduloIndex = fn list moduloIndex 1
    where
        fn [] _ _ = []
        fn (x:xs) moduloIndex currentIndex = [x | 0 /= (currentIndex `mod` moduloIndex)] ++ fn xs moduloIndex (currentIndex + 1)


-- Problem 17
split' :: String -> Int -> (String, String)
split' word 0 = ("", word)
split' (x:xs) splitAtIndex = first (x:) (split' xs (splitAtIndex - 1))

-- Problem 18
slice :: String -> Int -> Int -> String
slice [] _ _ = ""
slice (x:xs) 0 upperIndex 
    | upperIndex >= 0 = x : slice xs 0 (upperIndex - 1)
    | otherwise = ""
slice (_:xs) lowerIndex upperIndex
    | lowerIndex > 0 = slice xs (lowerIndex - 1) (upperIndex - 1)
    | otherwise = ""
    

-- Problem 19
rotate :: String -> Int -> String
rotate word val = (\(a, b) -> b ++ a) (fn word val)
    where 
        fn :: String -> Int -> (String, String)
        fn (x:xs) val
            | length xs == val = ("", xs)
            | length xs < val = (x:xs, "")
            | otherwise = first ([x]++) (fn xs val)


main :: IO ()
main = print (rotate ['a','b','c','d','e','f','g','h','i','k'] 20)