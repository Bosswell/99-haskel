-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery list moduloIndex = fn list moduloIndex 1
    where
        fn [] _ _ = []
        fn (x:xs) moduloIndex currentIndex = [x | 0 /= (currentIndex `mod` moduloIndex)] ++ fn xs moduloIndex (currentIndex + 1)

main :: IO ()
main = print (dropEvery [1, 2, 3, 4, 5, 6, 7] 4)