main :: IO ()
main =
    putStrLn $ show sorted
    where sorted = mergeSort [2, 1, 4, 3]

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort inp = merge (mergeSort left) (mergeSort right)
               where (left, right) = splitMid inp

splitMid :: [Int] -> ([Int], [Int])
splitMid inp = splitAt (div (length inp) 2) inp

merge :: [Int] -> [Int] -> [Int]
merge [ ] [ ] = [ ]
merge [x] [ ] = [x]
merge [ ] [x] = [x]
merge [x]  n  = if x > length (tail n) then n ++ [x] else x : n
merge  n  [x] = merge [x] n
merge left right =
    if head left < head right
        then head left : merge tailL right
        else head right : merge left tailR
                where
                    tailL = tail left
                    tailR = tail right
