main :: IO ()
main =
    putStrLn $ show sorted
    where sorted = mergeSort [2, 1, 4, 3, 5, 2, 3, 6, 1, 5, 7, 12 ]

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
merge [x] n
  | x > last n = n ++ [x]
  | x < head n = x : n
  | otherwise =
    concat expanded
        where
            expanded =  map (\l -> if head l > x then x : l else l ++ [x] ) leveled
            leveled = map (: []) n

merge  n  [x] = merge [x] n
merge left right =
    if head left < head right
        then head left : merge tailL right
        else head right : merge left tailR
                where
                    tailL = tail left
                    tailR = tail right
