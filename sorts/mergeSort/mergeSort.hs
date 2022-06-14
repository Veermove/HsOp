main :: IO ()
main =
    putStrLn $ show sorted
    where sorted = mergeSort [
                              2, 1, 4, 3, 5, 2, 3, 6, 1, 5, 7, 12,
                              2, 1, 4, 3, 5, 2, 3, 4, 4, 4, 4, 12,
                              3, 6, 1, 5, 7, 12, 2, 1, 4, 3, 5, 2
                             ]

mergeSort :: [Int] -> [Int]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort inp = merge (mergeSort left) (mergeSort right)
               where (left, right) = splitMid inp

splitMid :: [Int] -> ([Int], [Int])
splitMid inp = splitAt (div (length inp) 2) inp

merge :: [Int] -> [Int] -> [Int]
merge [ ] [ ] = [ ]
merge [x] [ ] = [x]
merge [ ] [x] = [x]
merge [x]  n  = inputAtPlace (Just x) n []
merge  n  [x] = merge [x]  n
merge left right =
    if head left < head right
        then head left : merge tailL right
        else head right : merge left tailR
            where
                tailL = tail left
                tailR = tail right

--              ToBeAdded    given    outBuilder
inputAtPlace :: Maybe Int -> [Int] ->    [Int]  -> [Int]
inputAtPlace n [] out = case n of
  Nothing -> out
  Just i -> out ++ [i]
inputAtPlace Nothing give out = inputAtPlace Nothing (tail give) (out ++ [head give])
inputAtPlace i give out = case i of
    Just i ->
        if i < head give then
            inputAtPlace Nothing (tail give) (out ++ [i] ++ [head give])
        else
            inputAtPlace (Just i) (tail give) (out ++ [head give])
    Nothing -> undefined
