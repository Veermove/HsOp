
main :: IO ()
main =
    print(encode "aaaabccaadeeee")

-- flatten
flatten :: [[Int]] -> [Int]
flatten = foldl (++) []

-- remove duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = compress Nothing

compress :: Eq a => Maybe a -> [a] -> [a]
compress Nothing lis = head lis : compress (Just (head lis)) (tail lis)
compress (Just prev) lis
    | null lis  = []
    | otherwise =
        if prev == head lis then
            compress (Just (head lis)) (tail lis)
        else
            head lis : compress (Just (head lis)) (tail lis)

-- group duplicates
groupDuplicates :: Eq a => [a] -> [[a]]
groupDuplicates give = reverse (groupB [] give [])

groupB :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
groupB current input output
    | null input   = current : output
    | null current = groupB [head input] (tail input) output
    | otherwise    =
        if head current == head input then
            groupB (head input : current) (tail input) output
        else
            groupB [head input] (tail input) (current : output)

-- Run-length encoding
encode :: Eq a => [a] -> [(Int, a)]
encode given = map (\ls -> (length ls, head ls)) (groupDuplicates given)
