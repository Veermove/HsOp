
main :: IO ()
main =
    print( decode $ encode "aaaabccaadeeee")

-- flatten
flatten :: [[Int]] -> [Int]
flatten = foldl (++) []

-- remove duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = compress Nothing

compress :: Eq a => Maybe a -> [a] -> [a]
compress Nothing lis = head lis : compress (Just $ head lis) (tail lis)
compress (Just prev) lis
    | null lis  = []
    | otherwise =
        if prev == head lis then
            compress (Just (head lis)) (tail lis)
        else
            head lis : compress (Just $ head lis) (tail lis)

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

data Element a = Paired Int a | Single a deriving (Show)

-- Run-length encoding
encode :: Eq a => [a] -> [Element a]
encode given = map (\ls -> 
    if length ls == 1 then
        Single $ head ls 
    else 
        Paired (length ls)  (head ls)
    ) (groupDuplicates given)

decode :: Eq a => [Element a] -> [a]
decode given = reverse $ decodeOne [] given

decodeOne :: Eq a => [a] -> [Element a] -> [a]
decodeOne cont []                    = cont
decodeOne cont (Single val : ta)     = decodeOne (val : cont) ta
decodeOne cont (Paired num val : ta) = 
    if num == 0 then
        decodeOne cont ta
    else
        decodeOne (val : cont) (Paired (num - 1) val : ta)
