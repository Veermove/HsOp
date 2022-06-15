
main :: IO ()
main =
    print sorted
    where sorted = quickSort [
                              2, 1, 4, 3, 5, 2, 3, 6, 1, 5, 7, 12,
                              2, 1, 4, 3, 5, 2, 3, 4, 4, 4, 4, 12,
                              3, 6, 1, 5, 7, 12, 2, 1, 4, 3, 5, 2
                             ]

quickSort :: [Int] -> [Int]
quickSort []  = []
quickSort [x] = [x]
quickSort listA  =
        quickSort leftList ++ eq ++ quickSort rightList
    where
        n = listA !! max 0 (div (length listA) 2)
        leftList  = filter (<n)  listA
        rightList = filter (>n)  listA
        eq        = filter (==n) listA
