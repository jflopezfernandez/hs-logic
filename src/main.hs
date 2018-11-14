
divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

ldf :: Integral a => a -> a -> a
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

ld :: Integral a => a -> a
ld n = ldf 2 n

prime0 :: Integral a => a -> Bool
prime0 n | n < 1        = error "Not a positive integer"
         | n == 1       = False
         | otherwise    = ld n == n

listMax :: Integral a => [a] -> a
listMax [] = error "Empty list"
listMax [x] = x
listMax (x:xs) = max x (listMax xs)

printListMax :: (Integral a, Show a) => [a] -> IO ()
printListMax l = print $ listMax l

listRemoveFirstElement :: Integral a => a -> [a] -> [a]
listRemoveFirstElement _ [] = error "Empty list"
listRemoveFirstElement m [n] | m == n    = []
                             | otherwise = [n]
listRemoveFirstElement m (x:xs) | m == x = (xs)
                                | otherwise = listRemoveFirstElement m (xs)

l1 :: Integral a => [a]
l1 = [1,2,3,4,5,6,5,4,3,2,1]


main :: IO ()
main = do
    print $ l1
    let l2 = (listRemoveFirstElement 3 l1)
    print $ l2
