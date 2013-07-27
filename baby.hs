maptup :: (a -> b) -> (a, a) -> (b, b)
maptup f (x, y) = (f x, f y)

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldl assign ([], [])
    where assign (t, f) x
              | p x       = (x:t, f) 
              | otherwise = (t, x:f)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = le ++ [x] ++ gt
    where (le, gt) = maptup qsort $ partition (<= x) xs
          
split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = (x:xs, y:ys)
    where (xs, ys) = split zs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x: merge xs (y:ys) 
    | otherwise = y: merge (x:xs) ys

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort zs = merge xs ys
    where (xs, ys) = maptup msort $ split zs
