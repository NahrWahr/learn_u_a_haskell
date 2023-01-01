main :: IO()
main = return()

divideByTen :: (Floating a) => a->a
divideByTen = (/10)

isUC :: Char -> Bool
isUC = (`elem` ['A'..'Z'])

applyTwice :: (a->a)->a->a
applyTwice f x = f (f x)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
flip' f y x = f x y

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallEnd = quicksort (filter (<=x) xs)
      greatEnd = quicksort (filter (>x) xs)
  in smallEnd ++ [x] ++ greatEnd

chain :: (Integral a) => a->[a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (3*n + 1)

longChain :: Int
longChain = length (filter cond (map chain [1..100]))
  where cond v = length v > 15

applyFnLst :: (Num a) => [(a->a)] -> [a] -> [a]
applyFnLst _ [] = []
applyFnLst [] _ = []
applyFnLst (f:fs) (x:xs) = f x : applyFnLst fs xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

oddSquareSum :: Integer
oddSquareSum = sum . map (^2) . take 1000 $ [1,2..]
