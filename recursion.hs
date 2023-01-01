main :: IO()
main = return()

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i->a->[a]
replicate' n x
  | n<=0 = []
  | otherwise = x:replicate' (n-1) x

-- take' n _
--   | n<=0 = []
-- take' _ [] = []
-- take' n (x:xs) = x : take' (n-1) xs
take' :: (Num i, Ord i, Eq a) => i -> [a] -> [a]
take' n v
  | n<=0 = []
  | v==[] = []
  | otherwise = x:take' (n-1) xs
  where (x:xs) = v

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a->[a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

ε :: (Eq a) => a -> [a] -> Bool
ε a [] = False
ε a (x:xs)
  | a==x = True
  | otherwise = ε a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a<-xs, a<=x]
      biggerSorted = quicksort [a | a<-xs, a>x]
  in smallerSorted ++ [x] ++ biggerSorted
