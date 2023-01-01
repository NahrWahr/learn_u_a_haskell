main :: IO()
main = return()

length' :: (Integral b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Integral a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "Spooky scary SKELETON"
  | bmi <= 25.0 = "failed p__is inspection"
  | bmi <= 30.0 = "Oink Oink"
  | otherwise = "Gojira Moment"
  where bmi = weight / height ^ 2

cylinder :: (RealFloat a) => a->a->a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w/h^2, bmi>=25.0]

test xs = "DING" ++ case xs of "DONG" -> " BING"
                               "DING" -> " BONG"
