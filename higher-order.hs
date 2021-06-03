ls = [x^2 | x <- [0..13]]


resolve e ls | e == m    = 0
             | e > m     = 1
             | otherwise = 2
               where
                 m = ls!!(div (length ls) 2)

f1 z x y = x^2+y^2 == z^2

pyth n = [(x,y,z) | z <- [1..n],x <- [1..z-1],y <- [x..z-1] ,f1 z x y]
                                                             
c a b = a*b

f0 = c 10



length1 :: Foldable t => t a -> Int

length1 = foldr (\x n -> 1+n) 0

-- r = foldr (\x xs -> xs++[x] ) []

-- f = foldr (\x n -> (x^2):n) []


f p x xs | p x       = x : xs
         | otherwise = xs

filter1 p = foldr c []
     where
        c = f p

