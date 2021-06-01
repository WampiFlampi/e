ls = [x^2 | x <- [0..13]]


resolve e ls | e == m    = 0
             | e > m     = 1
             | otherwise = 2
               where
                 m = ls!!(div (length ls) 2)

f z x y = x^2+y^2 == z^2

pyth n = [(x,y,z) | z <- [1..n],x <- [1..z-1],y <- [x..z-1] ,f z x y]
                                                             
c a b = a*b

f0 = c 10
