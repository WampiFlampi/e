facs n | n == 1     = [1]
       | n > 1      = n*head ns : ns
                      where
                       ns = facs(n-1)


cs = [x^2 | x <- [100..115]]

ls = [x^3 | x <- [20..30]]

fs = reverse (facs 15)

es = [2*x | x <- [1..10]]
os = [2*x - 1 | x <- [1..10]]

merge xs [] = xs
merge [] ls  = ls

merge (x : xs) ls = less ++ [x] ++ merge xs more
                    where
                        less = [a | a <- ls, a <= x]
                        more = [b | b <- ls, b > x ]


msplit :: [Integer] -> ([Integer],[Integer])
msplit xs = (as,bs)
            where 
                m = toInteger (div (length xs) 2)
                as = [a | (a,i) <- zip xs [1..], i <= m]
                bs = [b | (b,i) <- zip xs [1..], i > m ]

 
                  

