fs n | n == 1     = [1]
     | n > 1      = n*head ns:ns
                    where
                      ns = fs(n-1)

e n = sum(map (1/) (fs n)) + 1  
