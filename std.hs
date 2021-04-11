

and' [] = True
and' (False : _ ) = False

and' (x : xs) = and' xs



concat'  [] = []

concat' (x : xs) = x ++ concat' xs




replicate' 0 _ = []

replicate' n a = [a] ++ replicate' (n-1) a



index' 0 (x : xs) = x

index' n (x : xs) = index' (n-1) xs
 


elemh' _ [] False = False

elemh' _ _ True = True

elemh' a (x : xs) b = elemh' a xs (a == x)

elem' a (x : xs) = elemh' a xs (a == x)


reverse' [] = []

reverse' (x : xs) = x : reverse' xs
