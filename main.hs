mult :: [Int] -> [Int] -> Int
mult [] [] = 0
mult (x:xs) (y:ys) = (x) * (y) + (mult xs ys)

finalGrade :: [Int] -> [Int] -> Int
finalGrade [] [] = 0
finalGrade xs ys
	| (length(xs) >1) = (top_frac) `div` (bottom_frac)
	where top_frac = mult xs ys
	      bottom_frac = sum(ys)
