doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
	then x
	else x*2

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st , c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: (Int a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n-1)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b = a
	| otherwise = b

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<= x) xs) ++ [x] ++ quicksort (filter (> x) xs)

maximum' :: [a] -> a
maximum' [x] = x
maximum' (x:xs)
	| x > maximum' xs = x
	| otherwise = maximum' xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' _ [] = False
elem' a (x:xs) 
	| a == x = True
	| otherwise = elem' a xs

applyTwice f x = f (f x)

zipWidth' _ [] _ = []
zipWidth' _ _ [] = []
zipWidth' f (x:xs) (y:ys) = f x y : zipWidth' f xs ys
