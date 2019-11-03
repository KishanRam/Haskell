-- By the Newton Raphson method, given an initial choice x[0], as the "root" of
-- a (differentiable, etc) function f:R->R, we can compute a root of the
-- equation: f(x) = 0 by:
-- 
-- x[n+1] = x[n] - f(x)/f'(x)
--
-- Consider the function nr, defined as:
--
nr :: Fractional a => (a -> a) -> (a -> a) -> a -> a
nr f df x = x - (f x)/(df x)

-- With:
f x = (x*x - 2)
df x = 2*x

-- Definition of iterate in prelude:
-- iterate :: (a->a) -> a -> [a]
-- iterate f x = x : iterate f (f x)

-- *Main> take 10 (iterate (nr f df) 3.0)
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977,1.4142135623731118,1.414213562373095,1.4142135623730951,1.414213562373095,1.4142135623730951]
-- *Main> 

iterateCond :: (a -> a) -> (a -> a-> Bool) -> a -> [a]
iterateCond f cond x = if (cond x (f x)) then [] else (x: iterateCond f cond (f x))

-- *Main> :l Newton-Raphson.hs 
-- [1 of 1] Compiling Main             ( Newton-Raphson.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.1) 3.0
-- [3.0,1.8333333333333333]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.01) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.0001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.00001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.0000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.00000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.000000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.0000000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.00000000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.000000000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.0000000000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977]
-- *Main> iterateCond (nr f df) (\x y -> abs (x - y) < 0.00000000000001) 3.0
-- [3.0,1.8333333333333333,1.4621212121212122,1.4149984298948028,1.4142137800471977,1.4142135623731118]
-- *Main> 

foot :: [a] -> a
foot [] = error "Need non-empty list!"
foot [x] = x
foot (x:xs) = foot xs

findRoot f df initialGuess tolerance = foot (iterateCond (nr f df) (\x y -> abs (x - y) < tolerance) initialGuess)

-- *Main> findRoot f df 3.0 0.00000000000001
-- 1.4142135623731118
-- *Main>

-- For the record!
-- *Main> sqrt 2
-- 1.4142135623730951
-- *Main> 
