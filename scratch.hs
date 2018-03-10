import Data.Char

square :: (Num a) => a -> a
square x = x * x

min' :: (Ord a) => a -> a -> a
min' x y 
    | x < y = x
    | otherwise = y
    
side = 12
area = square side 

-- | 1.1.1
quad :: (Num a) => a -> a
quad x = square (square x)

-- | 1.1.2
max' :: (Ord a) => a -> a -> a
max' x y
    | x > y = x
    | otherwise = y

-- | 1.1.3
areaCircle :: (Floating a) => a -> a
areaCircle r = pi * square r

-- | 1.4.3
sign :: (Real a) => a -> a
sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

-- | 1.5.1
-- | isSquare x specification: there exists an integer a such that a^2==x
-- | isSquare x = (square (intsqrt x) == x) is a suitable implementation

-- | 1.5.2
-- | intsqrt x specification: square (intsqrt x) =< x && square (intsqrt x + 1) > x

-- | 2.1.7
--deriv :: (Num a) => (a -> a) -> a -> a
deriv f x = (f (x + dx) - f x) / dx
    where dx = 0.001

--improve :: (Num a) => (a -> a) -> a -> a
improve f y = y - (f y / deriv f y)

--sat :: (Num a, Ord a) => (a -> a) -> a -> Bool
sat f y = abs (f y) < eps
    where eps = 0.001

until :: (Num b) => (b -> Bool) -> (b -> b) -> b -> b
until p u y 
    | p y = y
    | otherwise = Main.until p u (u y)

newton f = Main.until (sat f) (improve f)

sqrt' x = newton f x
    where f y = y^2 - x

sat' f x y = abs (f x y) < eps*x
    where eps = 0.001

sqrt'' x = Main.until (sat' f x) (improve (f x)) x
    where f x y = y^2 - x

sat'' f y' = abs (y - y') < eps * abs y
    where eps = 0.001
          y = improve f y'

newton''' f = Main.until (sat'' f) (improve f)
sqrt''' x = newton f x
    where f y = y^2 - x

-- | 2.2.1
sumsqrs :: (Num a, Ord a) => a -> a -> a -> a
sumsqrs x y z
    | (min x y) < z = square (max x y) + square z
    | otherwise = square x + square y

rat x y = (x `div` d, y `div` d)
    where d = gcd (abs x) (abs y)
          

-- | 2.4.1
age :: (Integral a) => (a, a, a) -> (a, a, a) -> a
age t b = yeardiff t b + cor
    where cor = if bdaypassed t b then 0 else -1

yeardiff t b = year t - year b
    where year (d, m, y) = y

bdaypassed t b = month t > month b || (month t == month b && day t >= day b)
    where month (d, m, y) = m
          day (d, m, y) = d

-- | 2.4.2
split x
    | abs x `mod` 10 > 5 = (sign x * (r - 10), sign x * (q + 1))
    | otherwise = (sign x * r, sign x * q)
    where r = abs x `mod` 10
          q = abs x `div` 10

-- | 2.5.1
and True y = y
and False y = False

or True y = True
or False y = y

and' x y = x && y
or' x y = x || y

-- | 3.2.3
countNeg :: (Real a) => [a] -> Int
countNeg xs = sum [1 | x <- xs, x < 0]

-- | 3.2.4
intpairs :: (Integral t) => t -> [(t,t)]
intpairs n = [(i, j) | i <- [1..n], j <- [1..n]]

-- | 3.2.5
ssQuad n = [(a, b, c, d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], 
            square a + square b == square c + square d]

-- | 3.2.6
pow x n = product [x | i <- [1..n]]

-- | 3.2.8
mindivisor n = head [d | d <- [2..n], n `mod` d == 0]

isprime n = mindivisor n == n

-- | 3.2.9
gcd' 0 b = b
gcd' a 0 = a
gcd' a b 
    | a < b = gcd' a (b `mod` a)
    | otherwise = gcd' b (a `mod` b)

positions :: (Eq a) => [a] -> a -> [Int]
positions xs x = [i | (y, i) <- zip xs [0..length xs -1], y == x]

position :: (Eq a) => [a] -> a -> Int
position xs x = head (positions xs x ++ [-1])

-- | 3.3.8
zip4 (ws, xs, ys, zs) = [((fst . fst) a, (snd . fst) a, snd a, b) | 
    (a, b) <- ws `zip` xs `zip` ys `zip` zs]

-- | 3.3.9
trips xs = 
    [(fst x, snd x, y) | (x, y) <- xs `zip` (tail xs) `zip` (tail . tail) xs]

-- | 3.3.10
riffle xs = interleave os es
    where os = filter' isOdd xs
          es = filter' isEven xs
          isOdd x = x `mod` 2 == 1
          isEven x = not (isOdd x)

interleave xs ys = (concat . transpose) [xs, ys]

transpose [xs, ys] = [[x, y] | (x, y) <- xs `zip` ys]

filter' p xs = [x | x <- xs, p x]

-- | 3.3.12
minus :: (Eq a) => [a] -> [a] -> [a]
minus xs ys = [x | x <- xs, not (elem x ys)]

score cd gs = show bull ++ show cow
    where bull = sum [1 | (x, y) <- zip cd gs, x == y]
          cow = length cd - length (minus cd gs) - bull

-- | [x*x | x <- xs, even x] = map square (filter even xs)
-- | [x | xs <- xss, x <- xs] = concat xss
-- | [(i,j) | i <- [1..n], j <- [i+1..n]] = concat [[(i,j) | j <- [i+1..n]]] | i <- [1..n]]

-- | 3.4.1
filter p = concat . map box
    where box x = if (p x) then x:[] else []

-- | 3.4.2 
mapMap :: [a -> b] -> [[a] -> [b]]
mapMap = map map

-- | 3.4.3
-- [x | xs <- xss, x <- xs, odd x] =
-- concat [[x | x <- xs, odd x] | xs <- xss] =
-- concat [[x | x <- filter odd xs] | xs <- xss] =
-- concat [filter odd xs | xs <- xss] =
-- concat (map (filter odd) xss)
--
-- [(x,y) | x <- xs, p x, y <- ys] =
-- concat [[(x,y) | y <- ys] | x <- xs, p x] =
-- concat [ map (pair x) ys | x <- xs, p x] = 
-- concat [ map (pair x) ys | x <- filter p xs] = 
-- concat (map g (filter p xs))
--  where g x = map (pair x) ys

-- | 3.4.4
-- First one generates 10x100 = 1000 pairs
-- Second one generates 1000x100 = 100000 pairs

pack :: [Int] -> Int
pack xs = foldl op 0 xs
    where op n x = 10*n + x

-- | 3.5.1
all p xs = foldr (\x z -> (p x) && z) True xs

-- | 3.5.4
insert :: (Ord a, Num a) => a -> [a] -> [a]
insert x xs = takewhile (<= x) xs ++ [x] ++ dropwhile (<= x) xs

takewhile :: (a -> Bool) -> [a] -> [a]
takewhile _ [] = []
takewhile p xs 
    | p h = h : takewhile p (tail xs)
    | otherwise = []
    where h = head xs

dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile _ [] = []
dropwhile p xs
    | p (head xs) = dropwhile p (tail xs)
    | otherwise = xs


isort xs = foldr insert [] xs

-- | 3.5.5
remdups xs = foldr f [] xs
    where f x [] = [x]
          f x z = if head z == x then z else x:z

-- | 3.5.6
ssm xs = foldl f [] xs
    where f [] x = [x]
          f z x = if last z < x then z ++ [x] else z

-- | 3.5.7
-- (foldl plus a) . map f =
-- (foldr plustilde a) . reverse . map f = 
-- (foldr plustilde a) . map f . reverse =
-- ((foldr plustilde a) . map f) . reverse =
-- (foldr timestilde a) . reverse =
-- foldl times a

-- | 3.5.8
foldr1 f xs = foldr f (last xs) (init xs)

scan1 f xs = scanl f (head xs) (tail xs)

-- | 3.5.9
e n = sum (scanl (/) 1 [1..n])

-- | 3.6.4
swap x (hd:tl)
    | hd < x = hd:x:tl
    | otherwise = x:hd:tl

insert' x = foldr swap [x]

isort' xs = foldr insert' [] xs


-- | Variable length arithmetic
align xs ys
    | n > 0 = (xs, copy 0 n ++ ys)
    | otherwise = (copy 0 (-n) ++ xs, ys)
    where n = length xs - length ys

copy x n = [x | i <- [1..n]]

vcompare :: ([Int] -> [Int] -> Bool) -> [Int] -> [Int] -> Bool
vcompare op xs ys = op us vs
    where (us, vs) = align xs ys

veq = vcompare (==)
vge = vcompare (>=)
vgt = vcompare (>)
vle = vcompare (<=)
vlt = vcompare (<)

carry b x (c:xs) = (x + c) `div` b : (x + c) `mod` b : xs

norm :: Int -> ([Int] -> [Int])
norm b = strep . foldr (carry b) [0]
norm' b = strep . (edgecarry b) . foldr (carry b) [0]
edgecarry b (hd:tl) 
    | hd >= b = (norm b (padzero ++ [hd])) ++ tl
    | otherwise = hd:tl
    where padzero = copy 0 (hd - b + 1)


strep xs 
    | ys == [] = [0]
    | otherwise = ys
    where ys = dropwhile (==0) xs

vadd b xs ys = norm' b (zipWith (+) (fst t) (snd t))
    where t = align xs ys
vsub b xs ys = norm' b (zipWith (-) (fst t) (snd t))
    where t = align xs ys

negative xs = head xs < 0
negate b = (norm b) . map neg
neg x = -x

psums b xs ys = map (bmul b xs) ys
bmul :: Int -> [Int] -> Int -> [Int]
bmul b xs y = (norm b) . map (*y) $ xs

vmul b xs ys = foldl f [0] (psums b xs ys)
    where f us vs = vadd b (us ++ [0]) vs

-- | 4.2.1
absint :: [Int] -> Int
absint xs
    | negative ys = -absint (-(head ys):tail ys)
    | otherwise = pack ys
    where ys = strep xs

-- | 4.2.5
digit c = Data.Char.ord c - Data.Char.ord '0'

vpack b = foldl op []
    where op z x = vadd b (z ++ [0]) [x]

dtob d b = norm' b [d]

inv b = vpack b . map digit

-- | 5.3.1
(!) (x:xs) n 
    | n > 0 = (!) xs (n - 1)
    | otherwise = x
(!) _ _ = undefined

-- | 5.3.2
takewhile' _ [] = []
takewhile' p (x:xs)
    | p x = x:takewhile p xs
    | otherwise = []

dropwhile' _ [] = []
dropwhile' p (x:xs)
    | p x = dropwhile p xs
    | otherwise = x:xs

-- | 5.3.10
interval3 a b c 
    | (b + step >= min b c) && (b + step <= max b c) = a:interval3 b (b + step) c
    | otherwise = [a, b]
    where step = b - a

interleave' x [] = [[x]]
interleave' x (y:ys) = (x:y:ys) : map (y:) (interleave' x ys)

-- | 5.6.1
prefixes [] = []
prefixes xs = [xs] ++ prefixes (init xs)
segs [] = [[]]
segs xs = prefixes xs ++ segs (tail xs)

-- | 5.6.2
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs

