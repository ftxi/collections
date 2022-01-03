import Data.Set

infixl 5 &*
infix  8 &^

mmod :: Integral a => a -> a -> a
mmod a b = c where (_,c) = divMod a b

infix 7 `mmod`

class Eq a => Group a where
  (&*) :: a -> a -> a
  aneg :: a -> a
  aunit :: a
  aelems :: [a]

(&^) :: Group a => a -> Int -> a
g &^ i
  | i == 0 = aunit
  | i <  0 = aneg g &^ (- i)
  | otherwise = let loop x n
                      | n == 1    = x
                      | otherwise = loop (g &* x) (n-1)
                in loop g i

data G = G Int Int deriving (Show, Eq, Ord)

instance Group G where
  G m n &* G r s = G ((m+2^n*r) `mmod` 7) ((n+s) `mmod` 3)
  aneg (G m n) = G 0 (3-n) &* G (7-m) 0
  aunit = G 0 0
  aelems = [G m n | m <- [0..6], n <- [0..2]]

data H = H Int Int deriving (Show, Eq, Ord)

instance Group H where
  H m n &* H r s = H ((m+r) `mmod` 4) ((2^r*n+s) `mmod` 3)
  aneg (H m n) = H 0 (3-n) &* H (4-m) 0
  aunit = H 0 0
  aelems = [H m n | m <- [0..3], n <- [0..2]]

commutator :: (Group a, Ord a) => Set a
commutator = fromList [x &* y &* aneg x &* aneg y | x <- aelems, y <- aelems]

aorder :: Group a => a -> Int
aorder g = (+1) . length $ takeWhile (/= aunit) [g&^i | i <- [1..]]

gen_subgroup :: (Group a, Ord a) => Set a -> Set a
gen_subgroup s = let l0 = elems s
                     l = fromList $ [aneg x | x <- l0] ++ (aunit:l0)
                     muls t = [x &* y | x <- t, y <- t]
                     gen = fromList . muls . elems
                     loop x = if y == x then x else loop y
                       where y = gen x
                 in loop l

conj_class :: (Group a, Ord a) => a -> Set a
conj_class g = fromList [x &* g &* aneg x | x <- aelems]

conj_classes :: (Group a, Ord a) => Set (Set a)
conj_classes = fromList [conj_class g | g <- aelems]

main = do
  mapM (\x -> print (size x) >> print x) $ elems (conj_classes :: Set (Set G))
  return ()
