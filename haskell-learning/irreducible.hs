-- irreducible.hs

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}

infixl 7 -*-
infixl 6 .+
infixl 6 .-
infixl 7 .*

class Group a where
  (-*-) :: a -> a -> a
  ginv :: a -> a
  e :: a

instance Group Int where
  (-*-) = (+)
  ginv = negate
  e = 0

class Ring a where
  {-# MINIMAL (.+), (.*), fzero, funit, fneg #-}
  (.+) :: a -> a -> a
  (.*) :: a -> a -> a
  (.-) :: a -> a -> a
  fzero :: a
  funit :: a
  fneg :: a -> a

  {-# INLINE (.-) #-}
  {-# INLINE fneg #-}
  fneg x = fzero .- x
  x .- y = x .+ fneg y
  
class (Ring a) => Field a where
  finv :: a -> a

data GF2 = GF2_0 | GF2_1 deriving (Show, Eq)

instance Ring GF2 where
  GF2_0 .+ GF2_0 = GF2_0
  GF2_0 .+ GF2_1 = GF2_1
  GF2_1 .+ GF2_1 = GF2_0

  GF2_0 .* v = GF2_0
  GF2_1 .* v = v

  fneg GF2_0 = GF2_0
  fneg GF2_1 = GF2_1

  fzero = GF2_0
  funit = GF2_1

{-
instance Field GF2 where
  finv GF2_0 = error "attempt to find the inverse of GF2_0"
  finv GF2_1 = GF2_1
-}

data Polynomial a where 
  Polynomial :: (Ring a) => [a] -> Char -> Polynomial a

instance (Eq a) => Eq (Polynomial a) where
  (==) (Polynomial as _) (Polynomial bs _) = as == bs

instance (Show a) => Show (Polynomial a) where
  show (Polynomial (ah:ar) x)
    = show ah
      ++ concatMap (\(a, k) -> "+" ++ show a ++ x:'^':show k) (zip ar [1..])
  show (Polynomial [] _) = show (fzero::a)

polyx :: (Ring a) => [a] -> Polynomial a
polyx coeffs = Polynomial coeffs 'x'
  
instance (Num a) => Ring a where
  (.+) = (+)
  (.*) = (*)
  fneg = negate
  fzero = fromInteger 0
  funit = fromInteger 1
