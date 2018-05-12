-- abstract.hs
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}

infixl 6 &+
infixl 7 &*
infixl 6 $+
infixr 7 $*
  
class Group a where
  (&+) :: a -> a -> a
  
  aneg :: a -> a
  aneg x = aunit &- x
  
  (&-) :: a -> a -> a
  x &- y = x &+ aneg y
  
  aunit :: a


instance Group Integer where
  (&+) = (+)
  (&-) = (-)
  aunit = 0


class (Group a) => Ring a where
  (&*) :: a -> a -> a

  munit :: a

instance Ring Integer where
  (&*) = (*)
  munit = 1

class (Ring a) => Field a where
  mneg :: a -> a
  mneg = (munit &/)

  (&/) :: a -> a -> a
  x &/ y = x &* mneg y

instance Group Double where
  (&+) = (+)
  (&-) = (-)
  aunit = 0

instance Ring Double where
  (&*) = (*)
  munit = 1

instance Field Double where
  (&/) = (/)

class (Field a{-, Group v-}) => VectorSpace a v where
  ($*) :: a -> v -> v
  
  ($+) :: v -> v -> v
  --($+) = (&+)

{-
instance VectorSpace Double [Double] where
  k $* u = map (*k) u
  u $+ w = map (uncurry (+)) (zip u w)
-}

data Zero = Zero deriving (Show, Eq)

instance VectorSpace Double Zero where
  _ $* _ = Zero
  _ $+ _ = Zero

