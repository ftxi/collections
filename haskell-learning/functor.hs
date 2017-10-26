
data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  fmap f (Just' a) = Just' $ f a
  fmap _ Nothing' = Nothing'

-- let (Just x) = fmap (+1) (Just 3) in x

