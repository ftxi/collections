-- test2.hs

data Pair a b = Pair a b deriving (Show)

pairFst (Pair a _) = a
pairSnd (Pair _ b) = b

data QuadRuple a b = QuadRuple a a b b -- deriving (Show)

firstTwo :: QuadRuple a b -> [a]
firstTwo (QuadRuple a b c d) = [a, b]

lastTwo :: QuadRuple a b -> [b]
lastTwo (QuadRuple a b c d) = [c, d]

data Maybe' a = Nothing'
              | Just' a
              deriving (Show)

car' :: [a] -> Maybe' a
car' [] = Nothing'
car' (x:xs) = Just' x

data Tuple' a b c d = Tuple' a b c d deriving (Show)
tuple1 a = Tuple' (Just' a) Nothing' Nothing' Nothing'
tuple2 a b = Tuple' (Just' a) (Just' b) Nothing' Nothing'
tuple3 a b c = Tuple' (Just' a) (Just' b) (Just' c) Nothing'
tuple4 a b c d = Tuple' (Just' a) (Just' b) (Just' c) (Just' d)
firstElem  (Tuple' a _ _ _) = a
secondElem (Tuple' _ b _ _) = b
thirdElem  (Tuple' _ _ c _) = c
fourthElem (Tuple' _ _ _ d) = d

{-
data Tuple' a b c d = Tuple1 a
                    | Tuple2 a b
                    | Tuple3 a b c
                    | Tuple4 a b c d
                    deriving (Show)
firstElem  (Tuple1 a) = Just' a
secondElem (Tuple1 a) = Nothing'
...
-}

data List a = Nil
            | Cons a (List a)

-- treeElems t

