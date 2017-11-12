

foldr' op p xs = foldl (\k x-> k . op x) id xs p
