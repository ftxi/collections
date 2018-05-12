-- elimation.hs

type Symbol = [Char]

data Form = Lambda Symbol Form
          | Application Form Form
          | SymbolForm Symbol
          deriving (Eq)

instance Show Form where
  show (Lambda s f) = "λ" ++ s ++ ". " ++ show f
  show (Application f@(Lambda _ _) g)
    = "(" ++ show f ++ ") " ++ show g
  show (Application f g@(Application _ _))
    = show f ++ " (" ++ show g ++ ")"
  show (Application f g) = show f ++ " " ++ show g
  show (SymbolForm s) = s

-- instance Read Form where
  
