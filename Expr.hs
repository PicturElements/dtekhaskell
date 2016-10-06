data Func = Sin | Cos

data Expr = Num Double 
          | Var
          | Add Expr Expr
          | Mul Expr Expr
          | Fun Func Expr
  deriving (Eq,Show)

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e
