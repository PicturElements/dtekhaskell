data Func = Sin | Cos
  deriving (Eq,Show)

data Expr = Num Double 
          | Var
          | Add Expr Expr
          | Mul Expr Expr
          | Fun Func Expr
  deriving (Eq,Show)

--instance Show Expr where
--  show = showExpr

testExpr = Mul (Add (Num 23.0) (Num 34.5)) (Add (Num 27.2) (Num 56.7))
testExpr2 = Mul (Add (Num 23.0) (Num 34.5)) (Add (Num 27.2) Var)
testExpr3 = Mul Var Var

showExpr :: Expr -> String
showExpr Var         = "x"
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Fun Sin e) = "sin (" ++ showExpr e ++ ")"
showExpr (Fun Cos e) = "cos (" ++ showExpr e ++ ")"

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

eval :: Expr -> Double -> Double
eval  Var x        = x
eval (Num n) x     = n
eval (Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Fun Sin e) x = sin (eval e x)
eval (Fun Cos e) x = cos (eval e x)
