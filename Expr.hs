import Text.Read
import Data.Maybe

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
assocExpr = Add (Num 1) (Mul (Num 5)(Num 2))

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

type Parser a = String -> Maybe (a,String)

number :: Parser Double
number s = case (reads s :: [(Double,String)]) of
       [(d,str)] -> Just (d,str)
       []        -> Nothing

num :: Parser Expr
num s = case number s of
    Just (n,s1) -> Just (Num n, s1)
    Nothing     -> Nothing

factor ('(':s) = case expr s of
   Just(e,')':s1) -> Just (e, s1)
   _              -> Nothing
factor s       = num s    

term s = chain factor '*' Mul s
    
chain :: Parser a -> Char -> (a -> a -> a) ->  Parser a
chain p op f s = case p s of
  Just (ne,c:s1)
         |c == op -> case chain p op f s1 of
                              Just(e,s2) -> Just (f ne e, s2)
                              Nothing    -> Just (ne,c:s1)
  r               -> r

expr = chain term '+' Add 

readExpr :: String -> Maybe Expr
readExpr s = coreRead (remSpaces s "")
    where coreRead s = case expr s of
                        Just (e,"") -> Just e
                        _           -> Nothing

remSpaces :: String -> String -> String
remSpaces "" n                  = n
remSpaces (c:str) n | c == ' '  = remSpaces str n
                    | otherwise = remSpaces str (n++[c])
                    
fullEval :: String -> Double
fullEval s = case readExpr s of
            Just e  -> eval e 0
            Nothing -> 0/0      --Whoops! That's not a number...
