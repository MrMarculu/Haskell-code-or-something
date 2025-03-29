data Expr = Const Double 
            | Var String 
            | Sum Expr Expr
            | Mul Expr Expr
            | Exp Expr Expr 
            | Log Expr
            | Sin Expr
            | Cos Expr
            | Rec Expr 
            | Neg Expr 
            | Euler 
    deriving (Eq)

instance Num Expr where 
    (+) = Sum 
    a-b = Sum a (Neg b)
    (*) = Mul 
    negate = Neg 
    fromInteger i = Const (fromInteger i) 

instance Fractional Expr where 
    a / b = Mul a (Rec b) 
    fromRational a = Const (fromRational a)

instance Floating Expr where 
    pi = Const pi 
    exp = Exp Euler 
    sqrt = Exp (1/2)
    log = Log 
    sin = Sin 
    cos = Cos 
    (**) = Exp 

instance Show Expr where 
    show (Const i) = show i 
    show (Var v) = v 
    show (Mul a b) = show a ++ "*" ++ show b 
    show (Sum a b) = show a ++ "+" ++ show b 
    show (Neg a) = "-" ++ show a 
    show (Rec a) = "1/" ++ show a 
    show (Sin a) = "sin (" ++ show a ++ ")"
    show (Cos a) = "cos (" ++ show a ++ ")"
    show (Log a) = "log (" ++ show a ++ ")"
    show (Exp a b) = show a ++ "^(" ++ show b ++ ")"
    show (Euler) = "e"


_simplify :: Expr -> Expr 
_simplify (Sum (Const a) (Const b)) = Const (a + b)
_simplify (Mul (Const a) (Const b)) = Const (a * b)
_simplify (Sum (Const 0.0) b) = _simplify b  
_simplify (Sum a (Const 0.0)) = _simplify a 
_simplify (Mul (Const 0.0) b) = Const 0.0
_simplify (Mul a (Const 0.0)) = Const 0.0
_simplify (Mul (Const 1.0) b) = _simplify b
_simplify (Mul a (Const 1.0)) = _simplify a
_simplify (Mul (Const a) (Mul (Const b  ) c)) = Mul (Const (a*b)) c
_simplify (Sum a b) | a == b = 2 * _simplify a 
                    | otherwise = _simplify a + _simplify b 
_simplify (Mul a b) = _simplify a * _simplify b
_simplify a = a 

_derivate :: Expr -> Expr 
_derivate (Const _) = Const 0 
_derivate (Var _) = Const 1 
_derivate (Sum a b) = _derivate a + _derivate b 
_derivate (Mul a b) = (_derivate a * b) + (a * _derivate b)
_derivate (Rec a) = - (_derivate a) / (a**2)
_derivate (Neg a) = - _derivate a 
_derivate (Log a) = 1/a * (_derivate a)
_derivate (Sin a) = cos a * _derivate a 
_derivate (Cos a) = -sin a * _derivate a 
_derivate (Exp Euler a) = (exp a) * _derivate a 
_derivate (Exp (Var x) (Const a)) = (Const a) * (exp (Var x) ** (Const (a-1)))


simplify :: Expr -> Expr
simplify expr = 
    let simplified = _simplify expr 
    in if simplified == expr 
        then expr 
        else simplify simplified

derivate :: Expr -> Expr
derivate = simplify . _derivate

f = 2 * x + 5 
    where 
        x = Var "x"

main = print $ derivate f
