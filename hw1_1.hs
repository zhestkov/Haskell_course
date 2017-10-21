-- isZero :: Int -> Bool
-- isZero 0 = True
-- isZero x = False
-- factorial :: Int -> Int
-- factorial n = if n <= 1 then 1 else n * factorial (n-1)
--
-- data Point = Point Float Float deriving (Show)
-- data Shape = Circle Point Float | Rect Point Point
-- surface :: Shape -> Float
-- surface (Circle _ r) = pi * r ** 2
-- surface (Rect (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- to fix Prelude issue
import Prelude hiding((<*>))

data BinaryOperator = Plus | Minus | Multiply deriving (Show, Eq)
data UnaryOperator = UnaryMinus deriving (Show, Eq)
data Term = IntConstant { intValue :: Int }
            | Variable { varName :: String }
            | BinaryTerm { lhv :: Term, rhv :: Term, op :: BinaryOperator }
            | UnaryTerm {val :: Term, unaryOp :: UnaryOperator}  deriving(Show,Eq)

infixl 6 <+>
(IntConstant a) <+> (IntConstant b) = IntConstant (a + b)
a <+> b = BinaryTerm a b Plus

infixl 6 <->
(IntConstant a) <-> (IntConstant b) = IntConstant (a - b)
a <-> b = BinaryTerm a b Minus

infixl 7 <*>
(IntConstant a) <*> (IntConstant b) = IntConstant (a * b)
a <*> b = BinaryTerm a b Multiply

infixl 8 <-->
(IntConstant a) = IntConstant (-a)
(<-->) a = UnaryTerm a UnaryMinus


replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant intValue) _ _ = IntConstant intValue
replaceVar (Variable var) name newTerm = if var == name then newTerm else Variable var
replaceVar (BinaryTerm lhv rhv op) name newTerm = BinaryTerm (replaceVar lhv name newTerm) (replaceVar rhv name newTerm) op
replaceVar (UnaryTerm term op) name newTerm = UnaryTerm (replaceVar term name newTerm) op



-- test
main :: IO ()
main = do
  let f a b c = (IntConstant a) <*> (IntConstant b) <+> (IntConstant c)
  let res = f 1 2 3
  print res
