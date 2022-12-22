module Main
       ( main )
       where

type Name = String
type MonkeyTree = BinaryTree Monkey

data Operation = Add | Sub | Mul | Div
  deriving (Eq, Show)

data Expr a =
    Expr Name Operation Name
  | Value a
  deriving Show

data BinaryTree a = BinaryTree (BinaryTree a) a (BinaryTree a) | Leaf
  deriving Show

data Monkey = Monkey Name (Expr Rational)
  deriving Show

mkOp :: String -> Operation
mkOp op
  | op == "+" = Add
  | op == "-" = Sub
  | op == "*" = Mul
  | op == "/" = Div
  | otherwise = error ("unrecognized op " ++ op)

mkMonkey :: String -> Monkey
mkMonkey s =
  case words s of
    (name:val:[])    -> Monkey (init name) (Value (fromIntegral $ read val))
    (name:a:ops:b:[]) -> Monkey (init name) (Expr a (mkOp ops) b)

ap :: Operation -> Rational -> Rational -> Rational
ap op a b
  | op == Add = a + b
  | op == Sub = a - b
  | op == Mul = a * b
  | op == Div = a / b

insert :: MonkeyTree -> Monkey -> MonkeyTree
insert Leaf a = BinaryTree Leaf a Leaf
insert tree@(BinaryTree left val@(Monkey name _) right) a@(Monkey name' _)
  | name' < name   = BinaryTree (insert left a) val right
  | name' > name   = BinaryTree left val (insert right a)
  | otherwise      = BinaryTree left a right

find :: MonkeyTree -> Name -> Monkey
find Leaf name   = error ("could not find monkey " ++ name)
find (BinaryTree left m@(Monkey name _) right) name' 
  | name' < name = find left name'
  | name' > name = find right name'
  | otherwise    = m

eval :: MonkeyTree -> Name -> Rational
eval tree name = case find tree name of
                   (Monkey _ (Value a))     -> a
                   (Monkey _ (Expr a op b)) -> ap op (eval tree a) (eval tree b)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let tree = foldl insert Leaf $ map mkMonkey $ lines contents
  print $ eval tree "root"
  let (Monkey _ (Expr a _ b)) = find tree "root"
  let subbed = insert tree (Monkey "root" (Expr a Sub b))
  let subbed' = insert subbed (Monkey "humn" (Value 3555057453229))
  let res = eval subbed' "root"
  print $ res
  print $ ((fromRational res) :: Double)
