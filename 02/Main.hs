module Main
       ( main )
       where

mkTupleUnsafe :: [[a]] -> (a, a)
mkTupleUnsafe [[x], [y]] = (x, y)
mkTupleUnsafe _ = error "now ye done it"

shapeScore :: (Num a) => Char -> a
shapeScore a =
  case a of
    'A' -> 1
    'X' -> 1
    'B' -> 2
    'Y' -> 2
    'C' -> 3
    'Z' -> 3
    
wrap :: (Num a, Ord a) => a -> a
wrap x
  | x > 3 = 1
  | otherwise = x

scoreGame :: (Eq a, Num a, Ord a) => Char -> Char -> (a, a)
scoreGame a b
  | shapeA == shapeB = (3 + shapeA, 3 + shapeB)
  | wrap (shapeA + 1) == shapeB = (0 + shapeA, 6 + shapeB)
  | otherwise = (6 + shapeA, shapeB)
  where shapeA = shapeScore a
        shapeB = shapeScore b

main :: IO ()
main = do
  content <- readFile "input.txt"
  let games = map (mkTupleUnsafe . words) $ lines content
  print $ sum $ map (snd . uncurry scoreGame) games
