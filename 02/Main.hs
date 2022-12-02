module Main
       ( main )
       where

data Shape = Rock | Paper | Scissors
  deriving Show

losesTo :: Shape -> Shape
losesTo Rock     = Scissors
losesTo Paper    = Rock
losesTo Scissors = Paper

winsTo :: Shape -> Shape
winsTo Rock     = Paper
winsTo Paper    = Scissors
winsTo Scissors = Rock

mkShape :: Char -> Maybe Shape
mkShape c = case c of
              'A' -> Just Rock
              'B' -> Just Paper
              'C' -> Just Scissors
              _   -> Nothing

matchPlay :: Shape -> Char -> Shape
matchPlay _ c = case c of
                  'X' -> Rock
                  'Y' -> Paper
                  'Z' -> Scissors

matchOutcome :: Shape -> Char -> Shape
matchOutcome shape targetResult =
  case targetResult of
    'X' -> losesTo shape
    'Y' -> shape
    'Z' -> winsTo shape

mkTupleUnsafe :: [[Char]] -> Maybe (Shape, Char)
mkTupleUnsafe [[x], [y]] =
  case mkShape x of
    Just shape -> Just (shape, y)
    Nothing    -> Nothing
mkTupleUnsafe _ = error "now ye done it"

wrap :: (Num a, Ord a) => a -> a
wrap x
  | x > 3     = 1
  | otherwise = x

score :: (Num a) => Shape -> a
score shape =
  case shape of
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

scoreGame :: (Eq a, Num a, Ord a) => (Shape -> Char -> Shape) -> Shape -> Char -> a
scoreGame decider a b
  | shapeA == shapeB = 3 + shapeB
  | wrap (shapeA + 1) == shapeB = 6 + shapeB
  | otherwise = shapeB
  where shapeA = score a
        shapeB = score $ decider a b

main :: IO ()
main = do
  content <- readFile "input.txt"
  let games = mapM (mkTupleUnsafe . words) $ lines content
  case games of
    Nothing -> putStrLn "Hello, your input is invalid. Goodbye."
    Just games' -> do
      print $ sum $ map (uncurry $ scoreGame matchPlay) games'
      print $ sum $ map (uncurry $ scoreGame matchOutcome) games'
