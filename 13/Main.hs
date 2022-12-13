module Main
       ( main )
       where

import Data.List (elemIndex)
import Data.List.Split (chunksOf)
import Data.Sort (sort)

data Signal a = Simple a | Compound [Signal a]
  deriving (Eq, Read, Show)

instance (Eq a, Ord a) => Ord (Signal a) where
  compare (Simple a) (Simple b) = compare a b
  compare (Compound a) (Compound b) = compare a b
  compare a@(Simple _) b@(Compound _) = compare (Compound [a]) b
  compare a@(Compound _) b@(Simple _) = compare a (Compound [b])

divider1 :: Signal Int
divider1 = Compound [Compound [Simple 2]]

divider2 :: Signal Int
divider2 = Compound [Compound [Simple 6]]

readSignal :: String -> Signal Int
readSignal = read

unchunk :: [Signal a] -> (Signal a, Signal a)
unchunk (x:y:_) = (x, y)

part1 :: [Signal Int] -> IO ()
part1 signals = do
  let pairs = chunksOf 2 signals
  print $ sum $ map fst $ filter ((==LT) . uncurry compare . snd) $ zip [1..] $ map unchunk pairs

part2 :: [Signal Int] -> IO ()
part2 signals = do
  let sorted = sort (divider1 : divider2 : signals)
  case (elemIndex divider1 sorted, elemIndex divider2 sorted) of
    (Just a, Just b) -> print $ (a + 1) * (b + 1)
    _                -> putStrLn "missing dividers"

main :: IO ()
main = do
  contents <- readFile "modded_input.txt"
  let signals = map readSignal $ filter (/=[]) $ lines contents
  part1 signals
  part2 signals
