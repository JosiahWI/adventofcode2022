module Main
       ( main )
       where

import Data.List (intersperse)
import Data.List.Split (chunksOf)

getNumericalValue :: String -> Int
getNumericalValue "noop" = 0
getNumericalValue ('a':'d':'d':'x':' ':num) = read num

padInput :: [String] -> [String]
padInput [] = []
padInput (x@('a':_):xs) = "noop":x:(padInput xs)
padInput (x:xs) = x:(padInput xs)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let stages = scanl (+) 1 $ map getNumericalValue $ padInput $ lines contents
  print $ zip stages [0..]
  let interestingCycles = filter (\x -> (snd x - 20) `mod` 40 == 0) $ zip stages [1..]
  print interestingCycles
  print $ sum $ map (\x -> (fst x) * (snd x)) $ interestingCycles
  let pixels = [if abs ((x `mod` 40) - (clock `mod` 40)) <= 1 then '#' else '.' | (x, clock) <- zip stages [0..]]
  putStrLn $ concat $ intersperse "\n" $ chunksOf 40 pixels
