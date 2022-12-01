module Main
       ( main )
       where

import Data.List (sort)
import Data.List.Split (splitWhen)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let stuff = map (sum . map read) $ splitWhen (=="") $ lines contents
  print $ maximum $ stuff
  print $ sum $ take 3 $ reverse $ sort $ stuff
