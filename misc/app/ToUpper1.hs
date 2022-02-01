module Main where

import Data.Char
import Text.Printf

main :: IO ()
main = do
  let s = "abc123"
      t = map toUpper s
  putStrLn $ printf "s is %s" s
  putStrLn $ printf "t is %s" t