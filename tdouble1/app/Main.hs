module Main where

import TDouble
import Text.Printf

main :: IO ()
main = do
  d <- tDoubleNew 10.0
  value <- tDoubleGetValue d
  putStrLn $ printf "t_double_get_value succesfully assigned %f to value." value

  tDoubleSetValue d (-20.0)
  putStrLn $ printf "Now, set d (tDouble object) with %f." (-20.0 :: Double)
  value <- tDoubleGetValue d
  putStrLn $ printf "t_double_get_value succesfully assigned %f to value." value
