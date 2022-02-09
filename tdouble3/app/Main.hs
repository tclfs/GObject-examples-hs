module Main where

import TDouble
import Text.Printf

tPrint :: String -> TDouble -> TDouble -> TDouble -> IO ()
tPrint op d1 d2 d3 = do
  v1 <- tDoubleGetValue d1
  v2 <- tDoubleGetValue d2
  v3 <- tDoubleGetValue d3
  putStrLn $ printf "%f %s %f = %f" v1 op v2 v3

main :: IO ()
main = do
  d1 <- tDoubleNew 10.0
  d2 <- tDoubleNew 20.0
  d3 <- tDoubleAdd d1 d2
  tPrint "+" d1 d2 d3

  d3 <- tDoubleSub d1 d2
  tPrint "-" d1 d2 d3
  
  d3 <- tDoubleMul d1 d2
  tPrint "*" d1 d2 d3

  d3 <- tDoubleDiv d1 d2
  tPrint "/" d1 d2 d3

  d3 <- tDoubleUminus d1
  v1 <- tDoubleGetValue d1
  v3 <- tDoubleGetValue d3
  putStrLn $ printf "-(%f) = %f" v1 v3
