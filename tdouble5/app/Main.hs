{-# LANGUAGE OverloadedStrings #-}

module Main where

import TDouble
import Text.Printf
import GI.GObject
import Data.GI.Base.GClosure
import Data.GI.Base.ShortPrelude

import Foreign.Ptr

tPrint :: String -> TDouble -> TDouble -> TDouble -> IO ()
tPrint op d1 d2 d3 = do
  v1 <- tDoubleGetValue d1
  v2 <- tDoubleGetValue d2
  v3 <- tDoubleGetValue d3
  putStrLn $ printf "%f %s %f = %f" v1 op v2 v3

type DivByZeroCb = Ptr TDouble -> Ptr () -> IO ()
foreign import ccall "wrapper"
  mkDivByZeroCb :: DivByZeroCb -> IO (FunPtr DivByZeroCb)

type DivByZeroAfterCb = Ptr TDouble -> Ptr () -> IO ()
foreign import ccall "wrapper"
  mkDivByZeroAfterCb :: DivByZeroAfterCb -> IO (FunPtr DivByZeroAfterCb)

main :: IO ()
main = do
  d1 <- tDoubleNew 10.0
  d2 <- tDoubleNew 20.0
  
  cb <- mkDivByZeroCb $ \_ _ -> putStr "\nError happens in main.c.\n"
  connectSignalFunPtr d1 "div-by-zero" cb SignalConnectBefore Nothing
  cb <- mkDivByZeroAfterCb $ \_ _ -> 
      putStr "Error has happened in main.c and an error message has been displayed.\n\n"
  connectSignalFunPtr d1 "div-by-zero" cb SignalConnectAfter Nothing

  d3 <- tDoubleAdd d1 d2
  tPrint "+" d1 d2 d3

  d3 <- tDoubleSub d1 d2
  tPrint "-" d1 d2 d3
  
  d3 <- tDoubleMul d1 d2
  tPrint "*" d1 d2 d3

  d3 <- tDoubleDiv d1 d2
  case d3 of
    Just obj -> tPrint "/" d1 d2 obj
    Nothing  -> return ()

  tDoubleSetValue d2 0
  d3 <- tDoubleDiv d1 d2
  case d3 of
    Just obj -> tPrint "/" d1 d2 obj
    Nothing  -> return ()

  d3 <- tDoubleUminus d1
  v1 <- tDoubleGetValue d1
  v3 <- tDoubleGetValue d3
  putStrLn $ printf "-%f = %f" v1 v3
