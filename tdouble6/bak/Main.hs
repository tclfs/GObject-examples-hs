{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import TDouble
import Text.Printf
import GI.GObject
import Data.GI.Base
import Data.GI.Base.GClosure
import Data.GI.Base.ShortPrelude
import Data.Text
import Foreign.Ptr
import GI.GObject.Objects.Object
import Data.Foldable

tPrint :: String -> TDouble -> TDouble -> TDouble -> IO ()
tPrint op d1 d2 d3 = do
  v1 <- d1 `get` #value
  v2 <- d2 `get` #value
  v3 <- d3 `get` #value
  putStrLn $ printf "%f %s %f = %f" v1 op v2 v3

type NotifyCb = Ptr TDouble -> Ptr GParamSpec -> Ptr () -> IO ()
foreign import ccall "wrapper"
  mkNotifyCb :: NotifyCb -> IO (FunPtr NotifyCb)

main :: IO ()
main = do
  d1 <- new TDouble [ #value := 10.0]
  d2 <- new TDouble [ #value := 20.0]
  
  cb <- mkNotifyCb $ \ptDouble pspec _ -> do
    tDouble <- TDouble <$> newManagedPtr_ ptDouble
    spec <- GParamSpec <$> newManagedPtr_ pspec
    name <- paramSpecGetName spec
    if name == "value" then do
      val <- tDouble `get` #value
      putStr $ printf "Property \"%s\" is set to %f.\n" (unpack name) val
    else return ()
  
  connectSignalFunPtr d1 "notify::value" cb SignalConnectBefore Nothing

  d3 <- tDoubleAdd d1 d2
  tPrint "+" d1 d2 d3

  d3 <- tDoubleSub d1 d2
  tPrint "-" d1 d2 d3
  
  d3 <- tDoubleMul d1 d2
  tPrint "*" d1 d2 d3

  d3 <- tDoubleDiv d1 d2
  forM_ d3 (tPrint "/" d1 d2)

  d2 `set` [#value := 0]
  d3 <- tDoubleDiv d1 d2
  forM_ d3 (tPrint "/" d1 d2)

  d3 <- tDoubleUminus d1
  v1 <- d1 `get` #value
  v3 <- d3 `get` #value
  putStrLn $ printf "-%f = %f" v1 v3

  d1 `set` [#value := 100]
  -- the line above does not emit notify signal
  -- (toGValue (100 :: CInt)) >>= (objectSetProperty d1 "value")