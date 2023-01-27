{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import TInt
import Text.Printf
import GI.GObject
import Data.GI.Base
import Data.GI.Base.GClosure
import Data.GI.Base.ShortPrelude
import Data.Text
import Foreign.Ptr
import GI.GObject.Objects.Object
import Data.Foldable
import Control.Monad

tPrint :: String -> TInt -> TInt -> TInt -> IO ()
tPrint op d1 d2 d3 = do
  v1 <- d1 `get` #value
  v2 <- d2 `get` #value
  v3 <- d3 `get` #value
  putStrLn $ printf "%d %s %d = %d" v1 op v2 v3

type NotifyCb = Ptr TInt -> Ptr GParamSpec -> Ptr () -> IO ()
foreign import ccall "wrapper"
  mkNotifyCb :: NotifyCb -> IO (FunPtr NotifyCb)

main :: IO ()
main = do
  d1 <- new TInt [ #value := 10]
  d2 <- new TInt [ #value := 20]
  
  cb <- mkNotifyCb $ \ptInt pspec _ -> do
    tInt <- TInt <$> newManagedPtr_ ptInt
    spec <- GParamSpec <$> newManagedPtr_ pspec
    name <- paramSpecGetName spec
    when (name == "value")
      $ do val <- tInt `get` #value
           putStr $ printf "Property \"%s\" is set to %d.\n" (unpack name) val
    
  connectSignalFunPtr d1 "notify::value" cb SignalConnectBefore Nothing

  d3 <- tIntAdd d1 d2
  tPrint "+" d1 d2 d3

  d3 <- tIntSub d1 d2
  tPrint "-" d1 d2 d3
  
  d3 <- tIntMul d1 d2
  tPrint "*" d1 d2 d3

  d3 <- tIntDiv d1 d2
  forM_ d3 (tPrint "/" d1 d2)

  d2 `set` [#value := 0]
  d3 <- tIntDiv d1 d2
  forM_ d3 (tPrint "/" d1 d2)

  d3 <- tIntUminus d1
  v1 <- d1 `get` #value
  v3 <- d3 `get` #value
  putStrLn $ printf "-(%d) = %d" v1 v3

  d1 `set` [#value := 100]
  -- another method also works
  --(toGValue (100 :: CInt)) >>= (objectSetProperty d1 "value")