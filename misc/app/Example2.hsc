{-# LANGUAGE OverloadedLabels #-}

module Main where

#include <glib-object.h>

import Data.GI.Base 
import Data.GI.Base.BasicTypes
import Data.GI.Base.GType
import Data.GI.Base.GObject
import GI.GObject
import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr 
import Foreign.Ptr

showRefCount :: Object -> IO ()
showRefCount obj = do
  b <- checkInstanceType obj gtypeObject
  if b then
    withManagedPtr obj
      (\p-> do
          {- Users should not use ref_count member in their program. -}
          {- This is only for demonstration. -}
          let pr = p `plusPtr` #{offset GObject, ref_count}
          ref <- peek pr :: IO CUInt
          putStrLn $ "Reference count is " <> show ref <> ".";
        )
  else
    putStrLn "Instance is not GObject."  

main :: IO ()
main = do
  instance1 <- new Object []
  putStrLn "Call g_object_new."
  showRefCount instance1
  objectRef instance1 -- why increasing by 2?
  putStrLn "Call g_object_ref."
  showRefCount instance1
  objectUnref instance1
  putStrLn "Call g_object_unref."
  showRefCount instance1
  objectUnref instance1
  putStrLn "Call g_object_unref."
  showRefCount instance1
