module Main where

import Data.GI.Base 
import Data.GI.Base.GType
import Data.GI.Base.GObject
import GI.GObject
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Control.Monad

#include <glib-object.h>

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

-- Why redefine objectRef, the reason follows https://github.com/haskell-gi/haskell-gi/issues/377
foreign import ccall "g_object_ref" c_g_object_ref :: Ptr a -> IO (Ptr a)

g_object_ref :: GObject o => o -> IO ()
g_object_ref obj = void $ withManagedPtr obj c_g_object_ref

main :: IO ()
main = do
  instance1 <- new Object []
  putStrLn "Call g_object_new."
  showRefCount instance1
  g_object_ref instance1
  putStrLn "Call g_object_ref."
  showRefCount instance1
  objectUnref instance1
  putStrLn "Call g_object_unref."
  showRefCount instance1
  objectUnref instance1
  putStrLn "Call g_object_unref."
  putStrLn "Now the reference count is zero and the instance is destroyed."
  putStrLn "The instance memories are possibly returned to the system."
  putStrLn "Therefore, the access to the same address may cause a segmentation error."
