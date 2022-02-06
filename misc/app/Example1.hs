{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.GI.Base 
import Data.GI.Base.GObject
import GI.GObject
import Data.GI.Base.Utils (checkUnexpectedReturnNULL)
import Foreign.Ptr (Ptr(..))

foreign import ccall "g_type_class_peek" g_type_class_peek :: 
  CGType -> IO (Ptr TypeClass)

-- The `typeClassPeek` in gi-gobject does not works the same as that in C api, so you must redefine it.
-- The reason of that follows https://github.com/haskell-gi/haskell-gi/issues/377
typeClassPeek' :: GType -> IO TypeClass
typeClassPeek' type_ = do
    let type_' = gtypeToCGType type_
    result <- g_type_class_peek type_'
    checkUnexpectedReturnNULL "typeClassPeek'" result
    managed <- newManagedPtr_ result
    return (TypeClass managed)

main :: IO ()
main = do
  instance1 <- new Object []
  instance2 <- new Object []

  withManagedPtr instance1 (\obj->putStrLn $ "The address of instance1 is " <> show obj)
  withManagedPtr instance2 (\obj->putStrLn $ "The address of instance2 is " <> show obj)

  type1 <- gtypeFromInstance instance1
  type2 <- gtypeFromInstance instance2
  
  class1 <- typeClassPeek' type1
  class2 <- typeClassPeek' type2
  
  withManagedPtr class1 (\cls->putStrLn $ "The address of the class of instance1 is" <> show cls)
  withManagedPtr class2 (\cls->putStrLn $ "The address of the class of instance2 is" <> show cls)

-- The privious version below
-- main :: IO ()
-- main = do
--   instance1 <- new Object []
--   instance2 <- new Object []

--   withManagedPtr instance1 (\obj->putStrLn $ "The address of instance1 is " <> show obj)
--   withManagedPtr instance2 (\obj->putStrLn $ "The address of instance2 is " <> show obj)

--   type1 <- gtypeFromInstance instance1
--   type2 <- gtypeFromInstance instance2
  
--   class1 <- typeClassPeek type1
--   class2 <- typeClassPeek type2
  
--   -- Why are the addresses of the class of instance1 and instance2 different? 
--   withManagedPtr class1 (\cls->putStrLn $ "The address of the class of instance1 is" <> show cls)
--   withManagedPtr class2 (\cls->putStrLn $ "The address of the class of instance2 is" <> show cls)