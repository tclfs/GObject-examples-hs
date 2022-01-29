{-# LANGUAGE OverloadedLabels #-}

module Main where

import Data.GI.Base 
import Data.GI.Base.GObject
import GI.GObject

main :: IO ()
main = do
  instance1 <- new Object []
  instance2 <- new Object []

  withManagedPtr instance1 (\obj->putStrLn $ "The address of instance1 is " <> show obj)
  withManagedPtr instance2 (\obj->putStrLn $ "The address of instance2 is " <> show obj)

  type1 <- gtypeFromInstance instance1
  type2 <- gtypeFromInstance instance2
  
  class1 <- typeClassPeek type1
  class2 <- typeClassPeek type2
  
  -- Why are the addresses of the class of instance1 and instance2 different? 
  withManagedPtr class1 (\cls->putStrLn $ "The address of the class of instance1 is" <> show cls)
  withManagedPtr class2 (\cls->putStrLn $ "The address of the class of instance2 is" <> show cls)
