{-# LANGUAGE OverloadedLabels #-}

module Main where

import Data.GI.Base 
import Data.GI.Base.GObject
import GI.GObject

main :: IO ()
main = do
  instance1 <- new Object []
  instance2 <- new Object []

  putStrLn $ "The address of instance1 is " <> show (managedForeignPtr . toManagedPtr $ instance1)
  putStrLn $ "The address of instance2 is " <> show (managedForeignPtr . toManagedPtr $ instance2)

  type1 <- gtypeFromInstance instance1
  type2 <- gtypeFromInstance instance2

  class1 <- typeClassPeek type1
  class2 <- typeClassPeek type2
  
  -- Why are the addresses of the class of instance1 and instance2 different? 
  putStrLn $ "The address of the class of instance1 is" <> show (managedForeignPtr . toManagedPtr $ class1)
  putStrLn $ "The address of the class of instance2 is" <> show (managedForeignPtr . toManagedPtr $ class2)
