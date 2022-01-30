{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.GI.Base
import           Data.GI.Base.GObject 
import qualified Data.GI.Base.Overloading      as O

import           Control.Monad
import           Control.Monad.IO.Class
import qualified GI.GObject as GObject

newtype TDouble = TDouble (ManagedPtr TDouble)

instance TypedObject TDouble where
  glibType = registerGType TDouble

instance GObject TDouble

newtype TDoublePrivate = 
  TDoublePrivate { value :: Double }

instance DerivedGObject TDouble where
  type GObjectParentType  TDouble = GObject.Object
  type GObjectPrivateData TDouble = TDoublePrivate

  objectTypeName = "TDouble"

  objectClassInit = tDoubleClassInit
  objectInstanceInit = tDoubleInstanceInit

tDoubleClassInit :: GObjectClass -> IO ()
tDoubleClassInit klass = return ()

tDoubleInstanceInit :: GObjectClass -> TDouble -> IO TDoublePrivate
tDoubleInstanceInit klass t = return $ TDoublePrivate 0

instance O.HasParentTypes TDouble

type instance O.ParentTypes TDouble = GObject.Object ': O.ParentTypes GObject.Object

class (GObject o, O.IsDescendantOf TDouble o) => IsTDouble o
instance (GObject o, O.IsDescendantOf TDouble o) => IsTDouble o

toTDouble :: (MonadIO m, IsTDouble o) => o -> m TDouble
toTDouble = liftIO . unsafeCastTo TDouble

instance O.HasAttributeList TDouble
type instance O.AttributeList TDouble = O.AttributeList GObject.Object

type instance O.SignalList TDouble = O.SignalList GObject.Object


main :: IO ()
main = do
  dtype <- glibType @TDouble
  if dtype /= GType 0 then
    putStrLn $ "Registration was a success. The type is " <> show dtype <> "."
  else
    putStrLn "Registration failed."
  
  void $ new TDouble []