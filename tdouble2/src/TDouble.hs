{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

module TDouble 
    ( TDouble(..)
    , IsTDouble
    , toTDouble
    , tDoubleNew
    , tDoubleGetValue
    , tDoubleSetValue
    , tDoubleAdd
    , tDoubleSub
    , tDoubleMul
    , tDoubleDiv
    , tDoubleUminus
    )
    where

import           Data.GI.Base
import           Data.GI.Base.GObject 
import qualified Data.GI.Base.Overloading      as O
import           GHC.OverloadedLabels          as OL
import qualified GHC.Records as R

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

type family ResolveTDoubleMethod t o where
  ResolveTDoubleMethod t o = GObject.ResolveObjectMethod t o

{- The circular instance trick is to avoid the liberal coverage
condition. We should be using DYSFUNCTIONAL pragmas instead, once
those are implemented:
https://github.com/ghc-proposals/ghc-proposals/pull/374
-}
instance (info ~ ResolveTDoubleMethod method TDouble,
          O.OverloadedMethod info TDouble p,
          R.HasField method TDouble p)
    => R.HasField method TDouble p where
  getField = O.overloadedMethod @info

instance (info ~ ResolveTDoubleMethod t TDouble,
          O.OverloadedMethod info TDouble p)
         => OL.IsLabel t (TDouble -> p) where
    fromLabel = O.overloadedMethod @info

-- This is useful for debugging
instance (info ~ ResolveTDoubleMethod t TDouble,
          O.OverloadedMethodInfo info TDouble)
         => OL.IsLabel t (O.MethodProxy info TDouble) where
    fromLabel = O.MethodProxy

tDoubleNew :: Double -> IO TDouble
tDoubleNew val = do
  d <- new TDouble []
  gobjectModifyPrivateData d (\x -> x { value = val })
  return d

{-  getter and setter -}
tDoubleGetValue :: TDouble -> IO Double
tDoubleGetValue d = value <$> gobjectGetPrivateData d

tDoubleSetValue :: TDouble -> Double -> IO ()
tDoubleSetValue d val = gobjectModifyPrivateData d (\x -> x { value = val })

{- arithmetic operator -}
{- These operators create a new instance and return a pointer to it. -}
tDoubleAdd :: TDouble -> TDouble -> IO TDouble
tDoubleAdd = tDoubleBinaryOp (+)

tDoubleSub :: TDouble -> TDouble -> IO TDouble
tDoubleSub = tDoubleBinaryOp (-)

tDoubleMul :: TDouble -> TDouble -> IO TDouble
tDoubleMul = tDoubleBinaryOp (*)

tDoubleDiv :: TDouble -> TDouble -> IO TDouble
tDoubleDiv = tDoubleBinaryOp (/)

tDoubleUminus :: TDouble -> IO TDouble
tDoubleUminus self = do
  val <- tDoubleGetValue self
  tDoubleNew (-val)

tDoubleBinaryOp :: (Double -> Double -> Double) -> TDouble -> TDouble -> IO TDouble
tDoubleBinaryOp op self other = do
  sValue <- tDoubleGetValue self
  oValue <- tDoubleGetValue other
  tDoubleNew $ sValue `op` oValue
