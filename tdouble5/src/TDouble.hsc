{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

#include <glib-object.h>

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
import           Data.GI.Base.GValue
import           Data.GI.Base.GObject
import           Data.GI.Base.GClosure
import qualified Data.GI.Base.Overloading      as O
import           GHC.OverloadedLabels          as OL
import qualified GHC.Records as R

import           Control.Monad
import           Control.Monad.IO.Class
import qualified GI.GObject as GObject


import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Bits
import Data.IORef
import System.IO.Unsafe

-- g_signal_newv is marked as not introspectable (not exposed to bindings).
-- guint
-- g_signal_newv (
--   const gchar* signal_name,
--   GType itype,
--   GSignalFlags signal_flags,
--   GClosure* class_closure,
--   GSignalAccumulator accumulator,
--   gpointer accu_data,
--   GSignalCMarshaller c_marshaller,
--   GType return_type,
--   guint n_params,
--   GType* param_types
-- )
foreign import ccall unsafe "g_signal_newv" c_g_signal_newv :: 
  CString -> CGType -> CInt -> Ptr (GClosure a) -> FunPtr GObject.C_SignalAccumulator ->
  Ptr () -> FunPtr GObject.C_ClosureMarshal -> CGType -> CUInt -> Ptr CGType -> IO CUInt

type DivByZeroDefaultCb = Ptr TDouble -> IO ()
foreign import ccall "wrapper"
  mkDivByZeroDefaultCb :: DivByZeroDefaultCb -> IO (FunPtr DivByZeroDefaultCb)

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

combineSignalFlags :: [GObject.SignalFlags] -> Int
combineSignalFlags = foldr ((.|.) . fromEnum) 0

gTDoubleSignal :: IORef CUInt
{-# NOINLINE gTDoubleSignal #-}
gTDoubleSignal = unsafePerformIO (newIORef 0)

{- In gobject, g_signal_new_class_handler is implemented by g_signal_new_valist,
  whitch is implemented by g_signal_newv; and yet g_signal_new_class_handler and g_signal_new_valist
  with varargs are not foreign imported, so here we foreign import g_signal_newv.
-}
tDoubleClassInit :: GObjectClass -> IO ()
tDoubleClassInit klass = do
  signalName <- newCString "div-by-zero"
  itype <- gtypeFromClass klass
  let signalFlags = combineSignalFlags [ GObject.SignalFlagsRunLast
                                       , GObject.SignalFlagsNoRecurse
                                       , GObject.SignalFlagsNoHooks]
  defaultCb <- mkDivByZeroDefaultCb $ \_ -> putStrLn "\nError: division by zero.\n"
  closure <- newGClosure defaultCb
  withManagedPtr closure $ \c -> do
    tDoubleSignal <- c_g_signal_newv signalName
                                    (gtypeToCGType itype)
                                    (fromIntegral signalFlags)
                                    c
                                    nullFunPtr
                                    nullPtr
                                    nullFunPtr
                                    #const G_TYPE_NONE
                                    0
                                    nullPtr
    writeIORef gTDoubleSignal tDoubleSignal

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
tDoubleGetValue self = value <$> gobjectGetPrivateData self

tDoubleSetValue :: TDouble -> Double -> IO ()
tDoubleSetValue self val = gobjectModifyPrivateData self (\x -> x { value = val })

{- arithmetic operator -}
{- These operators create a new instance and return a pointer to it. -}
tDoubleAdd :: TDouble -> TDouble -> IO TDouble
tDoubleAdd = tDoubleBinaryOp (+)

tDoubleSub :: TDouble -> TDouble -> IO TDouble
tDoubleSub = tDoubleBinaryOp (-)

tDoubleMul :: TDouble -> TDouble -> IO TDouble
tDoubleMul = tDoubleBinaryOp (*)

tDoubleDiv :: TDouble -> TDouble -> IO (Maybe TDouble)
tDoubleDiv self other = do
  oValue <- tDoubleGetValue other
  if oValue == 0 then do
    -- the verbose version    
    -- withManagedPtr self 
    --   (\obj->do
    --       itype <- glibType @TDouble
    --       signalId <- readIORef gTDoubleSignal
    --       gvalueSelf <- buildGValue itype set_object obj
    --       GObject.signalEmitv [gvalueSelf] (fromIntegral signalId) 0
    --       return Nothing
    --     )
    withManagedPtr self 
      (\obj->do
          signalId <- readIORef gTDoubleSignal
          gvalueSelf <- toGValue obj
          GObject.signalEmitv [gvalueSelf] (fromIntegral signalId) 0
          return Nothing
        )
  else do
    sValue <- tDoubleGetValue self
    ret <- tDoubleNew $ sValue / oValue
    return $ Just ret

tDoubleUminus :: TDouble -> IO TDouble
tDoubleUminus self = do
  val <- tDoubleGetValue self
  tDoubleNew (-val)

tDoubleBinaryOp :: (Double -> Double -> Double) -> TDouble -> TDouble -> IO TDouble
tDoubleBinaryOp op self other = do
  sValue <- tDoubleGetValue self
  oValue <- tDoubleGetValue other
  tDoubleNew $ sValue `op` oValue