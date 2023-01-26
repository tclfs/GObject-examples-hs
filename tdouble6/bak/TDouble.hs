{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

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
    , tDoubleNewWithValue
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
import           Data.GI.Base.GParamSpec (PropertyInfo(..), GParamFlag(..))
import Data.GI.Base.Attributes (AttrInfo(..), AttrOpTag(..))
import           GHC.OverloadedLabels          as OL
import qualified GHC.Records as R

import           Control.Monad
import           Control.Monad.IO.Class
import qualified GI.GObject as GObject
import Data.GI.Base.GType

import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Bits
import Data.IORef
import Data.Coerce (coerce)
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

  gobjectInstallProperty klass valueProperty

tDoubleInstanceInit :: GObjectClass -> TDouble -> IO TDoublePrivate
tDoubleInstanceInit klass t = return $ TDoublePrivate 0

instance O.HasParentTypes TDouble

type instance O.ParentTypes TDouble = GObject.Object ': O.ParentTypes GObject.Object

class (GObject o, O.IsDescendantOf TDouble o) => IsTDouble o
instance (GObject o, O.IsDescendantOf TDouble o) => IsTDouble o

toTDouble :: (MonadIO m, IsTDouble o) => o -> m TDouble
toTDouble = liftIO . unsafeCastTo TDouble

-- ?? How to specify min, max and default value
valueProperty :: PropertyInfo TDouble CDouble
valueProperty =
  PropertyInfo { name   = "value"
               , nick   = "val"
               , blurb  = "Double value"
               , setter = setValueCDouble
               , getter = getValueCDouble
               , flags  = Just [GParamReadable, GParamWritable]
               }

setValueCDouble :: TDouble -> CDouble -> IO ()
setValueCDouble tDouble d =  setValue tDouble (realToFrac d)

getValueCDouble :: TDouble -> IO CDouble
getValueCDouble tDouble = realToFrac <$> value <$> gobjectGetPrivateData tDouble

setValue :: TDouble -> Double -> IO ()
setValue tDouble newValue = do
  gobjectModifyPrivateData tDouble
               (\priv -> priv {value = newValue})

foreign import ccall unsafe "g_value_set_double" _set_double ::
    Ptr GValue -> CDouble -> IO ()
foreign import ccall unsafe "g_value_get_double" _get_double ::
    Ptr GValue -> IO CDouble

set_double :: Ptr GValue -> Double -> IO ()
set_double gv d = _set_double gv (realToFrac d)

get_double :: Ptr GValue -> IO Double
get_double gv = realToFrac <$> _get_double gv

instance IsGValue CDouble where
  gvalueGType_ = return gtypeDouble
  gvalueSet_ = _set_double
  gvalueGet_ = _get_double
  
-- Tell the type system about the "value" property, so we can
-- use the overloading syntax.
data ValueAttrInfo

-- This instance encodes the information for the property at the type
-- level.
instance AttrInfo ValueAttrInfo where
  -- This is a list of the actions allowed on the attribute: we can
  -- get, set, and create it when constructing the object.
  type AttrAllowedOps ValueAttrInfo = '[ 'AttrGet, 'AttrSet,
                                         'AttrConstruct ]

  -- For which types can we get/set the attribute. Anything deriving
  -- from TDouble will do.
  type AttrBaseTypeConstraint ValueAttrInfo = IsTDouble

  -- Which type does 'get' on the property return. By default this is
  -- also the type that 'set' and 'new' accept.
  type AttrGetType ValueAttrInfo = Double

  -- Text description for the attribute, for use in error messages.
  type AttrLabel ValueAttrInfo = "value"

  -- Type defining the attribute, for use in error messages.
  type AttrOrigin ValueAttrInfo = TDouble

  -- Get the value of the attribute.
  attrGet tDouble = value <$>
                      gobjectGetPrivateData (coerce tDouble :: TDouble)

  -- Set the value of the argument.
  attrSet tDouble val = setValue (coerce tDouble :: TDouble) val

  -- Construct a 'GValue' containing the argument, tagged by the
  -- associated property name.
  attrConstruct val = do
    newValue <- toGValue (realToFrac val :: CDouble)
    return $ GValueConstruct "value" newValue

-- Allow the overloaded attribute syntax to work. In this case we
-- inherit all attributes of our parent type, and add the value
-- property.
instance O.HasAttributeList TDouble
type instance O.AttributeList TDouble = 
  '("value", ValueAttrInfo) ': O.AttributeList GObject.Object

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

tDoubleNew :: IO TDouble
tDoubleNew = do
  d <- new TDouble []
  gobjectModifyPrivateData d (\x -> x { value = 0 })
  return d

tDoubleNewWithValue :: Double -> IO TDouble
tDoubleNewWithValue val = do
  d <- new TDouble []
  gobjectModifyPrivateData d (\x -> x { value = val })
  return d

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
  oValue <- other `get` ##value
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
    sValue <-  self `get` ##value
    ret <- tDoubleNewWithValue $ sValue / oValue
    return $ Just ret

tDoubleUminus :: TDouble -> IO TDouble
tDoubleUminus self = do
  val <- self `get` ##value
  tDoubleNewWithValue (-val)

tDoubleBinaryOp :: (Double -> Double -> Double) -> TDouble -> TDouble -> IO TDouble
tDoubleBinaryOp op self other = do
  sValue <- self `get` ##value
  oValue <- other `get` ##value
  tDoubleNewWithValue $ sValue `op` oValue