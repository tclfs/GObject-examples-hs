{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds #-}

#include <glib-object.h>

module TInt 
    ( TInt(..)
    , IsTInt
    , toTInt
    , tIntNew
    , tIntNewWithValue
    , tIntAdd
    , tIntSub
    , tIntMul
    , tIntDiv
    , tIntUminus
    )
    where

import           Data.GI.Base
import           Data.GI.Base.GValue
import           Data.GI.Base.GObject
import           Data.GI.Base.GClosure
import qualified Data.GI.Base.Overloading      as O
import           Data.GI.Base.GParamSpec (CIntPropertyInfo(..), GParamFlag(..))
import Data.GI.Base.Attributes
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
import Data.Coerce
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

type DivByZeroDefaultCb = Ptr TInt -> IO ()
foreign import ccall "wrapper"
  mkDivByZeroDefaultCb :: DivByZeroDefaultCb -> IO (FunPtr DivByZeroDefaultCb)

newtype TInt = TInt (ManagedPtr TInt)

instance TypedObject TInt where
  glibType = registerGType TInt

instance GObject TInt

newtype TIntPrivate = 
  TIntPrivate { value :: Int }

instance DerivedGObject TInt where
  type GObjectParentType  TInt = GObject.Object
  type GObjectPrivateData TInt = TIntPrivate

  objectTypeName = "TInt"

  objectClassInit = tIntClassInit
  objectInstanceInit = tIntInstanceInit

combineSignalFlags :: [GObject.SignalFlags] -> Int
combineSignalFlags = foldr ((.|.) . fromEnum) 0

gTIntSignal :: IORef CUInt
{-# NOINLINE gTIntSignal #-}
gTIntSignal = unsafePerformIO (newIORef 0)

{- In gobject, g_signal_new_class_handler is implemented by g_signal_new_valist,
  whitch is implemented by g_signal_newv; and yet g_signal_new_class_handler and g_signal_new_valist
  with varargs are not foreign imported, so here we foreign import g_signal_newv.
-}
tIntClassInit :: GObjectClass -> IO ()
tIntClassInit klass = do
  signalName <- newCString "div-by-zero"
  itype <- gtypeFromClass klass
  let signalFlags = combineSignalFlags [ GObject.SignalFlagsRunLast
                                       , GObject.SignalFlagsNoRecurse
                                       , GObject.SignalFlagsNoHooks]
  defaultCb <- mkDivByZeroDefaultCb $ \_ -> putStrLn "\nError: division by zero.\n"
  closure <- newGClosure defaultCb
  withManagedPtr closure $ \c -> do
    tIntSignal <- c_g_signal_newv signalName
                                    (gtypeToCGType itype)
                                    (fromIntegral signalFlags)
                                    c
                                    nullFunPtr
                                    nullPtr
                                    nullFunPtr
                                    #const G_TYPE_NONE
                                    0
                                    nullPtr
    writeIORef gTIntSignal tIntSignal

  gobjectInstallCIntProperty klass valueProperty

tIntInstanceInit :: GObjectClass -> TInt -> IO TIntPrivate
tIntInstanceInit klass t = return $ TIntPrivate 0

instance O.HasParentTypes TInt

type instance O.ParentTypes TInt = GObject.Object ': O.ParentTypes GObject.Object

class (GObject o, O.IsDescendantOf TInt o) => IsTInt o
instance (GObject o, O.IsDescendantOf TInt o) => IsTInt o

toTInt :: (MonadIO m, IsTInt o) => o -> m TInt
toTInt = liftIO . unsafeCastTo TInt

valueProperty :: CIntPropertyInfo TInt
valueProperty =
  CIntPropertyInfo { name   = "value"
                   , nick   = "val"
                   , blurb  = "Int value"
                   , setter = setValueCInt
                   , getter = getValueCInt
                   , flags  = Just [GParamReadable, GParamWritable]
                   , defaultValue = 0
                   , minValue = Nothing
                   , maxValue = Nothing
               }

setValueCInt :: TInt -> CInt -> IO ()
setValueCInt tInt d =  setValue tInt (fromIntegral d)

getValueCInt :: TInt -> IO CInt
getValueCInt tInt = fromIntegral <$> value <$> gobjectGetPrivateData tInt

setValue :: TInt -> Int -> IO ()
setValue tInt newValue = do
  gobjectModifyPrivateData tInt
               (\priv -> priv {value = newValue})
  
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
  -- from TInt will do.
  type AttrBaseTypeConstraint ValueAttrInfo = IsTInt

  -- Which type does 'get' on the property return. By default this is
  -- also the type that 'set' and 'new' accept.
  type AttrGetType ValueAttrInfo = Int

  -- Text description for the attribute, for use in error messages.
  type AttrLabel ValueAttrInfo = "value"

  -- Type defining the attribute, for use in error messages.
  type AttrOrigin ValueAttrInfo = TInt

  -- Get the value of the attribute.
  attrGet tInt = value <$>
                      gobjectGetPrivateData (coerce tInt :: TInt)

  -- Set the value of the argument.
  attrSet tInt val = --setValue (coerce tInt :: TInt) val
    (toGValue $ (fromIntegral val :: CInt)) >>= (GObject.objectSetProperty (coerce tInt :: TInt) "value")

  -- Construct a 'GValue' containing the argument, tagged by the
  -- associated property name.
  attrConstruct val = do
    newValue <- toGValue (fromIntegral val :: CInt)
    return $ GValueConstruct "value" newValue

-- Allow the overloaded attribute syntax to work. In this case we
-- inherit all attributes of our parent type, and add the value
-- property.
instance O.HasAttributeList TInt
type instance O.AttributeList TInt = 
  '("value", ValueAttrInfo) ': O.AttributeList GObject.Object

type instance O.SignalList TInt = O.SignalList GObject.Object

type family ResolveTIntMethod t o where
  ResolveTIntMethod t o = GObject.ResolveObjectMethod t o

{- The circular instance trick is to avoid the liberal coverage
condition. We should be using DYSFUNCTIONAL pragmas instead, once
those are implemented:
https://github.com/ghc-proposals/ghc-proposals/pull/374
-}
instance (info ~ ResolveTIntMethod method TInt,
          O.OverloadedMethod info TInt p,
          R.HasField method TInt p)
    => R.HasField method TInt p where
  getField = O.overloadedMethod @info

instance (info ~ ResolveTIntMethod t TInt,
          O.OverloadedMethod info TInt p)
         => OL.IsLabel t (TInt -> p) where
    fromLabel = O.overloadedMethod @info

-- This is useful for debugging
instance (info ~ ResolveTIntMethod t TInt,
          O.OverloadedMethodInfo info TInt)
         => OL.IsLabel t (O.MethodProxy info TInt) where
    fromLabel = O.MethodProxy

tIntNew :: IO TInt
tIntNew = do
  d <- new TInt []
  gobjectModifyPrivateData d (\x -> x { value = 0 })
  return d

tIntNewWithValue :: Int -> IO TInt
tIntNewWithValue val = do
  d <- new TInt []
  gobjectModifyPrivateData d (\x -> x { value = val })
  return d

{- arithmetic operator -}
{- These operators create a new instance and return a pointer to it. -}
tIntAdd :: TInt -> TInt -> IO TInt
tIntAdd = tIntBinaryOp (+)

tIntSub :: TInt -> TInt -> IO TInt
tIntSub = tIntBinaryOp (-)

tIntMul :: TInt -> TInt -> IO TInt
tIntMul = tIntBinaryOp (*)

tIntDiv :: TInt -> TInt -> IO (Maybe TInt)
tIntDiv self other = do
  oValue <- other `get` ##value
  if oValue == 0 then do
    -- the verbose version       
    -- withManagedPtr self 
    --   (\obj->do
    --       itype <- glibType @TInt
    --       signalId <- readIORef gTIntSignal
    --       gvalueSelf <- buildGValue itype set_object obj
    --       GObject.signalEmitv [gvalueSelf] (fromIntegral signalId) 0
    --       return Nothing
    --     )
    withManagedPtr self 
      (\obj->do
          signalId <- readIORef gTIntSignal
          gvalueSelf <- toGValue obj
          GObject.signalEmitv [gvalueSelf] (fromIntegral signalId) 0
          return Nothing
        )
  else do
    sValue <-  self `get` ##value
    ret <- tIntNewWithValue $ sValue `div` oValue
    return $ Just ret

tIntUminus :: TInt -> IO TInt
tIntUminus self = do
  val <- self `get` ##value
  tIntNewWithValue (-val)

tIntBinaryOp :: (Int -> Int -> Int) -> TInt -> TInt -> IO TInt
tIntBinaryOp op self other = do
  sValue <- self `get` ##value
  oValue <- other `get` ##value
  tIntNewWithValue $ sValue `op` oValue