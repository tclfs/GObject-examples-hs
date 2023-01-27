-- Implement CDouble 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GObjectExtra
  ( gobjectInstallCDoubleProperty
  , CDoublePropertyInfo(..)
  ) where

import Data.GI.Base.GObject
import Data.GI.Base.BasicTypes
import Foreign.C
import Foreign.Ptr
import Data.Text
import Data.GI.Base.GParamSpec
import Data.GI.Base.ManagedPtr
import Data.GI.Base.BasicConversions
import Data.GI.Base.GQuark
import Data.GI.Base
import Data.GI.Base.GType
import Foreign.StablePtr
import Data.Coerce (coerce)

-- | The basic constructor for a GObject. They are all isomorphic.
newtype GObjectConstructor = GObjectConstructor (ManagedPtr GObjectConstructor)

-- | Construct a copy of the object from the given pointer.
objectFromPtr :: forall a o. GObject o => Ptr a -> IO o
objectFromPtr objPtr = newObject @o @o (coerce @_ @(ManagedPtr o -> o) GObjectConstructor) (castPtr objPtr)

foreign import ccall g_param_spec_set_qdata_full ::
  Ptr GParamSpec -> GQuark a -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()

foreign import ccall "&hs_free_stable_ptr" ptr_to_hs_free_stable_ptr ::
        FunPtr (Ptr a -> IO ())

-- | Set the given user data on the `GParamSpec`.
gParamSpecSetQData :: Ptr GParamSpec -> GQuark a -> a -> IO ()
gParamSpecSetQData pspecPtr quark d = do
  ptr <- newStablePtr d
  g_param_spec_set_qdata_full pspecPtr quark
                              (castStablePtrToPtr ptr)
                              ptr_to_hs_free_stable_ptr

foreign import ccall unsafe "g_value_set_double" _set_double ::
    Ptr GValue -> CDouble -> IO ()
foreign import ccall unsafe "g_value_get_double" _get_double ::
    Ptr GValue -> IO CDouble

instance IsGValue CDouble where
  gvalueGType_ = return gtypeDouble
  gvalueSet_ = _set_double
  gvalueGet_ = _get_double

-- | The `GQuark` pointing to the setter and getter of the property.
pspecQuark :: IO (GQuark (PropGetSetter o))
pspecQuark = gQuarkFromString "haskell-gi-get-set"

-- | Wrap a Haskell getter/setter into a lower level one.
wrapGetSet :: forall o a. (GObject o, IsGValue a) =>
              (o -> IO a)       -- ^ Haskell side getter
           -> (o -> a -> IO ()) -- ^ Haskell side setter
           -> (Ptr GValue -> a -> IO ()) -- ^ Setter for the `GValue`
           -> PropGetSetter o
wrapGetSet getter setter gvalueSetter = PropGetSetter {
  propGetter = \objPtr destPtr -> do
      value <- objectFromPtr objPtr >>= getter
      gvalueSetter destPtr value
  , propSetter = \objPtr newGValuePtr ->
      withTransient newGValuePtr $ \newGValue -> do
        obj <- objectFromPtr objPtr
        value <- fromGValue newGValue
        setter obj value
  }

foreign import ccall g_object_class_install_property ::
   GObjectClass -> CUInt -> Ptr GParamSpec -> IO ()

-- | Add a `Foreign.C.CDouble`-valued property to the given object class.
gobjectInstallCDoubleProperty :: DerivedGObject o =>
                              GObjectClass -> CDoublePropertyInfo o -> IO ()
gobjectInstallCDoubleProperty klass propInfo = do
  pspec <- gParamSpecCDouble propInfo
  withManagedPtr pspec $ \pspecPtr ->
    g_object_class_install_property klass 1 pspecPtr

data CDoublePropertyInfo o = CDoublePropertyInfo
  { name    :: Text              -- ^ Identifier for the property.
  , nick    :: Text              -- ^ Identifier for display to the user.
  , blurb   :: Text              -- ^ Description of the property.
  , defaultValue :: CDouble         -- ^ Default value.
  , setter :: o -> CDouble -> IO () -- ^ Handler invoked when the
                                 -- property is being set.
  , getter :: o -> IO CDouble       -- ^ Handler that returns the current
                                 -- value of the property.
  , flags   :: Maybe [GParamFlag] -- ^ Set of flags, or `Nothing` for
                                 -- the default set of flags.
  , minValue :: CDouble       -- ^ Minimum value.
  , maxValue :: CDouble       -- ^ Maximum value.
  }

-- | Default set of flags when constructing properties.
defaultFlags :: Num a => a
defaultFlags = gflagsToWord [GParamReadable, GParamWritable,
                             GParamExplicitNotify]

foreign import ccall g_param_spec_double  ::
   CString -> CString -> CString -> CDouble -> CDouble -> CDouble -> CInt
        -> IO (Ptr GParamSpec)

-- | Create a `GParamSpec` for an integer param.
gParamSpecCDouble :: GObject o => CDoublePropertyInfo o -> IO GParamSpec
gParamSpecCDouble (CDoublePropertyInfo {..}) =
  withTextCString name $ \cname ->
    withTextCString nick $ \cnick ->
      withTextCString blurb $ \cblurb -> do
        pspecPtr <- g_param_spec_double cname cnick cblurb
                                     minValue
                                     maxValue
                                     defaultValue
                                     (maybe defaultFlags gflagsToWord flags)
        quark <- pspecQuark
        gParamSpecSetQData pspecPtr quark (wrapGetSet getter setter gvalueSet_)
        wrapGParamSpecPtr pspecPtr