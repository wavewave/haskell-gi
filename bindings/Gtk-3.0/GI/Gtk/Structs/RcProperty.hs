{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Deprecated

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.RcProperty
    ( 

-- * Exported types
    RcProperty(..)                          ,
    newZeroRcProperty                       ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveRcPropertyMethod                 ,
#endif

-- ** parseBorder #method:parseBorder#

    rcPropertyParseBorder                   ,


-- ** parseColor #method:parseColor#

    rcPropertyParseColor                    ,


-- ** parseEnum #method:parseEnum#

    rcPropertyParseEnum                     ,


-- ** parseFlags #method:parseFlags#

    rcPropertyParseFlags                    ,


-- ** parseRequisition #method:parseRequisition#

    rcPropertyParseRequisition              ,




 -- * Properties


-- ** origin #attr:origin#
-- | field similar to one found in t'GI.Gtk.Structs.SettingsValue.SettingsValue'

    clearRcPropertyOrigin                   ,
    getRcPropertyOrigin                     ,
#if defined(ENABLE_OVERLOADING)
    rcProperty_origin                       ,
#endif
    setRcPropertyOrigin                     ,


-- ** propertyName #attr:propertyName#
-- | quark-ified property identifier like
--   “GtkScrollbar[spacing](#g:signal:spacing)”

    getRcPropertyPropertyName               ,
#if defined(ENABLE_OVERLOADING)
    rcProperty_propertyName                 ,
#endif
    setRcPropertyPropertyName               ,


-- ** typeName #attr:typeName#
-- | quark-ified type identifier

    getRcPropertyTypeName                   ,
#if defined(ENABLE_OVERLOADING)
    rcProperty_typeName                     ,
#endif
    setRcPropertyTypeName                   ,


-- ** value #attr:value#
-- | field similar to one found in t'GI.Gtk.Structs.SettingsValue.SettingsValue'

    clearRcPropertyValue                    ,
    getRcPropertyValue                      ,
#if defined(ENABLE_OVERLOADING)
    rcProperty_value                        ,
#endif
    setRcPropertyValue                      ,




    ) where

import Data.GI.Base.ShortPrelude
import qualified Data.GI.Base.ShortPrelude as SP
import qualified Data.GI.Base.Overloading as O
import qualified Prelude as P

import qualified Data.GI.Base.Attributes as GI.Attributes
import qualified Data.GI.Base.BasicTypes as B.Types
import qualified Data.GI.Base.ManagedPtr as B.ManagedPtr
import qualified Data.GI.Base.GArray as B.GArray
import qualified Data.GI.Base.GClosure as B.GClosure
import qualified Data.GI.Base.GError as B.GError
import qualified Data.GI.Base.GHashTable as B.GHT
import qualified Data.GI.Base.GVariant as B.GVariant
import qualified Data.GI.Base.GValue as B.GValue
import qualified Data.GI.Base.GParamSpec as B.GParamSpec
import qualified Data.GI.Base.CallStack as B.CallStack
import qualified Data.GI.Base.Properties as B.Properties
import qualified Data.GI.Base.Signals as B.Signals
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Coerce as Coerce
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Foreign.Ptr as FP
import qualified GHC.OverloadedLabels as OL
import qualified GHC.Records as R

import qualified GI.GLib.Structs.String as GLib.String

-- | Memory-managed wrapper type.
newtype RcProperty = RcProperty (SP.ManagedPtr RcProperty)
    deriving (Eq)

instance SP.ManagedPtrNewtype RcProperty where
    toManagedPtr (RcProperty p) = p

instance BoxedPtr RcProperty where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 40 >=> B.ManagedPtr.wrapPtr RcProperty)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr RcProperty where
    boxedPtrCalloc = callocBytes 40


-- | Construct a `RcProperty` struct initialized to zero.
newZeroRcProperty :: MonadIO m => m RcProperty
newZeroRcProperty = liftIO $ boxedPtrCalloc >>= wrapPtr RcProperty

instance tag ~ 'AttrSet => Constructible RcProperty tag where
    new _ attrs = do
        o <- newZeroRcProperty
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@type_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' rcProperty #typeName
-- @
getRcPropertyTypeName :: MonadIO m => RcProperty -> m Word32
getRcPropertyTypeName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO Word32
    return val

-- | Set the value of the “@type_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' rcProperty [ #typeName 'Data.GI.Base.Attributes.:=' value ]
-- @
setRcPropertyTypeName :: MonadIO m => RcProperty -> Word32 -> m ()
setRcPropertyTypeName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data RcPropertyTypeNameFieldInfo
instance AttrInfo RcPropertyTypeNameFieldInfo where
    type AttrBaseTypeConstraint RcPropertyTypeNameFieldInfo = (~) RcProperty
    type AttrAllowedOps RcPropertyTypeNameFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RcPropertyTypeNameFieldInfo = (~) Word32
    type AttrTransferTypeConstraint RcPropertyTypeNameFieldInfo = (~)Word32
    type AttrTransferType RcPropertyTypeNameFieldInfo = Word32
    type AttrGetType RcPropertyTypeNameFieldInfo = Word32
    type AttrLabel RcPropertyTypeNameFieldInfo = "type_name"
    type AttrOrigin RcPropertyTypeNameFieldInfo = RcProperty
    attrGet = getRcPropertyTypeName
    attrSet = setRcPropertyTypeName
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RcProperty.typeName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RcProperty.html#g:attr:typeName"
        })

rcProperty_typeName :: AttrLabelProxy "typeName"
rcProperty_typeName = AttrLabelProxy

#endif


-- | Get the value of the “@property_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' rcProperty #propertyName
-- @
getRcPropertyPropertyName :: MonadIO m => RcProperty -> m Word32
getRcPropertyPropertyName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 4) :: IO Word32
    return val

-- | Set the value of the “@property_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' rcProperty [ #propertyName 'Data.GI.Base.Attributes.:=' value ]
-- @
setRcPropertyPropertyName :: MonadIO m => RcProperty -> Word32 -> m ()
setRcPropertyPropertyName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 4) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data RcPropertyPropertyNameFieldInfo
instance AttrInfo RcPropertyPropertyNameFieldInfo where
    type AttrBaseTypeConstraint RcPropertyPropertyNameFieldInfo = (~) RcProperty
    type AttrAllowedOps RcPropertyPropertyNameFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RcPropertyPropertyNameFieldInfo = (~) Word32
    type AttrTransferTypeConstraint RcPropertyPropertyNameFieldInfo = (~)Word32
    type AttrTransferType RcPropertyPropertyNameFieldInfo = Word32
    type AttrGetType RcPropertyPropertyNameFieldInfo = Word32
    type AttrLabel RcPropertyPropertyNameFieldInfo = "property_name"
    type AttrOrigin RcPropertyPropertyNameFieldInfo = RcProperty
    attrGet = getRcPropertyPropertyName
    attrSet = setRcPropertyPropertyName
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RcProperty.propertyName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RcProperty.html#g:attr:propertyName"
        })

rcProperty_propertyName :: AttrLabelProxy "propertyName"
rcProperty_propertyName = AttrLabelProxy

#endif


-- | Get the value of the “@origin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' rcProperty #origin
-- @
getRcPropertyOrigin :: MonadIO m => RcProperty -> m (Maybe T.Text)
getRcPropertyOrigin s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@origin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' rcProperty [ #origin 'Data.GI.Base.Attributes.:=' value ]
-- @
setRcPropertyOrigin :: MonadIO m => RcProperty -> CString -> m ()
setRcPropertyOrigin s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@origin@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #origin
-- @
clearRcPropertyOrigin :: MonadIO m => RcProperty -> m ()
clearRcPropertyOrigin s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RcPropertyOriginFieldInfo
instance AttrInfo RcPropertyOriginFieldInfo where
    type AttrBaseTypeConstraint RcPropertyOriginFieldInfo = (~) RcProperty
    type AttrAllowedOps RcPropertyOriginFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RcPropertyOriginFieldInfo = (~) CString
    type AttrTransferTypeConstraint RcPropertyOriginFieldInfo = (~)CString
    type AttrTransferType RcPropertyOriginFieldInfo = CString
    type AttrGetType RcPropertyOriginFieldInfo = Maybe T.Text
    type AttrLabel RcPropertyOriginFieldInfo = "origin"
    type AttrOrigin RcPropertyOriginFieldInfo = RcProperty
    attrGet = getRcPropertyOrigin
    attrSet = setRcPropertyOrigin
    attrConstruct = undefined
    attrClear = clearRcPropertyOrigin
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RcProperty.origin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RcProperty.html#g:attr:origin"
        })

rcProperty_origin :: AttrLabelProxy "origin"
rcProperty_origin = AttrLabelProxy

#endif


-- | Get the value of the “@value@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' rcProperty #value
-- @
getRcPropertyValue :: MonadIO m => RcProperty -> m (Maybe GValue)
getRcPropertyValue s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO (Ptr GValue)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- B.GValue.newGValueFromPtr val'
        return val''
    return result

-- | Set the value of the “@value@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' rcProperty [ #value 'Data.GI.Base.Attributes.:=' value ]
-- @
setRcPropertyValue :: MonadIO m => RcProperty -> Ptr GValue -> m ()
setRcPropertyValue s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: Ptr GValue)

-- | Set the value of the “@value@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #value
-- @
clearRcPropertyValue :: MonadIO m => RcProperty -> m ()
clearRcPropertyValue s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: Ptr GValue)

#if defined(ENABLE_OVERLOADING)
data RcPropertyValueFieldInfo
instance AttrInfo RcPropertyValueFieldInfo where
    type AttrBaseTypeConstraint RcPropertyValueFieldInfo = (~) RcProperty
    type AttrAllowedOps RcPropertyValueFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RcPropertyValueFieldInfo = (~) (Ptr GValue)
    type AttrTransferTypeConstraint RcPropertyValueFieldInfo = (~)(Ptr GValue)
    type AttrTransferType RcPropertyValueFieldInfo = (Ptr GValue)
    type AttrGetType RcPropertyValueFieldInfo = Maybe GValue
    type AttrLabel RcPropertyValueFieldInfo = "value"
    type AttrOrigin RcPropertyValueFieldInfo = RcProperty
    attrGet = getRcPropertyValue
    attrSet = setRcPropertyValue
    attrConstruct = undefined
    attrClear = clearRcPropertyValue
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RcProperty.value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RcProperty.html#g:attr:value"
        })

rcProperty_value :: AttrLabelProxy "value"
rcProperty_value = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RcProperty
type instance O.AttributeList RcProperty = RcPropertyAttributeList
type RcPropertyAttributeList = ('[ '("typeName", RcPropertyTypeNameFieldInfo), '("propertyName", RcPropertyPropertyNameFieldInfo), '("origin", RcPropertyOriginFieldInfo), '("value", RcPropertyValueFieldInfo)] :: [(Symbol, *)])
#endif

-- method RcProperty::parse_border
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GParamSpec" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gstring"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "String" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GString to be parsed"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GValue which must hold boxed values."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_property_parse_border" gtk_rc_property_parse_border :: 
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    Ptr GLib.String.String ->               -- gstring : TInterface (Name {namespace = "GLib", name = "String"})
    Ptr GValue ->                           -- property_value : TGValue
    IO CInt

-- | A t'GI.Gtk.Callbacks.RcPropertyParser' for use with 'GI.Gtk.Objects.Settings.settingsInstallPropertyParser'
-- or @/gtk_widget_class_install_style_property_parser()/@ which parses
-- borders in the form
-- @\"{ left, right, top, bottom }\"@ for integers
-- left, right, top and bottom.
rcPropertyParseBorder ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GParamSpec
    -- ^ /@pspec@/: a t'GI.GObject.Objects.ParamSpec.ParamSpec'
    -> GLib.String.String
    -- ^ /@gstring@/: the t'GI.GLib.Structs.String.String' to be parsed
    -> GValue
    -- ^ /@propertyValue@/: a t'GI.GObject.Structs.Value.Value' which must hold boxed values.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@gstring@/ could be parsed and /@propertyValue@/
    -- has been set to the resulting t'GI.Gtk.Structs.Border.Border'.
rcPropertyParseBorder pspec gstring propertyValue = liftIO $ do
    pspec' <- unsafeManagedPtrGetPtr pspec
    gstring' <- unsafeManagedPtrGetPtr gstring
    propertyValue' <- unsafeManagedPtrGetPtr propertyValue
    result <- gtk_rc_property_parse_border pspec' gstring' propertyValue'
    let result' = (/= 0) result
    touchManagedPtr pspec
    touchManagedPtr gstring
    touchManagedPtr propertyValue
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RcProperty::parse_color
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GParamSpec" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gstring"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "String" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GString to be parsed"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GValue which must hold #GdkColor values."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_property_parse_color" gtk_rc_property_parse_color :: 
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    Ptr GLib.String.String ->               -- gstring : TInterface (Name {namespace = "GLib", name = "String"})
    Ptr GValue ->                           -- property_value : TGValue
    IO CInt

-- | A t'GI.Gtk.Callbacks.RcPropertyParser' for use with 'GI.Gtk.Objects.Settings.settingsInstallPropertyParser'
-- or @/gtk_widget_class_install_style_property_parser()/@ which parses a
-- color given either by its name or in the form
-- @{ red, green, blue }@ where red, green and
-- blue are integers between 0 and 65535 or floating-point numbers
-- between 0 and 1.
rcPropertyParseColor ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GParamSpec
    -- ^ /@pspec@/: a t'GI.GObject.Objects.ParamSpec.ParamSpec'
    -> GLib.String.String
    -- ^ /@gstring@/: the t'GI.GLib.Structs.String.String' to be parsed
    -> GValue
    -- ^ /@propertyValue@/: a t'GI.GObject.Structs.Value.Value' which must hold t'GI.Gdk.Structs.Color.Color' values.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@gstring@/ could be parsed and /@propertyValue@/
    -- has been set to the resulting t'GI.Gdk.Structs.Color.Color'.
rcPropertyParseColor pspec gstring propertyValue = liftIO $ do
    pspec' <- unsafeManagedPtrGetPtr pspec
    gstring' <- unsafeManagedPtrGetPtr gstring
    propertyValue' <- unsafeManagedPtrGetPtr propertyValue
    result <- gtk_rc_property_parse_color pspec' gstring' propertyValue'
    let result' = (/= 0) result
    touchManagedPtr pspec
    touchManagedPtr gstring
    touchManagedPtr propertyValue
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RcProperty::parse_enum
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GParamSpec" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gstring"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "String" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GString to be parsed"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GValue which must hold enum values."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_property_parse_enum" gtk_rc_property_parse_enum :: 
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    Ptr GLib.String.String ->               -- gstring : TInterface (Name {namespace = "GLib", name = "String"})
    Ptr GValue ->                           -- property_value : TGValue
    IO CInt

-- | A t'GI.Gtk.Callbacks.RcPropertyParser' for use with 'GI.Gtk.Objects.Settings.settingsInstallPropertyParser'
-- or @/gtk_widget_class_install_style_property_parser()/@ which parses a single
-- enumeration value.
-- 
-- The enumeration value can be specified by its name, its nickname or
-- its numeric value. For consistency with flags parsing, the value
-- may be surrounded by parentheses.
rcPropertyParseEnum ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GParamSpec
    -- ^ /@pspec@/: a t'GI.GObject.Objects.ParamSpec.ParamSpec'
    -> GLib.String.String
    -- ^ /@gstring@/: the t'GI.GLib.Structs.String.String' to be parsed
    -> GValue
    -- ^ /@propertyValue@/: a t'GI.GObject.Structs.Value.Value' which must hold enum values.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@gstring@/ could be parsed and /@propertyValue@/
    -- has been set to the resulting t'GI.GObject.Structs.EnumValue.EnumValue'.
rcPropertyParseEnum pspec gstring propertyValue = liftIO $ do
    pspec' <- unsafeManagedPtrGetPtr pspec
    gstring' <- unsafeManagedPtrGetPtr gstring
    propertyValue' <- unsafeManagedPtrGetPtr propertyValue
    result <- gtk_rc_property_parse_enum pspec' gstring' propertyValue'
    let result' = (/= 0) result
    touchManagedPtr pspec
    touchManagedPtr gstring
    touchManagedPtr propertyValue
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RcProperty::parse_flags
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GParamSpec" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gstring"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "String" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GString to be parsed"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GValue which must hold flags values."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_property_parse_flags" gtk_rc_property_parse_flags :: 
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    Ptr GLib.String.String ->               -- gstring : TInterface (Name {namespace = "GLib", name = "String"})
    Ptr GValue ->                           -- property_value : TGValue
    IO CInt

-- | A t'GI.Gtk.Callbacks.RcPropertyParser' for use with 'GI.Gtk.Objects.Settings.settingsInstallPropertyParser'
-- or @/gtk_widget_class_install_style_property_parser()/@ which parses flags.
-- 
-- Flags can be specified by their name, their nickname or
-- numerically. Multiple flags can be specified in the form
-- @\"( flag1 | flag2 | ... )\"@.
rcPropertyParseFlags ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GParamSpec
    -- ^ /@pspec@/: a t'GI.GObject.Objects.ParamSpec.ParamSpec'
    -> GLib.String.String
    -- ^ /@gstring@/: the t'GI.GLib.Structs.String.String' to be parsed
    -> GValue
    -- ^ /@propertyValue@/: a t'GI.GObject.Structs.Value.Value' which must hold flags values.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@gstring@/ could be parsed and /@propertyValue@/
    -- has been set to the resulting flags value.
rcPropertyParseFlags pspec gstring propertyValue = liftIO $ do
    pspec' <- unsafeManagedPtrGetPtr pspec
    gstring' <- unsafeManagedPtrGetPtr gstring
    propertyValue' <- unsafeManagedPtrGetPtr propertyValue
    result <- gtk_rc_property_parse_flags pspec' gstring' propertyValue'
    let result' = (/= 0) result
    touchManagedPtr pspec
    touchManagedPtr gstring
    touchManagedPtr propertyValue
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RcProperty::parse_requisition
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GParamSpec" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gstring"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "String" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GString to be parsed"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GValue which must hold boxed values."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_property_parse_requisition" gtk_rc_property_parse_requisition :: 
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    Ptr GLib.String.String ->               -- gstring : TInterface (Name {namespace = "GLib", name = "String"})
    Ptr GValue ->                           -- property_value : TGValue
    IO CInt

-- | A t'GI.Gtk.Callbacks.RcPropertyParser' for use with 'GI.Gtk.Objects.Settings.settingsInstallPropertyParser'
-- or @/gtk_widget_class_install_style_property_parser()/@ which parses a
-- requisition in the form
-- @\"{ width, height }\"@ for integers @/width/@ and @/height/@.
rcPropertyParseRequisition ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GParamSpec
    -- ^ /@pspec@/: a t'GI.GObject.Objects.ParamSpec.ParamSpec'
    -> GLib.String.String
    -- ^ /@gstring@/: the t'GI.GLib.Structs.String.String' to be parsed
    -> GValue
    -- ^ /@propertyValue@/: a t'GI.GObject.Structs.Value.Value' which must hold boxed values.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@gstring@/ could be parsed and /@propertyValue@/
    -- has been set to the resulting t'GI.Gtk.Structs.Requisition.Requisition'.
rcPropertyParseRequisition pspec gstring propertyValue = liftIO $ do
    pspec' <- unsafeManagedPtrGetPtr pspec
    gstring' <- unsafeManagedPtrGetPtr gstring
    propertyValue' <- unsafeManagedPtrGetPtr propertyValue
    result <- gtk_rc_property_parse_requisition pspec' gstring' propertyValue'
    let result' = (/= 0) result
    touchManagedPtr pspec
    touchManagedPtr gstring
    touchManagedPtr propertyValue
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveRcPropertyMethod (t :: Symbol) (o :: *) :: * where
    ResolveRcPropertyMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRcPropertyMethod t RcProperty, O.OverloadedMethod info RcProperty p) => OL.IsLabel t (RcProperty -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRcPropertyMethod t RcProperty, O.OverloadedMethod info RcProperty p, R.HasField t RcProperty p) => R.HasField t RcProperty p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRcPropertyMethod t RcProperty, O.OverloadedMethodInfo info RcProperty) => OL.IsLabel t (O.MethodProxy info RcProperty) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


