{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkNumerableIcon is a subclass of t'GI.Gio.Objects.EmblemedIcon.EmblemedIcon' that can
-- show a number or short string as an emblem. The number can
-- be overlayed on top of another emblem, if desired.
-- 
-- It supports theming by taking font and color information
-- from a provided t'GI.Gtk.Objects.StyleContext.StyleContext'; see
-- 'GI.Gtk.Objects.NumerableIcon.numerableIconSetStyleContext'.
-- 
-- Typical numerable icons:
-- <<https://developer.gnome.org/gtk3/stable/numerableicon.png>>
-- <<https://developer.gnome.org/gtk3/stable/numerableicon2.png>>

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.NumerableIcon
    ( 

-- * Exported types
    NumerableIcon(..)                       ,
    IsNumerableIcon                         ,
    toNumerableIcon                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addEmblem]("GI.Gio.Objects.EmblemedIcon#g:method:addEmblem"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [clearEmblems]("GI.Gio.Objects.EmblemedIcon#g:method:clearEmblems"), [equal]("GI.Gio.Interfaces.Icon#g:method:equal"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [serialize]("GI.Gio.Interfaces.Icon#g:method:serialize"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toString]("GI.Gio.Interfaces.Icon#g:method:toString"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBackgroundGicon]("GI.Gtk.Objects.NumerableIcon#g:method:getBackgroundGicon"), [getBackgroundIconName]("GI.Gtk.Objects.NumerableIcon#g:method:getBackgroundIconName"), [getCount]("GI.Gtk.Objects.NumerableIcon#g:method:getCount"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getEmblems]("GI.Gio.Objects.EmblemedIcon#g:method:getEmblems"), [getIcon]("GI.Gio.Objects.EmblemedIcon#g:method:getIcon"), [getLabel]("GI.Gtk.Objects.NumerableIcon#g:method:getLabel"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getStyleContext]("GI.Gtk.Objects.NumerableIcon#g:method:getStyleContext").
-- 
-- ==== Setters
-- [setBackgroundGicon]("GI.Gtk.Objects.NumerableIcon#g:method:setBackgroundGicon"), [setBackgroundIconName]("GI.Gtk.Objects.NumerableIcon#g:method:setBackgroundIconName"), [setCount]("GI.Gtk.Objects.NumerableIcon#g:method:setCount"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setLabel]("GI.Gtk.Objects.NumerableIcon#g:method:setLabel"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setStyleContext]("GI.Gtk.Objects.NumerableIcon#g:method:setStyleContext").

#if defined(ENABLE_OVERLOADING)
    ResolveNumerableIconMethod              ,
#endif

-- ** getBackgroundGicon #method:getBackgroundGicon#

#if defined(ENABLE_OVERLOADING)
    NumerableIconGetBackgroundGiconMethodInfo,
#endif
    numerableIconGetBackgroundGicon         ,


-- ** getBackgroundIconName #method:getBackgroundIconName#

#if defined(ENABLE_OVERLOADING)
    NumerableIconGetBackgroundIconNameMethodInfo,
#endif
    numerableIconGetBackgroundIconName      ,


-- ** getCount #method:getCount#

#if defined(ENABLE_OVERLOADING)
    NumerableIconGetCountMethodInfo         ,
#endif
    numerableIconGetCount                   ,


-- ** getLabel #method:getLabel#

#if defined(ENABLE_OVERLOADING)
    NumerableIconGetLabelMethodInfo         ,
#endif
    numerableIconGetLabel                   ,


-- ** getStyleContext #method:getStyleContext#

#if defined(ENABLE_OVERLOADING)
    NumerableIconGetStyleContextMethodInfo  ,
#endif
    numerableIconGetStyleContext            ,


-- ** new #method:new#

    numerableIconNew                        ,


-- ** newWithStyleContext #method:newWithStyleContext#

    numerableIconNewWithStyleContext        ,


-- ** setBackgroundGicon #method:setBackgroundGicon#

#if defined(ENABLE_OVERLOADING)
    NumerableIconSetBackgroundGiconMethodInfo,
#endif
    numerableIconSetBackgroundGicon         ,


-- ** setBackgroundIconName #method:setBackgroundIconName#

#if defined(ENABLE_OVERLOADING)
    NumerableIconSetBackgroundIconNameMethodInfo,
#endif
    numerableIconSetBackgroundIconName      ,


-- ** setCount #method:setCount#

#if defined(ENABLE_OVERLOADING)
    NumerableIconSetCountMethodInfo         ,
#endif
    numerableIconSetCount                   ,


-- ** setLabel #method:setLabel#

#if defined(ENABLE_OVERLOADING)
    NumerableIconSetLabelMethodInfo         ,
#endif
    numerableIconSetLabel                   ,


-- ** setStyleContext #method:setStyleContext#

#if defined(ENABLE_OVERLOADING)
    NumerableIconSetStyleContextMethodInfo  ,
#endif
    numerableIconSetStyleContext            ,




 -- * Properties


-- ** backgroundIcon #attr:backgroundIcon#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NumerableIconBackgroundIconPropertyInfo ,
#endif
    clearNumerableIconBackgroundIcon        ,
    constructNumerableIconBackgroundIcon    ,
    getNumerableIconBackgroundIcon          ,
#if defined(ENABLE_OVERLOADING)
    numerableIconBackgroundIcon             ,
#endif
    setNumerableIconBackgroundIcon          ,


-- ** backgroundIconName #attr:backgroundIconName#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NumerableIconBackgroundIconNamePropertyInfo,
#endif
    clearNumerableIconBackgroundIconName    ,
    constructNumerableIconBackgroundIconName,
    getNumerableIconBackgroundIconName      ,
#if defined(ENABLE_OVERLOADING)
    numerableIconBackgroundIconName         ,
#endif
    setNumerableIconBackgroundIconName      ,


-- ** count #attr:count#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NumerableIconCountPropertyInfo          ,
#endif
    constructNumerableIconCount             ,
    getNumerableIconCount                   ,
#if defined(ENABLE_OVERLOADING)
    numerableIconCount                      ,
#endif
    setNumerableIconCount                   ,


-- ** label #attr:label#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NumerableIconLabelPropertyInfo          ,
#endif
    clearNumerableIconLabel                 ,
    constructNumerableIconLabel             ,
    getNumerableIconLabel                   ,
#if defined(ENABLE_OVERLOADING)
    numerableIconLabel                      ,
#endif
    setNumerableIconLabel                   ,


-- ** styleContext #attr:styleContext#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NumerableIconStyleContextPropertyInfo   ,
#endif
    constructNumerableIconStyleContext      ,
    getNumerableIconStyleContext            ,
#if defined(ENABLE_OVERLOADING)
    numerableIconStyleContext               ,
#endif
    setNumerableIconStyleContext            ,




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

import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gio.Interfaces.Icon as Gio.Icon
import qualified GI.Gio.Objects.EmblemedIcon as Gio.EmblemedIcon
import {-# SOURCE #-} qualified GI.Gtk.Objects.StyleContext as Gtk.StyleContext

-- | Memory-managed wrapper type.
newtype NumerableIcon = NumerableIcon (SP.ManagedPtr NumerableIcon)
    deriving (Eq)

instance SP.ManagedPtrNewtype NumerableIcon where
    toManagedPtr (NumerableIcon p) = p

foreign import ccall "gtk_numerable_icon_get_type"
    c_gtk_numerable_icon_get_type :: IO B.Types.GType

instance B.Types.TypedObject NumerableIcon where
    glibType = c_gtk_numerable_icon_get_type

instance B.Types.GObject NumerableIcon

-- | Type class for types which can be safely cast to `NumerableIcon`, for instance with `toNumerableIcon`.
class (SP.GObject o, O.IsDescendantOf NumerableIcon o) => IsNumerableIcon o
instance (SP.GObject o, O.IsDescendantOf NumerableIcon o) => IsNumerableIcon o

instance O.HasParentTypes NumerableIcon
type instance O.ParentTypes NumerableIcon = '[Gio.EmblemedIcon.EmblemedIcon, GObject.Object.Object, Gio.Icon.Icon]

-- | Cast to `NumerableIcon`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toNumerableIcon :: (MIO.MonadIO m, IsNumerableIcon o) => o -> m NumerableIcon
toNumerableIcon = MIO.liftIO . B.ManagedPtr.unsafeCastTo NumerableIcon

-- | Convert 'NumerableIcon' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe NumerableIcon) where
    gvalueGType_ = c_gtk_numerable_icon_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr NumerableIcon)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr NumerableIcon)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject NumerableIcon ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveNumerableIconMethod (t :: Symbol) (o :: *) :: * where
    ResolveNumerableIconMethod "addEmblem" o = Gio.EmblemedIcon.EmblemedIconAddEmblemMethodInfo
    ResolveNumerableIconMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveNumerableIconMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveNumerableIconMethod "clearEmblems" o = Gio.EmblemedIcon.EmblemedIconClearEmblemsMethodInfo
    ResolveNumerableIconMethod "equal" o = Gio.Icon.IconEqualMethodInfo
    ResolveNumerableIconMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveNumerableIconMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveNumerableIconMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveNumerableIconMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveNumerableIconMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveNumerableIconMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveNumerableIconMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveNumerableIconMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveNumerableIconMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveNumerableIconMethod "serialize" o = Gio.Icon.IconSerializeMethodInfo
    ResolveNumerableIconMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveNumerableIconMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveNumerableIconMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveNumerableIconMethod "toString" o = Gio.Icon.IconToStringMethodInfo
    ResolveNumerableIconMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveNumerableIconMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveNumerableIconMethod "getBackgroundGicon" o = NumerableIconGetBackgroundGiconMethodInfo
    ResolveNumerableIconMethod "getBackgroundIconName" o = NumerableIconGetBackgroundIconNameMethodInfo
    ResolveNumerableIconMethod "getCount" o = NumerableIconGetCountMethodInfo
    ResolveNumerableIconMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveNumerableIconMethod "getEmblems" o = Gio.EmblemedIcon.EmblemedIconGetEmblemsMethodInfo
    ResolveNumerableIconMethod "getIcon" o = Gio.EmblemedIcon.EmblemedIconGetIconMethodInfo
    ResolveNumerableIconMethod "getLabel" o = NumerableIconGetLabelMethodInfo
    ResolveNumerableIconMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveNumerableIconMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveNumerableIconMethod "getStyleContext" o = NumerableIconGetStyleContextMethodInfo
    ResolveNumerableIconMethod "setBackgroundGicon" o = NumerableIconSetBackgroundGiconMethodInfo
    ResolveNumerableIconMethod "setBackgroundIconName" o = NumerableIconSetBackgroundIconNameMethodInfo
    ResolveNumerableIconMethod "setCount" o = NumerableIconSetCountMethodInfo
    ResolveNumerableIconMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveNumerableIconMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveNumerableIconMethod "setLabel" o = NumerableIconSetLabelMethodInfo
    ResolveNumerableIconMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveNumerableIconMethod "setStyleContext" o = NumerableIconSetStyleContextMethodInfo
    ResolveNumerableIconMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveNumerableIconMethod t NumerableIcon, O.OverloadedMethod info NumerableIcon p) => OL.IsLabel t (NumerableIcon -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveNumerableIconMethod t NumerableIcon, O.OverloadedMethod info NumerableIcon p, R.HasField t NumerableIcon p) => R.HasField t NumerableIcon p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveNumerableIconMethod t NumerableIcon, O.OverloadedMethodInfo info NumerableIcon) => OL.IsLabel t (O.MethodProxy info NumerableIcon) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "background-icon"
   -- Type: TInterface (Name {namespace = "Gio", name = "Icon"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@background-icon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' numerableIcon #backgroundIcon
-- @
getNumerableIconBackgroundIcon :: (MonadIO m, IsNumerableIcon o) => o -> m (Maybe Gio.Icon.Icon)
getNumerableIconBackgroundIcon obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "background-icon" Gio.Icon.Icon

-- | Set the value of the “@background-icon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' numerableIcon [ #backgroundIcon 'Data.GI.Base.Attributes.:=' value ]
-- @
setNumerableIconBackgroundIcon :: (MonadIO m, IsNumerableIcon o, Gio.Icon.IsIcon a) => o -> a -> m ()
setNumerableIconBackgroundIcon obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "background-icon" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@background-icon@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNumerableIconBackgroundIcon :: (IsNumerableIcon o, MIO.MonadIO m, Gio.Icon.IsIcon a) => a -> m (GValueConstruct o)
constructNumerableIconBackgroundIcon val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "background-icon" (P.Just val)

-- | Set the value of the “@background-icon@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #backgroundIcon
-- @
clearNumerableIconBackgroundIcon :: (MonadIO m, IsNumerableIcon o) => o -> m ()
clearNumerableIconBackgroundIcon obj = liftIO $ B.Properties.setObjectPropertyObject obj "background-icon" (Nothing :: Maybe Gio.Icon.Icon)

#if defined(ENABLE_OVERLOADING)
data NumerableIconBackgroundIconPropertyInfo
instance AttrInfo NumerableIconBackgroundIconPropertyInfo where
    type AttrAllowedOps NumerableIconBackgroundIconPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint NumerableIconBackgroundIconPropertyInfo = IsNumerableIcon
    type AttrSetTypeConstraint NumerableIconBackgroundIconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferTypeConstraint NumerableIconBackgroundIconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferType NumerableIconBackgroundIconPropertyInfo = Gio.Icon.Icon
    type AttrGetType NumerableIconBackgroundIconPropertyInfo = (Maybe Gio.Icon.Icon)
    type AttrLabel NumerableIconBackgroundIconPropertyInfo = "background-icon"
    type AttrOrigin NumerableIconBackgroundIconPropertyInfo = NumerableIcon
    attrGet = getNumerableIconBackgroundIcon
    attrSet = setNumerableIconBackgroundIcon
    attrTransfer _ v = do
        unsafeCastTo Gio.Icon.Icon v
    attrConstruct = constructNumerableIconBackgroundIcon
    attrClear = clearNumerableIconBackgroundIcon
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.backgroundIcon"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#g:attr:backgroundIcon"
        })
#endif

-- VVV Prop "background-icon-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@background-icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' numerableIcon #backgroundIconName
-- @
getNumerableIconBackgroundIconName :: (MonadIO m, IsNumerableIcon o) => o -> m (Maybe T.Text)
getNumerableIconBackgroundIconName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "background-icon-name"

-- | Set the value of the “@background-icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' numerableIcon [ #backgroundIconName 'Data.GI.Base.Attributes.:=' value ]
-- @
setNumerableIconBackgroundIconName :: (MonadIO m, IsNumerableIcon o) => o -> T.Text -> m ()
setNumerableIconBackgroundIconName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "background-icon-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@background-icon-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNumerableIconBackgroundIconName :: (IsNumerableIcon o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructNumerableIconBackgroundIconName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "background-icon-name" (P.Just val)

-- | Set the value of the “@background-icon-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #backgroundIconName
-- @
clearNumerableIconBackgroundIconName :: (MonadIO m, IsNumerableIcon o) => o -> m ()
clearNumerableIconBackgroundIconName obj = liftIO $ B.Properties.setObjectPropertyString obj "background-icon-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data NumerableIconBackgroundIconNamePropertyInfo
instance AttrInfo NumerableIconBackgroundIconNamePropertyInfo where
    type AttrAllowedOps NumerableIconBackgroundIconNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint NumerableIconBackgroundIconNamePropertyInfo = IsNumerableIcon
    type AttrSetTypeConstraint NumerableIconBackgroundIconNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint NumerableIconBackgroundIconNamePropertyInfo = (~) T.Text
    type AttrTransferType NumerableIconBackgroundIconNamePropertyInfo = T.Text
    type AttrGetType NumerableIconBackgroundIconNamePropertyInfo = (Maybe T.Text)
    type AttrLabel NumerableIconBackgroundIconNamePropertyInfo = "background-icon-name"
    type AttrOrigin NumerableIconBackgroundIconNamePropertyInfo = NumerableIcon
    attrGet = getNumerableIconBackgroundIconName
    attrSet = setNumerableIconBackgroundIconName
    attrTransfer _ v = do
        return v
    attrConstruct = constructNumerableIconBackgroundIconName
    attrClear = clearNumerableIconBackgroundIconName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.backgroundIconName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#g:attr:backgroundIconName"
        })
#endif

-- VVV Prop "count"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@count@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' numerableIcon #count
-- @
getNumerableIconCount :: (MonadIO m, IsNumerableIcon o) => o -> m Int32
getNumerableIconCount obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "count"

-- | Set the value of the “@count@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' numerableIcon [ #count 'Data.GI.Base.Attributes.:=' value ]
-- @
setNumerableIconCount :: (MonadIO m, IsNumerableIcon o) => o -> Int32 -> m ()
setNumerableIconCount obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "count" val

-- | Construct a `GValueConstruct` with valid value for the “@count@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNumerableIconCount :: (IsNumerableIcon o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructNumerableIconCount val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "count" val

#if defined(ENABLE_OVERLOADING)
data NumerableIconCountPropertyInfo
instance AttrInfo NumerableIconCountPropertyInfo where
    type AttrAllowedOps NumerableIconCountPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NumerableIconCountPropertyInfo = IsNumerableIcon
    type AttrSetTypeConstraint NumerableIconCountPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint NumerableIconCountPropertyInfo = (~) Int32
    type AttrTransferType NumerableIconCountPropertyInfo = Int32
    type AttrGetType NumerableIconCountPropertyInfo = Int32
    type AttrLabel NumerableIconCountPropertyInfo = "count"
    type AttrOrigin NumerableIconCountPropertyInfo = NumerableIcon
    attrGet = getNumerableIconCount
    attrSet = setNumerableIconCount
    attrTransfer _ v = do
        return v
    attrConstruct = constructNumerableIconCount
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.count"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#g:attr:count"
        })
#endif

-- VVV Prop "label"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' numerableIcon #label
-- @
getNumerableIconLabel :: (MonadIO m, IsNumerableIcon o) => o -> m (Maybe T.Text)
getNumerableIconLabel obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "label"

-- | Set the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' numerableIcon [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setNumerableIconLabel :: (MonadIO m, IsNumerableIcon o) => o -> T.Text -> m ()
setNumerableIconLabel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "label" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@label@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNumerableIconLabel :: (IsNumerableIcon o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructNumerableIconLabel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "label" (P.Just val)

-- | Set the value of the “@label@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #label
-- @
clearNumerableIconLabel :: (MonadIO m, IsNumerableIcon o) => o -> m ()
clearNumerableIconLabel obj = liftIO $ B.Properties.setObjectPropertyString obj "label" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data NumerableIconLabelPropertyInfo
instance AttrInfo NumerableIconLabelPropertyInfo where
    type AttrAllowedOps NumerableIconLabelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint NumerableIconLabelPropertyInfo = IsNumerableIcon
    type AttrSetTypeConstraint NumerableIconLabelPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint NumerableIconLabelPropertyInfo = (~) T.Text
    type AttrTransferType NumerableIconLabelPropertyInfo = T.Text
    type AttrGetType NumerableIconLabelPropertyInfo = (Maybe T.Text)
    type AttrLabel NumerableIconLabelPropertyInfo = "label"
    type AttrOrigin NumerableIconLabelPropertyInfo = NumerableIcon
    attrGet = getNumerableIconLabel
    attrSet = setNumerableIconLabel
    attrTransfer _ v = do
        return v
    attrConstruct = constructNumerableIconLabel
    attrClear = clearNumerableIconLabel
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#g:attr:label"
        })
#endif

-- VVV Prop "style-context"
   -- Type: TInterface (Name {namespace = "Gtk", name = "StyleContext"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@style-context@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' numerableIcon #styleContext
-- @
getNumerableIconStyleContext :: (MonadIO m, IsNumerableIcon o) => o -> m (Maybe Gtk.StyleContext.StyleContext)
getNumerableIconStyleContext obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "style-context" Gtk.StyleContext.StyleContext

-- | Set the value of the “@style-context@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' numerableIcon [ #styleContext 'Data.GI.Base.Attributes.:=' value ]
-- @
setNumerableIconStyleContext :: (MonadIO m, IsNumerableIcon o, Gtk.StyleContext.IsStyleContext a) => o -> a -> m ()
setNumerableIconStyleContext obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "style-context" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@style-context@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNumerableIconStyleContext :: (IsNumerableIcon o, MIO.MonadIO m, Gtk.StyleContext.IsStyleContext a) => a -> m (GValueConstruct o)
constructNumerableIconStyleContext val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "style-context" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data NumerableIconStyleContextPropertyInfo
instance AttrInfo NumerableIconStyleContextPropertyInfo where
    type AttrAllowedOps NumerableIconStyleContextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NumerableIconStyleContextPropertyInfo = IsNumerableIcon
    type AttrSetTypeConstraint NumerableIconStyleContextPropertyInfo = Gtk.StyleContext.IsStyleContext
    type AttrTransferTypeConstraint NumerableIconStyleContextPropertyInfo = Gtk.StyleContext.IsStyleContext
    type AttrTransferType NumerableIconStyleContextPropertyInfo = Gtk.StyleContext.StyleContext
    type AttrGetType NumerableIconStyleContextPropertyInfo = (Maybe Gtk.StyleContext.StyleContext)
    type AttrLabel NumerableIconStyleContextPropertyInfo = "style-context"
    type AttrOrigin NumerableIconStyleContextPropertyInfo = NumerableIcon
    attrGet = getNumerableIconStyleContext
    attrSet = setNumerableIconStyleContext
    attrTransfer _ v = do
        unsafeCastTo Gtk.StyleContext.StyleContext v
    attrConstruct = constructNumerableIconStyleContext
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.styleContext"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#g:attr:styleContext"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList NumerableIcon
type instance O.AttributeList NumerableIcon = NumerableIconAttributeList
type NumerableIconAttributeList = ('[ '("backgroundIcon", NumerableIconBackgroundIconPropertyInfo), '("backgroundIconName", NumerableIconBackgroundIconNamePropertyInfo), '("count", NumerableIconCountPropertyInfo), '("gicon", Gio.EmblemedIcon.EmblemedIconGiconPropertyInfo), '("label", NumerableIconLabelPropertyInfo), '("styleContext", NumerableIconStyleContextPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
numerableIconBackgroundIcon :: AttrLabelProxy "backgroundIcon"
numerableIconBackgroundIcon = AttrLabelProxy

numerableIconBackgroundIconName :: AttrLabelProxy "backgroundIconName"
numerableIconBackgroundIconName = AttrLabelProxy

numerableIconCount :: AttrLabelProxy "count"
numerableIconCount = AttrLabelProxy

numerableIconLabel :: AttrLabelProxy "label"
numerableIconLabel = AttrLabelProxy

numerableIconStyleContext :: AttrLabelProxy "styleContext"
numerableIconStyleContext = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList NumerableIcon = NumerableIconSignalList
type NumerableIconSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method NumerableIcon::get_background_gicon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
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
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "Icon" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_get_background_gicon" gtk_numerable_icon_get_background_gicon :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    IO (Ptr Gio.Icon.Icon)

{-# DEPRECATED numerableIconGetBackgroundGicon ["(Since version 3.14)"] #-}
-- | Returns the t'GI.Gio.Interfaces.Icon.Icon' that was set as the base background image, or
-- 'P.Nothing' if there’s none. The caller of this function does not own
-- a reference to the returned t'GI.Gio.Interfaces.Icon.Icon'.
-- 
-- /Since: 3.0/
numerableIconGetBackgroundGicon ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> m (Maybe Gio.Icon.Icon)
    -- ^ __Returns:__ a t'GI.Gio.Interfaces.Icon.Icon', or 'P.Nothing'
numerableIconGetBackgroundGicon self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_numerable_icon_get_background_gicon self'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gio.Icon.Icon) result'
        return result''
    touchManagedPtr self
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NumerableIconGetBackgroundGiconMethodInfo
instance (signature ~ (m (Maybe Gio.Icon.Icon)), MonadIO m, IsNumerableIcon a) => O.OverloadedMethod NumerableIconGetBackgroundGiconMethodInfo a signature where
    overloadedMethod = numerableIconGetBackgroundGicon

instance O.OverloadedMethodInfo NumerableIconGetBackgroundGiconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconGetBackgroundGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconGetBackgroundGicon"
        })


#endif

-- method NumerableIcon::get_background_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_get_background_icon_name" gtk_numerable_icon_get_background_icon_name :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    IO CString

{-# DEPRECATED numerableIconGetBackgroundIconName ["(Since version 3.14)"] #-}
-- | Returns the icon name used as the base background image,
-- or 'P.Nothing' if there’s none.
-- 
-- /Since: 3.0/
numerableIconGetBackgroundIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ an icon name, or 'P.Nothing'
numerableIconGetBackgroundIconName self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_numerable_icon_get_background_icon_name self'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr self
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NumerableIconGetBackgroundIconNameMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsNumerableIcon a) => O.OverloadedMethod NumerableIconGetBackgroundIconNameMethodInfo a signature where
    overloadedMethod = numerableIconGetBackgroundIconName

instance O.OverloadedMethodInfo NumerableIconGetBackgroundIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconGetBackgroundIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconGetBackgroundIconName"
        })


#endif

-- method NumerableIcon::get_count
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_get_count" gtk_numerable_icon_get_count :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    IO Int32

{-# DEPRECATED numerableIconGetCount ["(Since version 3.14)"] #-}
-- | Returns the value currently displayed by /@self@/.
-- 
-- /Since: 3.0/
numerableIconGetCount ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> m Int32
    -- ^ __Returns:__ the currently displayed value
numerableIconGetCount self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_numerable_icon_get_count self'
    touchManagedPtr self
    return result

#if defined(ENABLE_OVERLOADING)
data NumerableIconGetCountMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsNumerableIcon a) => O.OverloadedMethod NumerableIconGetCountMethodInfo a signature where
    overloadedMethod = numerableIconGetCount

instance O.OverloadedMethodInfo NumerableIconGetCountMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconGetCount",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconGetCount"
        })


#endif

-- method NumerableIcon::get_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_get_label" gtk_numerable_icon_get_label :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    IO CString

{-# DEPRECATED numerableIconGetLabel ["(Since version 3.14)"] #-}
-- | Returns the currently displayed label of the icon, or 'P.Nothing'.
-- 
-- /Since: 3.0/
numerableIconGetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the currently displayed label
numerableIconGetLabel self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_numerable_icon_get_label self'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr self
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NumerableIconGetLabelMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsNumerableIcon a) => O.OverloadedMethod NumerableIconGetLabelMethodInfo a signature where
    overloadedMethod = numerableIconGetLabel

instance O.OverloadedMethodInfo NumerableIconGetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconGetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconGetLabel"
        })


#endif

-- method NumerableIcon::get_style_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
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
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "StyleContext" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_get_style_context" gtk_numerable_icon_get_style_context :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    IO (Ptr Gtk.StyleContext.StyleContext)

{-# DEPRECATED numerableIconGetStyleContext ["(Since version 3.14)"] #-}
-- | Returns the t'GI.Gtk.Objects.StyleContext.StyleContext' used by the icon for theming,
-- or 'P.Nothing' if there’s none.
-- 
-- /Since: 3.0/
numerableIconGetStyleContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> m (Maybe Gtk.StyleContext.StyleContext)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.StyleContext.StyleContext', or 'P.Nothing'.
    --     This object is internal to GTK+ and should not be unreffed.
    --     Use 'GI.GObject.Objects.Object.objectRef' if you want to keep it around
numerableIconGetStyleContext self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_numerable_icon_get_style_context self'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.StyleContext.StyleContext) result'
        return result''
    touchManagedPtr self
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NumerableIconGetStyleContextMethodInfo
instance (signature ~ (m (Maybe Gtk.StyleContext.StyleContext)), MonadIO m, IsNumerableIcon a) => O.OverloadedMethod NumerableIconGetStyleContextMethodInfo a signature where
    overloadedMethod = numerableIconGetStyleContext

instance O.OverloadedMethodInfo NumerableIconGetStyleContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconGetStyleContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconGetStyleContext"
        })


#endif

-- method NumerableIcon::set_background_gicon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GIcon, or %NULL" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_set_background_gicon" gtk_numerable_icon_set_background_gicon :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    Ptr Gio.Icon.Icon ->                    -- icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    IO ()

{-# DEPRECATED numerableIconSetBackgroundGicon ["(Since version 3.14)"] #-}
-- | Updates the icon to use /@icon@/ as the base background image.
-- If /@icon@/ is 'P.Nothing', /@self@/ will go back using style information
-- or default theming for its background image.
-- 
-- If this method is called and an icon name was already set as
-- background for the icon, /@icon@/ will be used, i.e. the last method
-- called between 'GI.Gtk.Objects.NumerableIcon.numerableIconSetBackgroundGicon' and
-- 'GI.Gtk.Objects.NumerableIcon.numerableIconSetBackgroundIconName' has always priority.
-- 
-- /Since: 3.0/
numerableIconSetBackgroundGicon ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a, Gio.Icon.IsIcon b) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> Maybe (b)
    -- ^ /@icon@/: a t'GI.Gio.Interfaces.Icon.Icon', or 'P.Nothing'
    -> m ()
numerableIconSetBackgroundGicon self icon = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    maybeIcon <- case icon of
        Nothing -> return nullPtr
        Just jIcon -> do
            jIcon' <- unsafeManagedPtrCastPtr jIcon
            return jIcon'
    gtk_numerable_icon_set_background_gicon self' maybeIcon
    touchManagedPtr self
    whenJust icon touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data NumerableIconSetBackgroundGiconMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsNumerableIcon a, Gio.Icon.IsIcon b) => O.OverloadedMethod NumerableIconSetBackgroundGiconMethodInfo a signature where
    overloadedMethod = numerableIconSetBackgroundGicon

instance O.OverloadedMethodInfo NumerableIconSetBackgroundGiconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconSetBackgroundGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconSetBackgroundGicon"
        })


#endif

-- method NumerableIcon::set_background_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an icon name, or %NULL"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_set_background_icon_name" gtk_numerable_icon_set_background_icon_name :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    CString ->                              -- icon_name : TBasicType TUTF8
    IO ()

{-# DEPRECATED numerableIconSetBackgroundIconName ["(Since version 3.14)"] #-}
-- | Updates the icon to use the icon named /@iconName@/ from the
-- current icon theme as the base background image. If /@iconName@/
-- is 'P.Nothing', /@self@/ will go back using style information or default
-- theming for its background image.
-- 
-- If this method is called and a t'GI.Gio.Interfaces.Icon.Icon' was already set as
-- background for the icon, /@iconName@/ will be used, i.e. the
-- last method called between 'GI.Gtk.Objects.NumerableIcon.numerableIconSetBackgroundIconName'
-- and 'GI.Gtk.Objects.NumerableIcon.numerableIconSetBackgroundGicon' has always priority.
-- 
-- /Since: 3.0/
numerableIconSetBackgroundIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> Maybe (T.Text)
    -- ^ /@iconName@/: an icon name, or 'P.Nothing'
    -> m ()
numerableIconSetBackgroundIconName self iconName = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    maybeIconName <- case iconName of
        Nothing -> return nullPtr
        Just jIconName -> do
            jIconName' <- textToCString jIconName
            return jIconName'
    gtk_numerable_icon_set_background_icon_name self' maybeIconName
    touchManagedPtr self
    freeMem maybeIconName
    return ()

#if defined(ENABLE_OVERLOADING)
data NumerableIconSetBackgroundIconNameMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsNumerableIcon a) => O.OverloadedMethod NumerableIconSetBackgroundIconNameMethodInfo a signature where
    overloadedMethod = numerableIconSetBackgroundIconName

instance O.OverloadedMethodInfo NumerableIconSetBackgroundIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconSetBackgroundIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconSetBackgroundIconName"
        })


#endif

-- method NumerableIcon::set_count
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a number between -99 and 99"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_set_count" gtk_numerable_icon_set_count :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    Int32 ->                                -- count : TBasicType TInt
    IO ()

{-# DEPRECATED numerableIconSetCount ["(Since version 3.14)"] #-}
-- | Sets the currently displayed value of /@self@/ to /@count@/.
-- 
-- The numeric value is always clamped to make it two digits, i.e.
-- between -99 and 99. Setting a count of zero removes the emblem.
-- If this method is called, and a label was already set on the icon,
-- it will automatically be reset to 'P.Nothing' before rendering the number,
-- i.e. the last method called between 'GI.Gtk.Objects.NumerableIcon.numerableIconSetCount'
-- and 'GI.Gtk.Objects.NumerableIcon.numerableIconSetLabel' has always priority.
-- 
-- /Since: 3.0/
numerableIconSetCount ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> Int32
    -- ^ /@count@/: a number between -99 and 99
    -> m ()
numerableIconSetCount self count = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    gtk_numerable_icon_set_count self' count
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data NumerableIconSetCountMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsNumerableIcon a) => O.OverloadedMethod NumerableIconSetCountMethodInfo a signature where
    overloadedMethod = numerableIconSetCount

instance O.OverloadedMethodInfo NumerableIconSetCountMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconSetCount",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconSetCount"
        })


#endif

-- method NumerableIcon::set_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a short label, or %NULL"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_set_label" gtk_numerable_icon_set_label :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    CString ->                              -- label : TBasicType TUTF8
    IO ()

{-# DEPRECATED numerableIconSetLabel ["(Since version 3.14)"] #-}
-- | Sets the currently displayed value of /@self@/ to the string
-- in /@label@/. Setting an empty label removes the emblem.
-- 
-- Note that this is meant for displaying short labels, such as
-- roman numbers, or single letters. For roman numbers, consider
-- using the Unicode characters U+2160 - U+217F. Strings longer
-- than two characters will likely not be rendered very well.
-- 
-- If this method is called, and a number was already set on the
-- icon, it will automatically be reset to zero before rendering
-- the label, i.e. the last method called between
-- 'GI.Gtk.Objects.NumerableIcon.numerableIconSetLabel' and 'GI.Gtk.Objects.NumerableIcon.numerableIconSetCount'
-- has always priority.
-- 
-- /Since: 3.0/
numerableIconSetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> Maybe (T.Text)
    -- ^ /@label@/: a short label, or 'P.Nothing'
    -> m ()
numerableIconSetLabel self label = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    maybeLabel <- case label of
        Nothing -> return nullPtr
        Just jLabel -> do
            jLabel' <- textToCString jLabel
            return jLabel'
    gtk_numerable_icon_set_label self' maybeLabel
    touchManagedPtr self
    freeMem maybeLabel
    return ()

#if defined(ENABLE_OVERLOADING)
data NumerableIconSetLabelMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsNumerableIcon a) => O.OverloadedMethod NumerableIconSetLabelMethodInfo a signature where
    overloadedMethod = numerableIconSetLabel

instance O.OverloadedMethodInfo NumerableIconSetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconSetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconSetLabel"
        })


#endif

-- method NumerableIcon::set_style_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NumerableIcon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNumerableIcon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "style"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_set_style_context" gtk_numerable_icon_set_style_context :: 
    Ptr NumerableIcon ->                    -- self : TInterface (Name {namespace = "Gtk", name = "NumerableIcon"})
    Ptr Gtk.StyleContext.StyleContext ->    -- style : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO ()

{-# DEPRECATED numerableIconSetStyleContext ["(Since version 3.14)"] #-}
-- | Updates the icon to fetch theme information from the
-- given t'GI.Gtk.Objects.StyleContext.StyleContext'.
-- 
-- /Since: 3.0/
numerableIconSetStyleContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsNumerableIcon a, Gtk.StyleContext.IsStyleContext b) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'
    -> b
    -- ^ /@style@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m ()
numerableIconSetStyleContext self style = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    style' <- unsafeManagedPtrCastPtr style
    gtk_numerable_icon_set_style_context self' style'
    touchManagedPtr self
    touchManagedPtr style
    return ()

#if defined(ENABLE_OVERLOADING)
data NumerableIconSetStyleContextMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsNumerableIcon a, Gtk.StyleContext.IsStyleContext b) => O.OverloadedMethod NumerableIconSetStyleContextMethodInfo a signature where
    overloadedMethod = numerableIconSetStyleContext

instance O.OverloadedMethodInfo NumerableIconSetStyleContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NumerableIcon.numerableIconSetStyleContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NumerableIcon.html#v:numerableIconSetStyleContext"
        })


#endif

-- method NumerableIcon::new
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "base_icon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GIcon to overlay on"
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
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "Icon" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_new" gtk_numerable_icon_new :: 
    Ptr Gio.Icon.Icon ->                    -- base_icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    IO (Ptr Gio.Icon.Icon)

{-# DEPRECATED numerableIconNew ["(Since version 3.14)"] #-}
-- | Creates a new unthemed t'GI.Gtk.Objects.NumerableIcon.NumerableIcon'.
-- 
-- /Since: 3.0/
numerableIconNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gio.Icon.IsIcon a) =>
    a
    -- ^ /@baseIcon@/: a t'GI.Gio.Interfaces.Icon.Icon' to overlay on
    -> m Gio.Icon.Icon
    -- ^ __Returns:__ a new t'GI.Gio.Interfaces.Icon.Icon'
numerableIconNew baseIcon = liftIO $ do
    baseIcon' <- unsafeManagedPtrCastPtr baseIcon
    result <- gtk_numerable_icon_new baseIcon'
    checkUnexpectedReturnNULL "numerableIconNew" result
    result' <- (wrapObject Gio.Icon.Icon) result
    touchManagedPtr baseIcon
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method NumerableIcon::new_with_style_context
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "base_icon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GIcon to overlay on"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "Icon" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_numerable_icon_new_with_style_context" gtk_numerable_icon_new_with_style_context :: 
    Ptr Gio.Icon.Icon ->                    -- base_icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO (Ptr Gio.Icon.Icon)

{-# DEPRECATED numerableIconNewWithStyleContext ["(Since version 3.14)"] #-}
-- | Creates a new t'GI.Gtk.Objects.NumerableIcon.NumerableIcon' which will themed according
-- to the passed t'GI.Gtk.Objects.StyleContext.StyleContext'. This is a convenience constructor
-- that calls 'GI.Gtk.Objects.NumerableIcon.numerableIconSetStyleContext' internally.
-- 
-- /Since: 3.0/
numerableIconNewWithStyleContext ::
    (B.CallStack.HasCallStack, MonadIO m, Gio.Icon.IsIcon a, Gtk.StyleContext.IsStyleContext b) =>
    a
    -- ^ /@baseIcon@/: a t'GI.Gio.Interfaces.Icon.Icon' to overlay on
    -> b
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m Gio.Icon.Icon
    -- ^ __Returns:__ a new t'GI.Gio.Interfaces.Icon.Icon'
numerableIconNewWithStyleContext baseIcon context = liftIO $ do
    baseIcon' <- unsafeManagedPtrCastPtr baseIcon
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_numerable_icon_new_with_style_context baseIcon' context'
    checkUnexpectedReturnNULL "numerableIconNewWithStyleContext" result
    result' <- (wrapObject Gio.Icon.Icon) result
    touchManagedPtr baseIcon
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


