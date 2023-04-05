{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.SizeGroup.SizeGroup' provides a mechanism for grouping a number of widgets
-- together so they all request the same amount of space.  This is
-- typically useful when you want a column of widgets to have the same
-- size, but you can’t use a t'GI.Gtk.Objects.Grid.Grid' widget.
-- 
-- In detail, the size requested for each widget in a t'GI.Gtk.Objects.SizeGroup.SizeGroup' is
-- the maximum of the sizes that would have been requested for each
-- widget in the size group if they were not in the size group. The mode
-- of the size group (see 'GI.Gtk.Objects.SizeGroup.sizeGroupSetMode') determines whether
-- this applies to the horizontal size, the vertical size, or both sizes.
-- 
-- Note that size groups only affect the amount of space requested, not
-- the size that the widgets finally receive. If you want the widgets in
-- a t'GI.Gtk.Objects.SizeGroup.SizeGroup' to actually be the same size, you need to pack them in
-- such a way that they get the size they request and not more. For
-- example, if you are packing your widgets into a table, you would not
-- include the 'GI.Gtk.Flags.AttachOptionsFill' flag.
-- 
-- t'GI.Gtk.Objects.SizeGroup.SizeGroup' objects are referenced by each widget in the size group,
-- so once you have added all widgets to a t'GI.Gtk.Objects.SizeGroup.SizeGroup', you can drop
-- the initial reference to the size group with 'GI.GObject.Objects.Object.objectUnref'. If the
-- widgets in the size group are subsequently destroyed, then they will
-- be removed from the size group and drop their references on the size
-- group; when all widgets have been removed, the size group will be
-- freed.
-- 
-- Widgets can be part of multiple size groups; GTK+ will compute the
-- horizontal size of a widget from the horizontal requisition of all
-- widgets that can be reached from the widget by a chain of size groups
-- of type 'GI.Gtk.Enums.SizeGroupModeHorizontal' or 'GI.Gtk.Enums.SizeGroupModeBoth', and the
-- vertical size from the vertical requisition of all widgets that can be
-- reached from the widget by a chain of size groups of type
-- 'GI.Gtk.Enums.SizeGroupModeVertical' or 'GI.Gtk.Enums.SizeGroupModeBoth'.
-- 
-- Note that only non-contextual sizes of every widget are ever consulted
-- by size groups (since size groups have no knowledge of what size a widget
-- will be allocated in one dimension, it cannot derive how much height
-- a widget will receive for a given width). When grouping widgets that
-- trade height for width in mode 'GI.Gtk.Enums.SizeGroupModeVertical' or 'GI.Gtk.Enums.SizeGroupModeBoth':
-- the height for the minimum width will be the requested height for all
-- widgets in the group. The same is of course true when horizontally grouping
-- width for height widgets.
-- 
-- Widgets that trade height-for-width should set a reasonably large minimum width
-- by way of [Label:widthChars]("GI.Gtk.Objects.Label#g:attr:widthChars") for instance. Widgets with static sizes as well
-- as widgets that grow (such as ellipsizing text) need no such considerations.
-- 
-- = GtkSizeGroup as GtkBuildable
-- 
-- Size groups can be specified in a UI definition by placing an @\<object>@
-- element with @class=\"GtkSizeGroup\"@ somewhere in the UI definition. The
-- widgets that belong to the size group are specified by a @\<widgets>@ element
-- that may contain multiple @\<widget>@ elements, one for each member of the
-- size group. The ”name” attribute gives the id of the widget.
-- 
-- An example of a UI definition fragment with GtkSizeGroup:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkSizeGroup">
-- >  <property name="mode">GTK_SIZE_GROUP_HORIZONTAL</property>
-- >  <widgets>
-- >    <widget name="radio1"/>
-- >    <widget name="radio2"/>
-- >  </widgets>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.SizeGroup
    ( 

-- * Exported types
    SizeGroup(..)                           ,
    IsSizeGroup                             ,
    toSizeGroup                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addWidget]("GI.Gtk.Objects.SizeGroup#g:method:addWidget"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [removeWidget]("GI.Gtk.Objects.SizeGroup#g:method:removeWidget"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getIgnoreHidden]("GI.Gtk.Objects.SizeGroup#g:method:getIgnoreHidden"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMode]("GI.Gtk.Objects.SizeGroup#g:method:getMode"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getWidgets]("GI.Gtk.Objects.SizeGroup#g:method:getWidgets").
-- 
-- ==== Setters
-- [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setIgnoreHidden]("GI.Gtk.Objects.SizeGroup#g:method:setIgnoreHidden"), [setMode]("GI.Gtk.Objects.SizeGroup#g:method:setMode"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveSizeGroupMethod                  ,
#endif

-- ** addWidget #method:addWidget#

#if defined(ENABLE_OVERLOADING)
    SizeGroupAddWidgetMethodInfo            ,
#endif
    sizeGroupAddWidget                      ,


-- ** getIgnoreHidden #method:getIgnoreHidden#

#if defined(ENABLE_OVERLOADING)
    SizeGroupGetIgnoreHiddenMethodInfo      ,
#endif
    sizeGroupGetIgnoreHidden                ,


-- ** getMode #method:getMode#

#if defined(ENABLE_OVERLOADING)
    SizeGroupGetModeMethodInfo              ,
#endif
    sizeGroupGetMode                        ,


-- ** getWidgets #method:getWidgets#

#if defined(ENABLE_OVERLOADING)
    SizeGroupGetWidgetsMethodInfo           ,
#endif
    sizeGroupGetWidgets                     ,


-- ** new #method:new#

    sizeGroupNew                            ,


-- ** removeWidget #method:removeWidget#

#if defined(ENABLE_OVERLOADING)
    SizeGroupRemoveWidgetMethodInfo         ,
#endif
    sizeGroupRemoveWidget                   ,


-- ** setIgnoreHidden #method:setIgnoreHidden#

#if defined(ENABLE_OVERLOADING)
    SizeGroupSetIgnoreHiddenMethodInfo      ,
#endif
    sizeGroupSetIgnoreHidden                ,


-- ** setMode #method:setMode#

#if defined(ENABLE_OVERLOADING)
    SizeGroupSetModeMethodInfo              ,
#endif
    sizeGroupSetMode                        ,




 -- * Properties


-- ** ignoreHidden #attr:ignoreHidden#
-- | If 'P.True', unmapped widgets are ignored when determining
-- the size of the group.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    SizeGroupIgnoreHiddenPropertyInfo       ,
#endif
    constructSizeGroupIgnoreHidden          ,
    getSizeGroupIgnoreHidden                ,
    setSizeGroupIgnoreHidden                ,
#if defined(ENABLE_OVERLOADING)
    sizeGroupIgnoreHidden                   ,
#endif


-- ** mode #attr:mode#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SizeGroupModePropertyInfo               ,
#endif
    constructSizeGroupMode                  ,
    getSizeGroupMode                        ,
    setSizeGroupMode                        ,
#if defined(ENABLE_OVERLOADING)
    sizeGroupMode                           ,
#endif




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype SizeGroup = SizeGroup (SP.ManagedPtr SizeGroup)
    deriving (Eq)

instance SP.ManagedPtrNewtype SizeGroup where
    toManagedPtr (SizeGroup p) = p

foreign import ccall "gtk_size_group_get_type"
    c_gtk_size_group_get_type :: IO B.Types.GType

instance B.Types.TypedObject SizeGroup where
    glibType = c_gtk_size_group_get_type

instance B.Types.GObject SizeGroup

-- | Type class for types which can be safely cast to `SizeGroup`, for instance with `toSizeGroup`.
class (SP.GObject o, O.IsDescendantOf SizeGroup o) => IsSizeGroup o
instance (SP.GObject o, O.IsDescendantOf SizeGroup o) => IsSizeGroup o

instance O.HasParentTypes SizeGroup
type instance O.ParentTypes SizeGroup = '[GObject.Object.Object, Gtk.Buildable.Buildable]

-- | Cast to `SizeGroup`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toSizeGroup :: (MIO.MonadIO m, IsSizeGroup o) => o -> m SizeGroup
toSizeGroup = MIO.liftIO . B.ManagedPtr.unsafeCastTo SizeGroup

-- | Convert 'SizeGroup' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe SizeGroup) where
    gvalueGType_ = c_gtk_size_group_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr SizeGroup)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr SizeGroup)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject SizeGroup ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveSizeGroupMethod (t :: Symbol) (o :: *) :: * where
    ResolveSizeGroupMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveSizeGroupMethod "addWidget" o = SizeGroupAddWidgetMethodInfo
    ResolveSizeGroupMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveSizeGroupMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveSizeGroupMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveSizeGroupMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveSizeGroupMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveSizeGroupMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveSizeGroupMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveSizeGroupMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveSizeGroupMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveSizeGroupMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveSizeGroupMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveSizeGroupMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveSizeGroupMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveSizeGroupMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveSizeGroupMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveSizeGroupMethod "removeWidget" o = SizeGroupRemoveWidgetMethodInfo
    ResolveSizeGroupMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveSizeGroupMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveSizeGroupMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveSizeGroupMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveSizeGroupMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveSizeGroupMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveSizeGroupMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveSizeGroupMethod "getIgnoreHidden" o = SizeGroupGetIgnoreHiddenMethodInfo
    ResolveSizeGroupMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveSizeGroupMethod "getMode" o = SizeGroupGetModeMethodInfo
    ResolveSizeGroupMethod "getName" o = Gtk.Buildable.BuildableGetNameMethodInfo
    ResolveSizeGroupMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveSizeGroupMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveSizeGroupMethod "getWidgets" o = SizeGroupGetWidgetsMethodInfo
    ResolveSizeGroupMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveSizeGroupMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveSizeGroupMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveSizeGroupMethod "setIgnoreHidden" o = SizeGroupSetIgnoreHiddenMethodInfo
    ResolveSizeGroupMethod "setMode" o = SizeGroupSetModeMethodInfo
    ResolveSizeGroupMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveSizeGroupMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveSizeGroupMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveSizeGroupMethod t SizeGroup, O.OverloadedMethod info SizeGroup p) => OL.IsLabel t (SizeGroup -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveSizeGroupMethod t SizeGroup, O.OverloadedMethod info SizeGroup p, R.HasField t SizeGroup p) => R.HasField t SizeGroup p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveSizeGroupMethod t SizeGroup, O.OverloadedMethodInfo info SizeGroup) => OL.IsLabel t (O.MethodProxy info SizeGroup) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "ignore-hidden"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@ignore-hidden@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' sizeGroup #ignoreHidden
-- @
getSizeGroupIgnoreHidden :: (MonadIO m, IsSizeGroup o) => o -> m Bool
getSizeGroupIgnoreHidden obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "ignore-hidden"

-- | Set the value of the “@ignore-hidden@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' sizeGroup [ #ignoreHidden 'Data.GI.Base.Attributes.:=' value ]
-- @
setSizeGroupIgnoreHidden :: (MonadIO m, IsSizeGroup o) => o -> Bool -> m ()
setSizeGroupIgnoreHidden obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "ignore-hidden" val

-- | Construct a `GValueConstruct` with valid value for the “@ignore-hidden@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSizeGroupIgnoreHidden :: (IsSizeGroup o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSizeGroupIgnoreHidden val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "ignore-hidden" val

#if defined(ENABLE_OVERLOADING)
data SizeGroupIgnoreHiddenPropertyInfo
instance AttrInfo SizeGroupIgnoreHiddenPropertyInfo where
    type AttrAllowedOps SizeGroupIgnoreHiddenPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SizeGroupIgnoreHiddenPropertyInfo = IsSizeGroup
    type AttrSetTypeConstraint SizeGroupIgnoreHiddenPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SizeGroupIgnoreHiddenPropertyInfo = (~) Bool
    type AttrTransferType SizeGroupIgnoreHiddenPropertyInfo = Bool
    type AttrGetType SizeGroupIgnoreHiddenPropertyInfo = Bool
    type AttrLabel SizeGroupIgnoreHiddenPropertyInfo = "ignore-hidden"
    type AttrOrigin SizeGroupIgnoreHiddenPropertyInfo = SizeGroup
    attrGet = getSizeGroupIgnoreHidden
    attrSet = setSizeGroupIgnoreHidden
    attrTransfer _ v = do
        return v
    attrConstruct = constructSizeGroupIgnoreHidden
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.ignoreHidden"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#g:attr:ignoreHidden"
        })
#endif

-- VVV Prop "mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SizeGroupMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' sizeGroup #mode
-- @
getSizeGroupMode :: (MonadIO m, IsSizeGroup o) => o -> m Gtk.Enums.SizeGroupMode
getSizeGroupMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "mode"

-- | Set the value of the “@mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' sizeGroup [ #mode 'Data.GI.Base.Attributes.:=' value ]
-- @
setSizeGroupMode :: (MonadIO m, IsSizeGroup o) => o -> Gtk.Enums.SizeGroupMode -> m ()
setSizeGroupMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "mode" val

-- | Construct a `GValueConstruct` with valid value for the “@mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSizeGroupMode :: (IsSizeGroup o, MIO.MonadIO m) => Gtk.Enums.SizeGroupMode -> m (GValueConstruct o)
constructSizeGroupMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "mode" val

#if defined(ENABLE_OVERLOADING)
data SizeGroupModePropertyInfo
instance AttrInfo SizeGroupModePropertyInfo where
    type AttrAllowedOps SizeGroupModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SizeGroupModePropertyInfo = IsSizeGroup
    type AttrSetTypeConstraint SizeGroupModePropertyInfo = (~) Gtk.Enums.SizeGroupMode
    type AttrTransferTypeConstraint SizeGroupModePropertyInfo = (~) Gtk.Enums.SizeGroupMode
    type AttrTransferType SizeGroupModePropertyInfo = Gtk.Enums.SizeGroupMode
    type AttrGetType SizeGroupModePropertyInfo = Gtk.Enums.SizeGroupMode
    type AttrLabel SizeGroupModePropertyInfo = "mode"
    type AttrOrigin SizeGroupModePropertyInfo = SizeGroup
    attrGet = getSizeGroupMode
    attrSet = setSizeGroupMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructSizeGroupMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.mode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#g:attr:mode"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList SizeGroup
type instance O.AttributeList SizeGroup = SizeGroupAttributeList
type SizeGroupAttributeList = ('[ '("ignoreHidden", SizeGroupIgnoreHiddenPropertyInfo), '("mode", SizeGroupModePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
sizeGroupIgnoreHidden :: AttrLabelProxy "ignoreHidden"
sizeGroupIgnoreHidden = AttrLabelProxy

sizeGroupMode :: AttrLabelProxy "mode"
sizeGroupMode = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList SizeGroup = SizeGroupSignalList
type SizeGroupSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method SizeGroup::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "mode"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroupMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the mode for the new size group."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "SizeGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_size_group_new" gtk_size_group_new :: 
    CUInt ->                                -- mode : TInterface (Name {namespace = "Gtk", name = "SizeGroupMode"})
    IO (Ptr SizeGroup)

-- | Create a new t'GI.Gtk.Objects.SizeGroup.SizeGroup'.
sizeGroupNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.Enums.SizeGroupMode
    -- ^ /@mode@/: the mode for the new size group.
    -> m SizeGroup
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.SizeGroup.SizeGroup'
sizeGroupNew mode = liftIO $ do
    let mode' = (fromIntegral . fromEnum) mode
    result <- gtk_size_group_new mode'
    checkUnexpectedReturnNULL "sizeGroupNew" result
    result' <- (wrapObject SizeGroup) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method SizeGroup::add_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "size_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSizeGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkWidget to add"
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

foreign import ccall "gtk_size_group_add_widget" gtk_size_group_add_widget :: 
    Ptr SizeGroup ->                        -- size_group : TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Adds a widget to a t'GI.Gtk.Objects.SizeGroup.SizeGroup'. In the future, the requisition
-- of the widget will be determined as the maximum of its requisition
-- and the requisition of the other widgets in the size group.
-- Whether this applies horizontally, vertically, or in both directions
-- depends on the mode of the size group. See 'GI.Gtk.Objects.SizeGroup.sizeGroupSetMode'.
-- 
-- When the widget is destroyed or no longer referenced elsewhere, it will
-- be removed from the size group.
sizeGroupAddWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsSizeGroup a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@sizeGroup@/: a t'GI.Gtk.Objects.SizeGroup.SizeGroup'
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' to add
    -> m ()
sizeGroupAddWidget sizeGroup widget = liftIO $ do
    sizeGroup' <- unsafeManagedPtrCastPtr sizeGroup
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_size_group_add_widget sizeGroup' widget'
    touchManagedPtr sizeGroup
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data SizeGroupAddWidgetMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsSizeGroup a, Gtk.Widget.IsWidget b) => O.OverloadedMethod SizeGroupAddWidgetMethodInfo a signature where
    overloadedMethod = sizeGroupAddWidget

instance O.OverloadedMethodInfo SizeGroupAddWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.sizeGroupAddWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#v:sizeGroupAddWidget"
        })


#endif

-- method SizeGroup::get_ignore_hidden
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "size_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSizeGroup" , sinceVersion = Nothing }
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

foreign import ccall "gtk_size_group_get_ignore_hidden" gtk_size_group_get_ignore_hidden :: 
    Ptr SizeGroup ->                        -- size_group : TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
    IO CInt

{-# DEPRECATED sizeGroupGetIgnoreHidden ["(Since version 3.22)","Measuring the size of hidden widgets has not worked","    reliably for a long time. In most cases, they will report a size","    of 0 nowadays, and thus, their size will not affect the other","    size group members. In effect, size groups will always operate","    as if this property was 'P.True'. Use a t'GI.Gtk.Objects.Stack.Stack' instead to hide","    widgets while still having their size taken into account."] #-}
-- | Returns if invisible widgets are ignored when calculating the size.
-- 
-- /Since: 2.8/
sizeGroupGetIgnoreHidden ::
    (B.CallStack.HasCallStack, MonadIO m, IsSizeGroup a) =>
    a
    -- ^ /@sizeGroup@/: a t'GI.Gtk.Objects.SizeGroup.SizeGroup'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if invisible widgets are ignored.
sizeGroupGetIgnoreHidden sizeGroup = liftIO $ do
    sizeGroup' <- unsafeManagedPtrCastPtr sizeGroup
    result <- gtk_size_group_get_ignore_hidden sizeGroup'
    let result' = (/= 0) result
    touchManagedPtr sizeGroup
    return result'

#if defined(ENABLE_OVERLOADING)
data SizeGroupGetIgnoreHiddenMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsSizeGroup a) => O.OverloadedMethod SizeGroupGetIgnoreHiddenMethodInfo a signature where
    overloadedMethod = sizeGroupGetIgnoreHidden

instance O.OverloadedMethodInfo SizeGroupGetIgnoreHiddenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.sizeGroupGetIgnoreHidden",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#v:sizeGroupGetIgnoreHidden"
        })


#endif

-- method SizeGroup::get_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "size_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSizeGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "SizeGroupMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_size_group_get_mode" gtk_size_group_get_mode :: 
    Ptr SizeGroup ->                        -- size_group : TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
    IO CUInt

-- | Gets the current mode of the size group. See 'GI.Gtk.Objects.SizeGroup.sizeGroupSetMode'.
sizeGroupGetMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsSizeGroup a) =>
    a
    -- ^ /@sizeGroup@/: a t'GI.Gtk.Objects.SizeGroup.SizeGroup'
    -> m Gtk.Enums.SizeGroupMode
    -- ^ __Returns:__ the current mode of the size group.
sizeGroupGetMode sizeGroup = liftIO $ do
    sizeGroup' <- unsafeManagedPtrCastPtr sizeGroup
    result <- gtk_size_group_get_mode sizeGroup'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr sizeGroup
    return result'

#if defined(ENABLE_OVERLOADING)
data SizeGroupGetModeMethodInfo
instance (signature ~ (m Gtk.Enums.SizeGroupMode), MonadIO m, IsSizeGroup a) => O.OverloadedMethod SizeGroupGetModeMethodInfo a signature where
    overloadedMethod = sizeGroupGetMode

instance O.OverloadedMethodInfo SizeGroupGetModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.sizeGroupGetMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#v:sizeGroupGetMode"
        })


#endif

-- method SizeGroup::get_widgets
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "size_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSizeGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGSList (TInterface Name { namespace = "Gtk" , name = "Widget" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_size_group_get_widgets" gtk_size_group_get_widgets :: 
    Ptr SizeGroup ->                        -- size_group : TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
    IO (Ptr (GSList (Ptr Gtk.Widget.Widget)))

-- | Returns the list of widgets associated with /@sizeGroup@/.
-- 
-- /Since: 2.10/
sizeGroupGetWidgets ::
    (B.CallStack.HasCallStack, MonadIO m, IsSizeGroup a) =>
    a
    -- ^ /@sizeGroup@/: a t'GI.Gtk.Objects.SizeGroup.SizeGroup'
    -> m [Gtk.Widget.Widget]
    -- ^ __Returns:__ a t'GI.GLib.Structs.SList.SList' of
    --   widgets. The list is owned by GTK+ and should not be modified.
sizeGroupGetWidgets sizeGroup = liftIO $ do
    sizeGroup' <- unsafeManagedPtrCastPtr sizeGroup
    result <- gtk_size_group_get_widgets sizeGroup'
    result' <- unpackGSList result
    result'' <- mapM (newObject Gtk.Widget.Widget) result'
    touchManagedPtr sizeGroup
    return result''

#if defined(ENABLE_OVERLOADING)
data SizeGroupGetWidgetsMethodInfo
instance (signature ~ (m [Gtk.Widget.Widget]), MonadIO m, IsSizeGroup a) => O.OverloadedMethod SizeGroupGetWidgetsMethodInfo a signature where
    overloadedMethod = sizeGroupGetWidgets

instance O.OverloadedMethodInfo SizeGroupGetWidgetsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.sizeGroupGetWidgets",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#v:sizeGroupGetWidgets"
        })


#endif

-- method SizeGroup::remove_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "size_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSizeGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkWidget to remove"
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

foreign import ccall "gtk_size_group_remove_widget" gtk_size_group_remove_widget :: 
    Ptr SizeGroup ->                        -- size_group : TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Removes a widget from a t'GI.Gtk.Objects.SizeGroup.SizeGroup'.
sizeGroupRemoveWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsSizeGroup a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@sizeGroup@/: a t'GI.Gtk.Objects.SizeGroup.SizeGroup'
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' to remove
    -> m ()
sizeGroupRemoveWidget sizeGroup widget = liftIO $ do
    sizeGroup' <- unsafeManagedPtrCastPtr sizeGroup
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_size_group_remove_widget sizeGroup' widget'
    touchManagedPtr sizeGroup
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data SizeGroupRemoveWidgetMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsSizeGroup a, Gtk.Widget.IsWidget b) => O.OverloadedMethod SizeGroupRemoveWidgetMethodInfo a signature where
    overloadedMethod = sizeGroupRemoveWidget

instance O.OverloadedMethodInfo SizeGroupRemoveWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.sizeGroupRemoveWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#v:sizeGroupRemoveWidget"
        })


#endif

-- method SizeGroup::set_ignore_hidden
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "size_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSizeGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ignore_hidden"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether unmapped widgets should be ignored\n  when calculating the size"
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

foreign import ccall "gtk_size_group_set_ignore_hidden" gtk_size_group_set_ignore_hidden :: 
    Ptr SizeGroup ->                        -- size_group : TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
    CInt ->                                 -- ignore_hidden : TBasicType TBoolean
    IO ()

{-# DEPRECATED sizeGroupSetIgnoreHidden ["(Since version 3.22)","Measuring the size of hidden widgets has not worked","    reliably for a long time. In most cases, they will report a size","    of 0 nowadays, and thus, their size will not affect the other","    size group members. In effect, size groups will always operate","    as if this property was 'P.True'. Use a t'GI.Gtk.Objects.Stack.Stack' instead to hide","    widgets while still having their size taken into account."] #-}
-- | Sets whether unmapped widgets should be ignored when
-- calculating the size.
-- 
-- /Since: 2.8/
sizeGroupSetIgnoreHidden ::
    (B.CallStack.HasCallStack, MonadIO m, IsSizeGroup a) =>
    a
    -- ^ /@sizeGroup@/: a t'GI.Gtk.Objects.SizeGroup.SizeGroup'
    -> Bool
    -- ^ /@ignoreHidden@/: whether unmapped widgets should be ignored
    --   when calculating the size
    -> m ()
sizeGroupSetIgnoreHidden sizeGroup ignoreHidden = liftIO $ do
    sizeGroup' <- unsafeManagedPtrCastPtr sizeGroup
    let ignoreHidden' = (fromIntegral . fromEnum) ignoreHidden
    gtk_size_group_set_ignore_hidden sizeGroup' ignoreHidden'
    touchManagedPtr sizeGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data SizeGroupSetIgnoreHiddenMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsSizeGroup a) => O.OverloadedMethod SizeGroupSetIgnoreHiddenMethodInfo a signature where
    overloadedMethod = sizeGroupSetIgnoreHidden

instance O.OverloadedMethodInfo SizeGroupSetIgnoreHiddenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.sizeGroupSetIgnoreHidden",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#v:sizeGroupSetIgnoreHidden"
        })


#endif

-- method SizeGroup::set_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "size_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSizeGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mode"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SizeGroupMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the mode to set for the size group."
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

foreign import ccall "gtk_size_group_set_mode" gtk_size_group_set_mode :: 
    Ptr SizeGroup ->                        -- size_group : TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
    CUInt ->                                -- mode : TInterface (Name {namespace = "Gtk", name = "SizeGroupMode"})
    IO ()

-- | Sets the t'GI.Gtk.Enums.SizeGroupMode' of the size group. The mode of the size
-- group determines whether the widgets in the size group should
-- all have the same horizontal requisition ('GI.Gtk.Enums.SizeGroupModeHorizontal')
-- all have the same vertical requisition ('GI.Gtk.Enums.SizeGroupModeVertical'),
-- or should all have the same requisition in both directions
-- ('GI.Gtk.Enums.SizeGroupModeBoth').
sizeGroupSetMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsSizeGroup a) =>
    a
    -- ^ /@sizeGroup@/: a t'GI.Gtk.Objects.SizeGroup.SizeGroup'
    -> Gtk.Enums.SizeGroupMode
    -- ^ /@mode@/: the mode to set for the size group.
    -> m ()
sizeGroupSetMode sizeGroup mode = liftIO $ do
    sizeGroup' <- unsafeManagedPtrCastPtr sizeGroup
    let mode' = (fromIntegral . fromEnum) mode
    gtk_size_group_set_mode sizeGroup' mode'
    touchManagedPtr sizeGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data SizeGroupSetModeMethodInfo
instance (signature ~ (Gtk.Enums.SizeGroupMode -> m ()), MonadIO m, IsSizeGroup a) => O.OverloadedMethod SizeGroupSetModeMethodInfo a signature where
    overloadedMethod = sizeGroupSetMode

instance O.OverloadedMethodInfo SizeGroupSetModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SizeGroup.sizeGroupSetMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SizeGroup.html#v:sizeGroupSetMode"
        })


#endif


