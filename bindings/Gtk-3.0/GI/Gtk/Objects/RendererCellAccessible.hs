{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.RendererCellAccessible
    ( 

-- * Exported types
    RendererCellAccessible(..)              ,
    IsRendererCellAccessible                ,
    toRendererCellAccessible                ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [doAction]("GI.Atk.Interfaces.Action#g:method:doAction"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getColumnHeaderCells]("GI.Atk.Interfaces.TableCell#g:method:getColumnHeaderCells"), [getColumnSpan]("GI.Atk.Interfaces.TableCell#g:method:getColumnSpan"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getKeybinding]("GI.Atk.Interfaces.Action#g:method:getKeybinding"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getLocalizedName]("GI.Atk.Interfaces.Action#g:method:getLocalizedName"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getNActions]("GI.Atk.Interfaces.Action#g:method:getNActions"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getRowColumnSpan]("GI.Atk.Interfaces.TableCell#g:method:getRowColumnSpan"), [getRowHeaderCells]("GI.Atk.Interfaces.TableCell#g:method:getRowHeaderCells"), [getRowSpan]("GI.Atk.Interfaces.TableCell#g:method:getRowSpan"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getTable]("GI.Atk.Interfaces.TableCell#g:method:getTable"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolveRendererCellAccessibleMethod     ,
#endif

-- ** new #method:new#

    rendererCellAccessibleNew               ,




 -- * Properties


-- ** renderer #attr:renderer#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    RendererCellAccessibleRendererPropertyInfo,
#endif
    constructRendererCellAccessibleRenderer ,
    getRendererCellAccessibleRenderer       ,
#if defined(ENABLE_OVERLOADING)
    rendererCellAccessibleRenderer          ,
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

import qualified GI.Atk.Interfaces.Action as Atk.Action
import qualified GI.Atk.Interfaces.Component as Atk.Component
import qualified GI.Atk.Interfaces.TableCell as Atk.TableCell
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellAccessible as Gtk.CellAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer

-- | Memory-managed wrapper type.
newtype RendererCellAccessible = RendererCellAccessible (SP.ManagedPtr RendererCellAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype RendererCellAccessible where
    toManagedPtr (RendererCellAccessible p) = p

foreign import ccall "gtk_renderer_cell_accessible_get_type"
    c_gtk_renderer_cell_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject RendererCellAccessible where
    glibType = c_gtk_renderer_cell_accessible_get_type

instance B.Types.GObject RendererCellAccessible

-- | Type class for types which can be safely cast to `RendererCellAccessible`, for instance with `toRendererCellAccessible`.
class (SP.GObject o, O.IsDescendantOf RendererCellAccessible o) => IsRendererCellAccessible o
instance (SP.GObject o, O.IsDescendantOf RendererCellAccessible o) => IsRendererCellAccessible o

instance O.HasParentTypes RendererCellAccessible
type instance O.ParentTypes RendererCellAccessible = '[Gtk.CellAccessible.CellAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Action.Action, Atk.Component.Component, Atk.TableCell.TableCell]

-- | Cast to `RendererCellAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toRendererCellAccessible :: (MIO.MonadIO m, IsRendererCellAccessible o) => o -> m RendererCellAccessible
toRendererCellAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo RendererCellAccessible

-- | Convert 'RendererCellAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe RendererCellAccessible) where
    gvalueGType_ = c_gtk_renderer_cell_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr RendererCellAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr RendererCellAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject RendererCellAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveRendererCellAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveRendererCellAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveRendererCellAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveRendererCellAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveRendererCellAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveRendererCellAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveRendererCellAccessibleMethod "doAction" o = Atk.Action.ActionDoActionMethodInfo
    ResolveRendererCellAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveRendererCellAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveRendererCellAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveRendererCellAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveRendererCellAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveRendererCellAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveRendererCellAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveRendererCellAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveRendererCellAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveRendererCellAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveRendererCellAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveRendererCellAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveRendererCellAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveRendererCellAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveRendererCellAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveRendererCellAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveRendererCellAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveRendererCellAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveRendererCellAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveRendererCellAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveRendererCellAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveRendererCellAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveRendererCellAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveRendererCellAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveRendererCellAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveRendererCellAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveRendererCellAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveRendererCellAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveRendererCellAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveRendererCellAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveRendererCellAccessibleMethod "getColumnHeaderCells" o = Atk.TableCell.TableCellGetColumnHeaderCellsMethodInfo
    ResolveRendererCellAccessibleMethod "getColumnSpan" o = Atk.TableCell.TableCellGetColumnSpanMethodInfo
    ResolveRendererCellAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveRendererCellAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveRendererCellAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveRendererCellAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveRendererCellAccessibleMethod "getKeybinding" o = Atk.Action.ActionGetKeybindingMethodInfo
    ResolveRendererCellAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveRendererCellAccessibleMethod "getLocalizedName" o = Atk.Action.ActionGetLocalizedNameMethodInfo
    ResolveRendererCellAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveRendererCellAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveRendererCellAccessibleMethod "getNActions" o = Atk.Action.ActionGetNActionsMethodInfo
    ResolveRendererCellAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveRendererCellAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveRendererCellAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveRendererCellAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveRendererCellAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveRendererCellAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveRendererCellAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveRendererCellAccessibleMethod "getRowColumnSpan" o = Atk.TableCell.TableCellGetRowColumnSpanMethodInfo
    ResolveRendererCellAccessibleMethod "getRowHeaderCells" o = Atk.TableCell.TableCellGetRowHeaderCellsMethodInfo
    ResolveRendererCellAccessibleMethod "getRowSpan" o = Atk.TableCell.TableCellGetRowSpanMethodInfo
    ResolveRendererCellAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveRendererCellAccessibleMethod "getTable" o = Atk.TableCell.TableCellGetTableMethodInfo
    ResolveRendererCellAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveRendererCellAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveRendererCellAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveRendererCellAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveRendererCellAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveRendererCellAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveRendererCellAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveRendererCellAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveRendererCellAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveRendererCellAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveRendererCellAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveRendererCellAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveRendererCellAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveRendererCellAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRendererCellAccessibleMethod t RendererCellAccessible, O.OverloadedMethod info RendererCellAccessible p) => OL.IsLabel t (RendererCellAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRendererCellAccessibleMethod t RendererCellAccessible, O.OverloadedMethod info RendererCellAccessible p, R.HasField t RendererCellAccessible p) => R.HasField t RendererCellAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRendererCellAccessibleMethod t RendererCellAccessible, O.OverloadedMethodInfo info RendererCellAccessible) => OL.IsLabel t (O.MethodProxy info RendererCellAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "renderer"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@renderer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' rendererCellAccessible #renderer
-- @
getRendererCellAccessibleRenderer :: (MonadIO m, IsRendererCellAccessible o) => o -> m (Maybe Gtk.CellRenderer.CellRenderer)
getRendererCellAccessibleRenderer obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "renderer" Gtk.CellRenderer.CellRenderer

-- | Construct a `GValueConstruct` with valid value for the “@renderer@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRendererCellAccessibleRenderer :: (IsRendererCellAccessible o, MIO.MonadIO m, Gtk.CellRenderer.IsCellRenderer a) => a -> m (GValueConstruct o)
constructRendererCellAccessibleRenderer val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "renderer" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data RendererCellAccessibleRendererPropertyInfo
instance AttrInfo RendererCellAccessibleRendererPropertyInfo where
    type AttrAllowedOps RendererCellAccessibleRendererPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint RendererCellAccessibleRendererPropertyInfo = IsRendererCellAccessible
    type AttrSetTypeConstraint RendererCellAccessibleRendererPropertyInfo = Gtk.CellRenderer.IsCellRenderer
    type AttrTransferTypeConstraint RendererCellAccessibleRendererPropertyInfo = Gtk.CellRenderer.IsCellRenderer
    type AttrTransferType RendererCellAccessibleRendererPropertyInfo = Gtk.CellRenderer.CellRenderer
    type AttrGetType RendererCellAccessibleRendererPropertyInfo = (Maybe Gtk.CellRenderer.CellRenderer)
    type AttrLabel RendererCellAccessibleRendererPropertyInfo = "renderer"
    type AttrOrigin RendererCellAccessibleRendererPropertyInfo = RendererCellAccessible
    attrGet = getRendererCellAccessibleRenderer
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.CellRenderer.CellRenderer v
    attrConstruct = constructRendererCellAccessibleRenderer
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RendererCellAccessible.renderer"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RendererCellAccessible.html#g:attr:renderer"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RendererCellAccessible
type instance O.AttributeList RendererCellAccessible = RendererCellAccessibleAttributeList
type RendererCellAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("renderer", RendererCellAccessibleRendererPropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
rendererCellAccessibleRenderer :: AttrLabelProxy "renderer"
rendererCellAccessibleRenderer = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList RendererCellAccessible = RendererCellAccessibleSignalList
type RendererCellAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif

-- method RendererCellAccessible::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "RendererCellAccessible" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_renderer_cell_accessible_new" gtk_renderer_cell_accessible_new :: 
    Ptr Gtk.CellRenderer.CellRenderer ->    -- renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO (Ptr RendererCellAccessible)

-- | /No description available in the introspection data./
rendererCellAccessibleNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.CellRenderer.IsCellRenderer a) =>
    a
    -> m RendererCellAccessible
rendererCellAccessibleNew renderer = liftIO $ do
    renderer' <- unsafeManagedPtrCastPtr renderer
    result <- gtk_renderer_cell_accessible_new renderer'
    checkUnexpectedReturnNULL "rendererCellAccessibleNew" result
    result' <- (wrapObject RendererCellAccessible) result
    touchManagedPtr renderer
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


