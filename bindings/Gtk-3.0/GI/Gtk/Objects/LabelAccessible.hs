{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.LabelAccessible
    ( 

-- * Exported types
    LabelAccessible(..)                     ,
    IsLabelAccessible                       ,
    toLabelAccessible                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [addSelection]("GI.Atk.Interfaces.Text#g:method:addSelection"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [removeSelection]("GI.Atk.Interfaces.Text#g:method:removeSelection"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollSubstringTo]("GI.Atk.Interfaces.Text#g:method:scrollSubstringTo"), [scrollSubstringToPoint]("GI.Atk.Interfaces.Text#g:method:scrollSubstringToPoint"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getBoundedRanges]("GI.Atk.Interfaces.Text#g:method:getBoundedRanges"), [getCaretOffset]("GI.Atk.Interfaces.Text#g:method:getCaretOffset"), [getCharacterAtOffset]("GI.Atk.Interfaces.Text#g:method:getCharacterAtOffset"), [getCharacterCount]("GI.Atk.Interfaces.Text#g:method:getCharacterCount"), [getCharacterExtents]("GI.Atk.Interfaces.Text#g:method:getCharacterExtents"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultAttributes]("GI.Atk.Interfaces.Text#g:method:getDefaultAttributes"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getLink]("GI.Atk.Interfaces.Hypertext#g:method:getLink"), [getLinkIndex]("GI.Atk.Interfaces.Hypertext#g:method:getLinkIndex"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getNLinks]("GI.Atk.Interfaces.Hypertext#g:method:getNLinks"), [getNSelections]("GI.Atk.Interfaces.Text#g:method:getNSelections"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getOffsetAtPoint]("GI.Atk.Interfaces.Text#g:method:getOffsetAtPoint"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRangeExtents]("GI.Atk.Interfaces.Text#g:method:getRangeExtents"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getRunAttributes]("GI.Atk.Interfaces.Text#g:method:getRunAttributes"), [getSelection]("GI.Atk.Interfaces.Text#g:method:getSelection"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getStringAtOffset]("GI.Atk.Interfaces.Text#g:method:getStringAtOffset"), [getText]("GI.Atk.Interfaces.Text#g:method:getText"), [getTextAfterOffset]("GI.Atk.Interfaces.Text#g:method:getTextAfterOffset"), [getTextAtOffset]("GI.Atk.Interfaces.Text#g:method:getTextAtOffset"), [getTextBeforeOffset]("GI.Atk.Interfaces.Text#g:method:getTextBeforeOffset"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setCaretOffset]("GI.Atk.Interfaces.Text#g:method:setCaretOffset"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setSelection]("GI.Atk.Interfaces.Text#g:method:setSelection"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolveLabelAccessibleMethod            ,
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

import qualified GI.Atk.Interfaces.Component as Atk.Component
import qualified GI.Atk.Interfaces.Hypertext as Atk.Hypertext
import qualified GI.Atk.Interfaces.Text as Atk.Text
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible

-- | Memory-managed wrapper type.
newtype LabelAccessible = LabelAccessible (SP.ManagedPtr LabelAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype LabelAccessible where
    toManagedPtr (LabelAccessible p) = p

foreign import ccall "gtk_label_accessible_get_type"
    c_gtk_label_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject LabelAccessible where
    glibType = c_gtk_label_accessible_get_type

instance B.Types.GObject LabelAccessible

-- | Type class for types which can be safely cast to `LabelAccessible`, for instance with `toLabelAccessible`.
class (SP.GObject o, O.IsDescendantOf LabelAccessible o) => IsLabelAccessible o
instance (SP.GObject o, O.IsDescendantOf LabelAccessible o) => IsLabelAccessible o

instance O.HasParentTypes LabelAccessible
type instance O.ParentTypes LabelAccessible = '[Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Component.Component, Atk.Hypertext.Hypertext, Atk.Text.Text]

-- | Cast to `LabelAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toLabelAccessible :: (MIO.MonadIO m, IsLabelAccessible o) => o -> m LabelAccessible
toLabelAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo LabelAccessible

-- | Convert 'LabelAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe LabelAccessible) where
    gvalueGType_ = c_gtk_label_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr LabelAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr LabelAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject LabelAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveLabelAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveLabelAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveLabelAccessibleMethod "addSelection" o = Atk.Text.TextAddSelectionMethodInfo
    ResolveLabelAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveLabelAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveLabelAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveLabelAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveLabelAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveLabelAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveLabelAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveLabelAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveLabelAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveLabelAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveLabelAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveLabelAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveLabelAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveLabelAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveLabelAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveLabelAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveLabelAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveLabelAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveLabelAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveLabelAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveLabelAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveLabelAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveLabelAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveLabelAccessibleMethod "removeSelection" o = Atk.Text.TextRemoveSelectionMethodInfo
    ResolveLabelAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveLabelAccessibleMethod "scrollSubstringTo" o = Atk.Text.TextScrollSubstringToMethodInfo
    ResolveLabelAccessibleMethod "scrollSubstringToPoint" o = Atk.Text.TextScrollSubstringToPointMethodInfo
    ResolveLabelAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveLabelAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveLabelAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveLabelAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveLabelAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveLabelAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveLabelAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveLabelAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveLabelAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveLabelAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveLabelAccessibleMethod "getBoundedRanges" o = Atk.Text.TextGetBoundedRangesMethodInfo
    ResolveLabelAccessibleMethod "getCaretOffset" o = Atk.Text.TextGetCaretOffsetMethodInfo
    ResolveLabelAccessibleMethod "getCharacterAtOffset" o = Atk.Text.TextGetCharacterAtOffsetMethodInfo
    ResolveLabelAccessibleMethod "getCharacterCount" o = Atk.Text.TextGetCharacterCountMethodInfo
    ResolveLabelAccessibleMethod "getCharacterExtents" o = Atk.Text.TextGetCharacterExtentsMethodInfo
    ResolveLabelAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveLabelAccessibleMethod "getDefaultAttributes" o = Atk.Text.TextGetDefaultAttributesMethodInfo
    ResolveLabelAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveLabelAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveLabelAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveLabelAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveLabelAccessibleMethod "getLink" o = Atk.Hypertext.HypertextGetLinkMethodInfo
    ResolveLabelAccessibleMethod "getLinkIndex" o = Atk.Hypertext.HypertextGetLinkIndexMethodInfo
    ResolveLabelAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveLabelAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveLabelAccessibleMethod "getNLinks" o = Atk.Hypertext.HypertextGetNLinksMethodInfo
    ResolveLabelAccessibleMethod "getNSelections" o = Atk.Text.TextGetNSelectionsMethodInfo
    ResolveLabelAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveLabelAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveLabelAccessibleMethod "getOffsetAtPoint" o = Atk.Text.TextGetOffsetAtPointMethodInfo
    ResolveLabelAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveLabelAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveLabelAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveLabelAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveLabelAccessibleMethod "getRangeExtents" o = Atk.Text.TextGetRangeExtentsMethodInfo
    ResolveLabelAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveLabelAccessibleMethod "getRunAttributes" o = Atk.Text.TextGetRunAttributesMethodInfo
    ResolveLabelAccessibleMethod "getSelection" o = Atk.Text.TextGetSelectionMethodInfo
    ResolveLabelAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveLabelAccessibleMethod "getStringAtOffset" o = Atk.Text.TextGetStringAtOffsetMethodInfo
    ResolveLabelAccessibleMethod "getText" o = Atk.Text.TextGetTextMethodInfo
    ResolveLabelAccessibleMethod "getTextAfterOffset" o = Atk.Text.TextGetTextAfterOffsetMethodInfo
    ResolveLabelAccessibleMethod "getTextAtOffset" o = Atk.Text.TextGetTextAtOffsetMethodInfo
    ResolveLabelAccessibleMethod "getTextBeforeOffset" o = Atk.Text.TextGetTextBeforeOffsetMethodInfo
    ResolveLabelAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveLabelAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveLabelAccessibleMethod "setCaretOffset" o = Atk.Text.TextSetCaretOffsetMethodInfo
    ResolveLabelAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveLabelAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveLabelAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveLabelAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveLabelAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveLabelAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveLabelAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveLabelAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveLabelAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveLabelAccessibleMethod "setSelection" o = Atk.Text.TextSetSelectionMethodInfo
    ResolveLabelAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveLabelAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveLabelAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveLabelAccessibleMethod t LabelAccessible, O.OverloadedMethod info LabelAccessible p) => OL.IsLabel t (LabelAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveLabelAccessibleMethod t LabelAccessible, O.OverloadedMethod info LabelAccessible p, R.HasField t LabelAccessible p) => R.HasField t LabelAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveLabelAccessibleMethod t LabelAccessible, O.OverloadedMethodInfo info LabelAccessible) => OL.IsLabel t (O.MethodProxy info LabelAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList LabelAccessible
type instance O.AttributeList LabelAccessible = LabelAccessibleAttributeList
type LabelAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList LabelAccessible = LabelAccessibleSignalList
type LabelAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("linkSelected", Atk.Hypertext.HypertextLinkSelectedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("textAttributesChanged", Atk.Text.TextTextAttributesChangedSignalInfo), '("textCaretMoved", Atk.Text.TextTextCaretMovedSignalInfo), '("textChanged", Atk.Text.TextTextChangedSignalInfo), '("textInsert", Atk.Text.TextTextInsertSignalInfo), '("textRemove", Atk.Text.TextTextRemoveSignalInfo), '("textSelectionChanged", Atk.Text.TextTextSelectionChangedSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


