{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TextViewAccessible
    ( 

-- * Exported types
    TextViewAccessible(..)                  ,
    IsTextViewAccessible                    ,
    toTextViewAccessible                    ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [addSelection]("GI.Atk.Interfaces.Text#g:method:addSelection"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [copyText]("GI.Atk.Interfaces.EditableText#g:method:copyText"), [cutText]("GI.Atk.Interfaces.EditableText#g:method:cutText"), [deleteText]("GI.Atk.Interfaces.EditableText#g:method:deleteText"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [insertText]("GI.Atk.Interfaces.EditableText#g:method:insertText"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [pasteText]("GI.Atk.Interfaces.EditableText#g:method:pasteText"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [removeSelection]("GI.Atk.Interfaces.Text#g:method:removeSelection"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollSubstringTo]("GI.Atk.Interfaces.Text#g:method:scrollSubstringTo"), [scrollSubstringToPoint]("GI.Atk.Interfaces.Text#g:method:scrollSubstringToPoint"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getBoundedRanges]("GI.Atk.Interfaces.Text#g:method:getBoundedRanges"), [getCaretOffset]("GI.Atk.Interfaces.Text#g:method:getCaretOffset"), [getCharacterAtOffset]("GI.Atk.Interfaces.Text#g:method:getCharacterAtOffset"), [getCharacterCount]("GI.Atk.Interfaces.Text#g:method:getCharacterCount"), [getCharacterExtents]("GI.Atk.Interfaces.Text#g:method:getCharacterExtents"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultAttributes]("GI.Atk.Interfaces.Text#g:method:getDefaultAttributes"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getMimeType]("GI.Atk.Interfaces.StreamableContent#g:method:getMimeType"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getNMimeTypes]("GI.Atk.Interfaces.StreamableContent#g:method:getNMimeTypes"), [getNSelections]("GI.Atk.Interfaces.Text#g:method:getNSelections"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getOffsetAtPoint]("GI.Atk.Interfaces.Text#g:method:getOffsetAtPoint"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRangeExtents]("GI.Atk.Interfaces.Text#g:method:getRangeExtents"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getRunAttributes]("GI.Atk.Interfaces.Text#g:method:getRunAttributes"), [getSelection]("GI.Atk.Interfaces.Text#g:method:getSelection"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getStream]("GI.Atk.Interfaces.StreamableContent#g:method:getStream"), [getStringAtOffset]("GI.Atk.Interfaces.Text#g:method:getStringAtOffset"), [getText]("GI.Atk.Interfaces.Text#g:method:getText"), [getTextAfterOffset]("GI.Atk.Interfaces.Text#g:method:getTextAfterOffset"), [getTextAtOffset]("GI.Atk.Interfaces.Text#g:method:getTextAtOffset"), [getTextBeforeOffset]("GI.Atk.Interfaces.Text#g:method:getTextBeforeOffset"), [getUri]("GI.Atk.Interfaces.StreamableContent#g:method:getUri"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setCaretOffset]("GI.Atk.Interfaces.Text#g:method:setCaretOffset"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setRunAttributes]("GI.Atk.Interfaces.EditableText#g:method:setRunAttributes"), [setSelection]("GI.Atk.Interfaces.Text#g:method:setSelection"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setTextContents]("GI.Atk.Interfaces.EditableText#g:method:setTextContents"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolveTextViewAccessibleMethod         ,
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
import qualified GI.Atk.Interfaces.EditableText as Atk.EditableText
import qualified GI.Atk.Interfaces.StreamableContent as Atk.StreamableContent
import qualified GI.Atk.Interfaces.Text as Atk.Text
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.ContainerAccessible as Gtk.ContainerAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible

-- | Memory-managed wrapper type.
newtype TextViewAccessible = TextViewAccessible (SP.ManagedPtr TextViewAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextViewAccessible where
    toManagedPtr (TextViewAccessible p) = p

foreign import ccall "gtk_text_view_accessible_get_type"
    c_gtk_text_view_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject TextViewAccessible where
    glibType = c_gtk_text_view_accessible_get_type

instance B.Types.GObject TextViewAccessible

-- | Type class for types which can be safely cast to `TextViewAccessible`, for instance with `toTextViewAccessible`.
class (SP.GObject o, O.IsDescendantOf TextViewAccessible o) => IsTextViewAccessible o
instance (SP.GObject o, O.IsDescendantOf TextViewAccessible o) => IsTextViewAccessible o

instance O.HasParentTypes TextViewAccessible
type instance O.ParentTypes TextViewAccessible = '[Gtk.ContainerAccessible.ContainerAccessible, Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Component.Component, Atk.EditableText.EditableText, Atk.StreamableContent.StreamableContent, Atk.Text.Text]

-- | Cast to `TextViewAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTextViewAccessible :: (MIO.MonadIO m, IsTextViewAccessible o) => o -> m TextViewAccessible
toTextViewAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo TextViewAccessible

-- | Convert 'TextViewAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextViewAccessible) where
    gvalueGType_ = c_gtk_text_view_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TextViewAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TextViewAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TextViewAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTextViewAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextViewAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveTextViewAccessibleMethod "addSelection" o = Atk.Text.TextAddSelectionMethodInfo
    ResolveTextViewAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTextViewAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTextViewAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveTextViewAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveTextViewAccessibleMethod "copyText" o = Atk.EditableText.EditableTextCopyTextMethodInfo
    ResolveTextViewAccessibleMethod "cutText" o = Atk.EditableText.EditableTextCutTextMethodInfo
    ResolveTextViewAccessibleMethod "deleteText" o = Atk.EditableText.EditableTextDeleteTextMethodInfo
    ResolveTextViewAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTextViewAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTextViewAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTextViewAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveTextViewAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveTextViewAccessibleMethod "insertText" o = Atk.EditableText.EditableTextInsertTextMethodInfo
    ResolveTextViewAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTextViewAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTextViewAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTextViewAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveTextViewAccessibleMethod "pasteText" o = Atk.EditableText.EditableTextPasteTextMethodInfo
    ResolveTextViewAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveTextViewAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTextViewAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveTextViewAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveTextViewAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveTextViewAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTextViewAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveTextViewAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveTextViewAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveTextViewAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveTextViewAccessibleMethod "removeSelection" o = Atk.Text.TextRemoveSelectionMethodInfo
    ResolveTextViewAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTextViewAccessibleMethod "scrollSubstringTo" o = Atk.Text.TextScrollSubstringToMethodInfo
    ResolveTextViewAccessibleMethod "scrollSubstringToPoint" o = Atk.Text.TextScrollSubstringToPointMethodInfo
    ResolveTextViewAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveTextViewAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveTextViewAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTextViewAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTextViewAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTextViewAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTextViewAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTextViewAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveTextViewAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveTextViewAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveTextViewAccessibleMethod "getBoundedRanges" o = Atk.Text.TextGetBoundedRangesMethodInfo
    ResolveTextViewAccessibleMethod "getCaretOffset" o = Atk.Text.TextGetCaretOffsetMethodInfo
    ResolveTextViewAccessibleMethod "getCharacterAtOffset" o = Atk.Text.TextGetCharacterAtOffsetMethodInfo
    ResolveTextViewAccessibleMethod "getCharacterCount" o = Atk.Text.TextGetCharacterCountMethodInfo
    ResolveTextViewAccessibleMethod "getCharacterExtents" o = Atk.Text.TextGetCharacterExtentsMethodInfo
    ResolveTextViewAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTextViewAccessibleMethod "getDefaultAttributes" o = Atk.Text.TextGetDefaultAttributesMethodInfo
    ResolveTextViewAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveTextViewAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveTextViewAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveTextViewAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveTextViewAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveTextViewAccessibleMethod "getMimeType" o = Atk.StreamableContent.StreamableContentGetMimeTypeMethodInfo
    ResolveTextViewAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveTextViewAccessibleMethod "getNMimeTypes" o = Atk.StreamableContent.StreamableContentGetNMimeTypesMethodInfo
    ResolveTextViewAccessibleMethod "getNSelections" o = Atk.Text.TextGetNSelectionsMethodInfo
    ResolveTextViewAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveTextViewAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveTextViewAccessibleMethod "getOffsetAtPoint" o = Atk.Text.TextGetOffsetAtPointMethodInfo
    ResolveTextViewAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveTextViewAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveTextViewAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTextViewAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTextViewAccessibleMethod "getRangeExtents" o = Atk.Text.TextGetRangeExtentsMethodInfo
    ResolveTextViewAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveTextViewAccessibleMethod "getRunAttributes" o = Atk.Text.TextGetRunAttributesMethodInfo
    ResolveTextViewAccessibleMethod "getSelection" o = Atk.Text.TextGetSelectionMethodInfo
    ResolveTextViewAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveTextViewAccessibleMethod "getStream" o = Atk.StreamableContent.StreamableContentGetStreamMethodInfo
    ResolveTextViewAccessibleMethod "getStringAtOffset" o = Atk.Text.TextGetStringAtOffsetMethodInfo
    ResolveTextViewAccessibleMethod "getText" o = Atk.Text.TextGetTextMethodInfo
    ResolveTextViewAccessibleMethod "getTextAfterOffset" o = Atk.Text.TextGetTextAfterOffsetMethodInfo
    ResolveTextViewAccessibleMethod "getTextAtOffset" o = Atk.Text.TextGetTextAtOffsetMethodInfo
    ResolveTextViewAccessibleMethod "getTextBeforeOffset" o = Atk.Text.TextGetTextBeforeOffsetMethodInfo
    ResolveTextViewAccessibleMethod "getUri" o = Atk.StreamableContent.StreamableContentGetUriMethodInfo
    ResolveTextViewAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveTextViewAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveTextViewAccessibleMethod "setCaretOffset" o = Atk.Text.TextSetCaretOffsetMethodInfo
    ResolveTextViewAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTextViewAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTextViewAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveTextViewAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveTextViewAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveTextViewAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveTextViewAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveTextViewAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTextViewAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveTextViewAccessibleMethod "setRunAttributes" o = Atk.EditableText.EditableTextSetRunAttributesMethodInfo
    ResolveTextViewAccessibleMethod "setSelection" o = Atk.Text.TextSetSelectionMethodInfo
    ResolveTextViewAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveTextViewAccessibleMethod "setTextContents" o = Atk.EditableText.EditableTextSetTextContentsMethodInfo
    ResolveTextViewAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveTextViewAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextViewAccessibleMethod t TextViewAccessible, O.OverloadedMethod info TextViewAccessible p) => OL.IsLabel t (TextViewAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextViewAccessibleMethod t TextViewAccessible, O.OverloadedMethod info TextViewAccessible p, R.HasField t TextViewAccessible p) => R.HasField t TextViewAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextViewAccessibleMethod t TextViewAccessible, O.OverloadedMethodInfo info TextViewAccessible) => OL.IsLabel t (O.MethodProxy info TextViewAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextViewAccessible
type instance O.AttributeList TextViewAccessible = TextViewAccessibleAttributeList
type TextViewAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TextViewAccessible = TextViewAccessibleSignalList
type TextViewAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("textAttributesChanged", Atk.Text.TextTextAttributesChangedSignalInfo), '("textCaretMoved", Atk.Text.TextTextCaretMovedSignalInfo), '("textChanged", Atk.Text.TextTextChangedSignalInfo), '("textInsert", Atk.Text.TextTextInsertSignalInfo), '("textRemove", Atk.Text.TextTextRemoveSignalInfo), '("textSelectionChanged", Atk.Text.TextTextSelectionChangedSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


