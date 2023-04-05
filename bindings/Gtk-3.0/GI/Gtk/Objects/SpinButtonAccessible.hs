{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.SpinButtonAccessible
    ( 

-- * Exported types
    SpinButtonAccessible(..)                ,
    IsSpinButtonAccessible                  ,
    toSpinButtonAccessible                  ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [addSelection]("GI.Atk.Interfaces.Text#g:method:addSelection"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [copyText]("GI.Atk.Interfaces.EditableText#g:method:copyText"), [cutText]("GI.Atk.Interfaces.EditableText#g:method:cutText"), [deleteText]("GI.Atk.Interfaces.EditableText#g:method:deleteText"), [doAction]("GI.Atk.Interfaces.Action#g:method:doAction"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [insertText]("GI.Atk.Interfaces.EditableText#g:method:insertText"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [pasteText]("GI.Atk.Interfaces.EditableText#g:method:pasteText"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [removeSelection]("GI.Atk.Interfaces.Text#g:method:removeSelection"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollSubstringTo]("GI.Atk.Interfaces.Text#g:method:scrollSubstringTo"), [scrollSubstringToPoint]("GI.Atk.Interfaces.Text#g:method:scrollSubstringToPoint"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getBoundedRanges]("GI.Atk.Interfaces.Text#g:method:getBoundedRanges"), [getCaretOffset]("GI.Atk.Interfaces.Text#g:method:getCaretOffset"), [getCharacterAtOffset]("GI.Atk.Interfaces.Text#g:method:getCharacterAtOffset"), [getCharacterCount]("GI.Atk.Interfaces.Text#g:method:getCharacterCount"), [getCharacterExtents]("GI.Atk.Interfaces.Text#g:method:getCharacterExtents"), [getCurrentValue]("GI.Atk.Interfaces.Value#g:method:getCurrentValue"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultAttributes]("GI.Atk.Interfaces.Text#g:method:getDefaultAttributes"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIncrement]("GI.Atk.Interfaces.Value#g:method:getIncrement"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getKeybinding]("GI.Atk.Interfaces.Action#g:method:getKeybinding"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getLocalizedName]("GI.Atk.Interfaces.Action#g:method:getLocalizedName"), [getMaximumValue]("GI.Atk.Interfaces.Value#g:method:getMaximumValue"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getMinimumIncrement]("GI.Atk.Interfaces.Value#g:method:getMinimumIncrement"), [getMinimumValue]("GI.Atk.Interfaces.Value#g:method:getMinimumValue"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getNActions]("GI.Atk.Interfaces.Action#g:method:getNActions"), [getNSelections]("GI.Atk.Interfaces.Text#g:method:getNSelections"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getOffsetAtPoint]("GI.Atk.Interfaces.Text#g:method:getOffsetAtPoint"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRange]("GI.Atk.Interfaces.Value#g:method:getRange"), [getRangeExtents]("GI.Atk.Interfaces.Text#g:method:getRangeExtents"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getRunAttributes]("GI.Atk.Interfaces.Text#g:method:getRunAttributes"), [getSelection]("GI.Atk.Interfaces.Text#g:method:getSelection"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getStringAtOffset]("GI.Atk.Interfaces.Text#g:method:getStringAtOffset"), [getSubRanges]("GI.Atk.Interfaces.Value#g:method:getSubRanges"), [getText]("GI.Atk.Interfaces.Text#g:method:getText"), [getTextAfterOffset]("GI.Atk.Interfaces.Text#g:method:getTextAfterOffset"), [getTextAtOffset]("GI.Atk.Interfaces.Text#g:method:getTextAtOffset"), [getTextBeforeOffset]("GI.Atk.Interfaces.Text#g:method:getTextBeforeOffset"), [getValueAndText]("GI.Atk.Interfaces.Value#g:method:getValueAndText"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setCaretOffset]("GI.Atk.Interfaces.Text#g:method:setCaretOffset"), [setCurrentValue]("GI.Atk.Interfaces.Value#g:method:setCurrentValue"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setRunAttributes]("GI.Atk.Interfaces.EditableText#g:method:setRunAttributes"), [setSelection]("GI.Atk.Interfaces.Text#g:method:setSelection"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setTextContents]("GI.Atk.Interfaces.EditableText#g:method:setTextContents"), [setValue]("GI.Atk.Interfaces.Value#g:method:setValue"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolveSpinButtonAccessibleMethod       ,
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
import qualified GI.Atk.Interfaces.EditableText as Atk.EditableText
import qualified GI.Atk.Interfaces.Text as Atk.Text
import qualified GI.Atk.Interfaces.Value as Atk.Value
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.EntryAccessible as Gtk.EntryAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible

-- | Memory-managed wrapper type.
newtype SpinButtonAccessible = SpinButtonAccessible (SP.ManagedPtr SpinButtonAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype SpinButtonAccessible where
    toManagedPtr (SpinButtonAccessible p) = p

foreign import ccall "gtk_spin_button_accessible_get_type"
    c_gtk_spin_button_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject SpinButtonAccessible where
    glibType = c_gtk_spin_button_accessible_get_type

instance B.Types.GObject SpinButtonAccessible

-- | Type class for types which can be safely cast to `SpinButtonAccessible`, for instance with `toSpinButtonAccessible`.
class (SP.GObject o, O.IsDescendantOf SpinButtonAccessible o) => IsSpinButtonAccessible o
instance (SP.GObject o, O.IsDescendantOf SpinButtonAccessible o) => IsSpinButtonAccessible o

instance O.HasParentTypes SpinButtonAccessible
type instance O.ParentTypes SpinButtonAccessible = '[Gtk.EntryAccessible.EntryAccessible, Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Action.Action, Atk.Component.Component, Atk.EditableText.EditableText, Atk.Text.Text, Atk.Value.Value]

-- | Cast to `SpinButtonAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toSpinButtonAccessible :: (MIO.MonadIO m, IsSpinButtonAccessible o) => o -> m SpinButtonAccessible
toSpinButtonAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo SpinButtonAccessible

-- | Convert 'SpinButtonAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe SpinButtonAccessible) where
    gvalueGType_ = c_gtk_spin_button_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr SpinButtonAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr SpinButtonAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject SpinButtonAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveSpinButtonAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveSpinButtonAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveSpinButtonAccessibleMethod "addSelection" o = Atk.Text.TextAddSelectionMethodInfo
    ResolveSpinButtonAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveSpinButtonAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveSpinButtonAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveSpinButtonAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveSpinButtonAccessibleMethod "copyText" o = Atk.EditableText.EditableTextCopyTextMethodInfo
    ResolveSpinButtonAccessibleMethod "cutText" o = Atk.EditableText.EditableTextCutTextMethodInfo
    ResolveSpinButtonAccessibleMethod "deleteText" o = Atk.EditableText.EditableTextDeleteTextMethodInfo
    ResolveSpinButtonAccessibleMethod "doAction" o = Atk.Action.ActionDoActionMethodInfo
    ResolveSpinButtonAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveSpinButtonAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveSpinButtonAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveSpinButtonAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveSpinButtonAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveSpinButtonAccessibleMethod "insertText" o = Atk.EditableText.EditableTextInsertTextMethodInfo
    ResolveSpinButtonAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveSpinButtonAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveSpinButtonAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveSpinButtonAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveSpinButtonAccessibleMethod "pasteText" o = Atk.EditableText.EditableTextPasteTextMethodInfo
    ResolveSpinButtonAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveSpinButtonAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveSpinButtonAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveSpinButtonAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveSpinButtonAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveSpinButtonAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveSpinButtonAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveSpinButtonAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveSpinButtonAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveSpinButtonAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveSpinButtonAccessibleMethod "removeSelection" o = Atk.Text.TextRemoveSelectionMethodInfo
    ResolveSpinButtonAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveSpinButtonAccessibleMethod "scrollSubstringTo" o = Atk.Text.TextScrollSubstringToMethodInfo
    ResolveSpinButtonAccessibleMethod "scrollSubstringToPoint" o = Atk.Text.TextScrollSubstringToPointMethodInfo
    ResolveSpinButtonAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveSpinButtonAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveSpinButtonAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveSpinButtonAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveSpinButtonAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveSpinButtonAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveSpinButtonAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveSpinButtonAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveSpinButtonAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveSpinButtonAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveSpinButtonAccessibleMethod "getBoundedRanges" o = Atk.Text.TextGetBoundedRangesMethodInfo
    ResolveSpinButtonAccessibleMethod "getCaretOffset" o = Atk.Text.TextGetCaretOffsetMethodInfo
    ResolveSpinButtonAccessibleMethod "getCharacterAtOffset" o = Atk.Text.TextGetCharacterAtOffsetMethodInfo
    ResolveSpinButtonAccessibleMethod "getCharacterCount" o = Atk.Text.TextGetCharacterCountMethodInfo
    ResolveSpinButtonAccessibleMethod "getCharacterExtents" o = Atk.Text.TextGetCharacterExtentsMethodInfo
    ResolveSpinButtonAccessibleMethod "getCurrentValue" o = Atk.Value.ValueGetCurrentValueMethodInfo
    ResolveSpinButtonAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveSpinButtonAccessibleMethod "getDefaultAttributes" o = Atk.Text.TextGetDefaultAttributesMethodInfo
    ResolveSpinButtonAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveSpinButtonAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveSpinButtonAccessibleMethod "getIncrement" o = Atk.Value.ValueGetIncrementMethodInfo
    ResolveSpinButtonAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveSpinButtonAccessibleMethod "getKeybinding" o = Atk.Action.ActionGetKeybindingMethodInfo
    ResolveSpinButtonAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveSpinButtonAccessibleMethod "getLocalizedName" o = Atk.Action.ActionGetLocalizedNameMethodInfo
    ResolveSpinButtonAccessibleMethod "getMaximumValue" o = Atk.Value.ValueGetMaximumValueMethodInfo
    ResolveSpinButtonAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveSpinButtonAccessibleMethod "getMinimumIncrement" o = Atk.Value.ValueGetMinimumIncrementMethodInfo
    ResolveSpinButtonAccessibleMethod "getMinimumValue" o = Atk.Value.ValueGetMinimumValueMethodInfo
    ResolveSpinButtonAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveSpinButtonAccessibleMethod "getNActions" o = Atk.Action.ActionGetNActionsMethodInfo
    ResolveSpinButtonAccessibleMethod "getNSelections" o = Atk.Text.TextGetNSelectionsMethodInfo
    ResolveSpinButtonAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveSpinButtonAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveSpinButtonAccessibleMethod "getOffsetAtPoint" o = Atk.Text.TextGetOffsetAtPointMethodInfo
    ResolveSpinButtonAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveSpinButtonAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveSpinButtonAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveSpinButtonAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveSpinButtonAccessibleMethod "getRange" o = Atk.Value.ValueGetRangeMethodInfo
    ResolveSpinButtonAccessibleMethod "getRangeExtents" o = Atk.Text.TextGetRangeExtentsMethodInfo
    ResolveSpinButtonAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveSpinButtonAccessibleMethod "getRunAttributes" o = Atk.Text.TextGetRunAttributesMethodInfo
    ResolveSpinButtonAccessibleMethod "getSelection" o = Atk.Text.TextGetSelectionMethodInfo
    ResolveSpinButtonAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveSpinButtonAccessibleMethod "getStringAtOffset" o = Atk.Text.TextGetStringAtOffsetMethodInfo
    ResolveSpinButtonAccessibleMethod "getSubRanges" o = Atk.Value.ValueGetSubRangesMethodInfo
    ResolveSpinButtonAccessibleMethod "getText" o = Atk.Text.TextGetTextMethodInfo
    ResolveSpinButtonAccessibleMethod "getTextAfterOffset" o = Atk.Text.TextGetTextAfterOffsetMethodInfo
    ResolveSpinButtonAccessibleMethod "getTextAtOffset" o = Atk.Text.TextGetTextAtOffsetMethodInfo
    ResolveSpinButtonAccessibleMethod "getTextBeforeOffset" o = Atk.Text.TextGetTextBeforeOffsetMethodInfo
    ResolveSpinButtonAccessibleMethod "getValueAndText" o = Atk.Value.ValueGetValueAndTextMethodInfo
    ResolveSpinButtonAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveSpinButtonAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveSpinButtonAccessibleMethod "setCaretOffset" o = Atk.Text.TextSetCaretOffsetMethodInfo
    ResolveSpinButtonAccessibleMethod "setCurrentValue" o = Atk.Value.ValueSetCurrentValueMethodInfo
    ResolveSpinButtonAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveSpinButtonAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveSpinButtonAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveSpinButtonAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveSpinButtonAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveSpinButtonAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveSpinButtonAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveSpinButtonAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveSpinButtonAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveSpinButtonAccessibleMethod "setRunAttributes" o = Atk.EditableText.EditableTextSetRunAttributesMethodInfo
    ResolveSpinButtonAccessibleMethod "setSelection" o = Atk.Text.TextSetSelectionMethodInfo
    ResolveSpinButtonAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveSpinButtonAccessibleMethod "setTextContents" o = Atk.EditableText.EditableTextSetTextContentsMethodInfo
    ResolveSpinButtonAccessibleMethod "setValue" o = Atk.Value.ValueSetValueMethodInfo
    ResolveSpinButtonAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveSpinButtonAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveSpinButtonAccessibleMethod t SpinButtonAccessible, O.OverloadedMethod info SpinButtonAccessible p) => OL.IsLabel t (SpinButtonAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveSpinButtonAccessibleMethod t SpinButtonAccessible, O.OverloadedMethod info SpinButtonAccessible p, R.HasField t SpinButtonAccessible p) => R.HasField t SpinButtonAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveSpinButtonAccessibleMethod t SpinButtonAccessible, O.OverloadedMethodInfo info SpinButtonAccessible) => OL.IsLabel t (O.MethodProxy info SpinButtonAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList SpinButtonAccessible
type instance O.AttributeList SpinButtonAccessible = SpinButtonAccessibleAttributeList
type SpinButtonAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList SpinButtonAccessible = SpinButtonAccessibleSignalList
type SpinButtonAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("textAttributesChanged", Atk.Text.TextTextAttributesChangedSignalInfo), '("textCaretMoved", Atk.Text.TextTextCaretMovedSignalInfo), '("textChanged", Atk.Text.TextTextChangedSignalInfo), '("textInsert", Atk.Text.TextTextInsertSignalInfo), '("textRemove", Atk.Text.TextTextRemoveSignalInfo), '("textSelectionChanged", Atk.Text.TextTextSelectionChangedSignalInfo), '("valueChanged", Atk.Value.ValueValueChangedSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


