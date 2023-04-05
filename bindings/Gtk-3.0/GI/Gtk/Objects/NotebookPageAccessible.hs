{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.NotebookPageAccessible
    ( 

-- * Exported types
    NotebookPageAccessible(..)              ,
    IsNotebookPageAccessible                ,
    toNotebookPageAccessible                ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [invalidate]("GI.Gtk.Objects.NotebookPageAccessible#g:method:invalidate"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize").

#if defined(ENABLE_OVERLOADING)
    ResolveNotebookPageAccessibleMethod     ,
#endif

-- ** invalidate #method:invalidate#

#if defined(ENABLE_OVERLOADING)
    NotebookPageAccessibleInvalidateMethodInfo,
#endif
    notebookPageAccessibleInvalidate        ,


-- ** new #method:new#

    notebookPageAccessibleNew               ,




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
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.NotebookAccessible as Gtk.NotebookAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype NotebookPageAccessible = NotebookPageAccessible (SP.ManagedPtr NotebookPageAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype NotebookPageAccessible where
    toManagedPtr (NotebookPageAccessible p) = p

foreign import ccall "gtk_notebook_page_accessible_get_type"
    c_gtk_notebook_page_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject NotebookPageAccessible where
    glibType = c_gtk_notebook_page_accessible_get_type

instance B.Types.GObject NotebookPageAccessible

-- | Type class for types which can be safely cast to `NotebookPageAccessible`, for instance with `toNotebookPageAccessible`.
class (SP.GObject o, O.IsDescendantOf NotebookPageAccessible o) => IsNotebookPageAccessible o
instance (SP.GObject o, O.IsDescendantOf NotebookPageAccessible o) => IsNotebookPageAccessible o

instance O.HasParentTypes NotebookPageAccessible
type instance O.ParentTypes NotebookPageAccessible = '[Atk.Object.Object, GObject.Object.Object, Atk.Component.Component]

-- | Cast to `NotebookPageAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toNotebookPageAccessible :: (MIO.MonadIO m, IsNotebookPageAccessible o) => o -> m NotebookPageAccessible
toNotebookPageAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo NotebookPageAccessible

-- | Convert 'NotebookPageAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe NotebookPageAccessible) where
    gvalueGType_ = c_gtk_notebook_page_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr NotebookPageAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr NotebookPageAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject NotebookPageAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveNotebookPageAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveNotebookPageAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveNotebookPageAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveNotebookPageAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveNotebookPageAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveNotebookPageAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveNotebookPageAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveNotebookPageAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveNotebookPageAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveNotebookPageAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveNotebookPageAccessibleMethod "invalidate" o = NotebookPageAccessibleInvalidateMethodInfo
    ResolveNotebookPageAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveNotebookPageAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveNotebookPageAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveNotebookPageAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveNotebookPageAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveNotebookPageAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveNotebookPageAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveNotebookPageAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveNotebookPageAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveNotebookPageAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveNotebookPageAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveNotebookPageAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveNotebookPageAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveNotebookPageAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveNotebookPageAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveNotebookPageAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveNotebookPageAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveNotebookPageAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveNotebookPageAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveNotebookPageAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveNotebookPageAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveNotebookPageAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveNotebookPageAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveNotebookPageAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveNotebookPageAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveNotebookPageAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveNotebookPageAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveNotebookPageAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveNotebookPageAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveNotebookPageAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveNotebookPageAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveNotebookPageAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveNotebookPageAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveNotebookPageAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveNotebookPageAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveNotebookPageAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveNotebookPageAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveNotebookPageAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveNotebookPageAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveNotebookPageAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveNotebookPageAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveNotebookPageAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveNotebookPageAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveNotebookPageAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveNotebookPageAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveNotebookPageAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveNotebookPageAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveNotebookPageAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveNotebookPageAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveNotebookPageAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveNotebookPageAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveNotebookPageAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveNotebookPageAccessibleMethod t NotebookPageAccessible, O.OverloadedMethod info NotebookPageAccessible p) => OL.IsLabel t (NotebookPageAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveNotebookPageAccessibleMethod t NotebookPageAccessible, O.OverloadedMethod info NotebookPageAccessible p, R.HasField t NotebookPageAccessible p) => R.HasField t NotebookPageAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveNotebookPageAccessibleMethod t NotebookPageAccessible, O.OverloadedMethodInfo info NotebookPageAccessible) => OL.IsLabel t (O.MethodProxy info NotebookPageAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList NotebookPageAccessible
type instance O.AttributeList NotebookPageAccessible = NotebookPageAccessibleAttributeList
type NotebookPageAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList NotebookPageAccessible = NotebookPageAccessibleSignalList
type NotebookPageAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif

-- method NotebookPageAccessible::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NotebookAccessible" }
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
--       , Arg
--           { argCName = "child"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
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
--                  Name { namespace = "Gtk" , name = "NotebookPageAccessible" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_notebook_page_accessible_new" gtk_notebook_page_accessible_new :: 
    Ptr Gtk.NotebookAccessible.NotebookAccessible -> -- notebook : TInterface (Name {namespace = "Gtk", name = "NotebookAccessible"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr NotebookPageAccessible)

-- | /No description available in the introspection data./
notebookPageAccessibleNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.NotebookAccessible.IsNotebookAccessible a, Gtk.Widget.IsWidget b) =>
    a
    -> b
    -> m NotebookPageAccessible
notebookPageAccessibleNew notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_notebook_page_accessible_new notebook' child'
    checkUnexpectedReturnNULL "notebookPageAccessibleNew" result
    result' <- (wrapObject NotebookPageAccessible) result
    touchManagedPtr notebook
    touchManagedPtr child
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method NotebookPageAccessible::invalidate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "page"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "NotebookPageAccessible" }
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_notebook_page_accessible_invalidate" gtk_notebook_page_accessible_invalidate :: 
    Ptr NotebookPageAccessible ->           -- page : TInterface (Name {namespace = "Gtk", name = "NotebookPageAccessible"})
    IO ()

-- | /No description available in the introspection data./
notebookPageAccessibleInvalidate ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebookPageAccessible a) =>
    a
    -> m ()
notebookPageAccessibleInvalidate page = liftIO $ do
    page' <- unsafeManagedPtrCastPtr page
    gtk_notebook_page_accessible_invalidate page'
    touchManagedPtr page
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookPageAccessibleInvalidateMethodInfo
instance (signature ~ (m ()), MonadIO m, IsNotebookPageAccessible a) => O.OverloadedMethod NotebookPageAccessibleInvalidateMethodInfo a signature where
    overloadedMethod = notebookPageAccessibleInvalidate

instance O.OverloadedMethodInfo NotebookPageAccessibleInvalidateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NotebookPageAccessible.notebookPageAccessibleInvalidate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NotebookPageAccessible.html#v:notebookPageAccessibleInvalidate"
        })


#endif


