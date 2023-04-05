{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ToplevelAccessible
    ( 

-- * Exported types
    ToplevelAccessible(..)                  ,
    IsToplevelAccessible                    ,
    toToplevelAccessible                    ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getChildren]("GI.Gtk.Objects.ToplevelAccessible#g:method:getChildren"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRole]("GI.Atk.Objects.Object#g:method:getRole").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole").

#if defined(ENABLE_OVERLOADING)
    ResolveToplevelAccessibleMethod         ,
#endif

-- ** getChildren #method:getChildren#

#if defined(ENABLE_OVERLOADING)
    ToplevelAccessibleGetChildrenMethodInfo ,
#endif
    toplevelAccessibleGetChildren           ,




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

import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype ToplevelAccessible = ToplevelAccessible (SP.ManagedPtr ToplevelAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype ToplevelAccessible where
    toManagedPtr (ToplevelAccessible p) = p

foreign import ccall "gtk_toplevel_accessible_get_type"
    c_gtk_toplevel_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject ToplevelAccessible where
    glibType = c_gtk_toplevel_accessible_get_type

instance B.Types.GObject ToplevelAccessible

-- | Type class for types which can be safely cast to `ToplevelAccessible`, for instance with `toToplevelAccessible`.
class (SP.GObject o, O.IsDescendantOf ToplevelAccessible o) => IsToplevelAccessible o
instance (SP.GObject o, O.IsDescendantOf ToplevelAccessible o) => IsToplevelAccessible o

instance O.HasParentTypes ToplevelAccessible
type instance O.ParentTypes ToplevelAccessible = '[Atk.Object.Object, GObject.Object.Object]

-- | Cast to `ToplevelAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toToplevelAccessible :: (MIO.MonadIO m, IsToplevelAccessible o) => o -> m ToplevelAccessible
toToplevelAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo ToplevelAccessible

-- | Convert 'ToplevelAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ToplevelAccessible) where
    gvalueGType_ = c_gtk_toplevel_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ToplevelAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ToplevelAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ToplevelAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveToplevelAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveToplevelAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveToplevelAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveToplevelAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveToplevelAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveToplevelAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveToplevelAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveToplevelAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveToplevelAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveToplevelAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveToplevelAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveToplevelAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveToplevelAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveToplevelAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveToplevelAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveToplevelAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveToplevelAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveToplevelAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveToplevelAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveToplevelAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveToplevelAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveToplevelAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveToplevelAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveToplevelAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveToplevelAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveToplevelAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveToplevelAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveToplevelAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveToplevelAccessibleMethod "getChildren" o = ToplevelAccessibleGetChildrenMethodInfo
    ResolveToplevelAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveToplevelAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveToplevelAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveToplevelAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveToplevelAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveToplevelAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveToplevelAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveToplevelAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveToplevelAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveToplevelAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveToplevelAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveToplevelAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveToplevelAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveToplevelAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveToplevelAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveToplevelAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveToplevelAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveToplevelAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveToplevelAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveToplevelAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveToplevelAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveToplevelAccessibleMethod t ToplevelAccessible, O.OverloadedMethod info ToplevelAccessible p) => OL.IsLabel t (ToplevelAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveToplevelAccessibleMethod t ToplevelAccessible, O.OverloadedMethod info ToplevelAccessible p, R.HasField t ToplevelAccessible p) => R.HasField t ToplevelAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveToplevelAccessibleMethod t ToplevelAccessible, O.OverloadedMethodInfo info ToplevelAccessible) => OL.IsLabel t (O.MethodProxy info ToplevelAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ToplevelAccessible
type instance O.AttributeList ToplevelAccessible = ToplevelAccessibleAttributeList
type ToplevelAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ToplevelAccessible = ToplevelAccessibleSignalList
type ToplevelAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif

-- method ToplevelAccessible::get_children
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "accessible"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToplevelAccessible" }
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
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Window" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_toplevel_accessible_get_children" gtk_toplevel_accessible_get_children :: 
    Ptr ToplevelAccessible ->               -- accessible : TInterface (Name {namespace = "Gtk", name = "ToplevelAccessible"})
    IO (Ptr (GList (Ptr Gtk.Window.Window)))

-- | /No description available in the introspection data./
toplevelAccessibleGetChildren ::
    (B.CallStack.HasCallStack, MonadIO m, IsToplevelAccessible a) =>
    a
    -> m [Gtk.Window.Window]
    -- ^ __Returns:__ List of
    --   children.
toplevelAccessibleGetChildren accessible = liftIO $ do
    accessible' <- unsafeManagedPtrCastPtr accessible
    result <- gtk_toplevel_accessible_get_children accessible'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.Window.Window) result'
    touchManagedPtr accessible
    return result''

#if defined(ENABLE_OVERLOADING)
data ToplevelAccessibleGetChildrenMethodInfo
instance (signature ~ (m [Gtk.Window.Window]), MonadIO m, IsToplevelAccessible a) => O.OverloadedMethod ToplevelAccessibleGetChildrenMethodInfo a signature where
    overloadedMethod = toplevelAccessibleGetChildren

instance O.OverloadedMethodInfo ToplevelAccessibleGetChildrenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToplevelAccessible.toplevelAccessibleGetChildren",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToplevelAccessible.html#v:toplevelAccessibleGetChildren"
        })


#endif


