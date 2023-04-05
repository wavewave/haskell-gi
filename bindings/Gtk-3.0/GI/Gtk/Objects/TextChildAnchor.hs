{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor' is a spot in the buffer where child widgets can
-- be “anchored” (inserted inline, as if they were characters). The anchor
-- can have multiple widgets anchored, to allow for multiple views.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TextChildAnchor
    ( 

-- * Exported types
    TextChildAnchor(..)                     ,
    IsTextChildAnchor                       ,
    toTextChildAnchor                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeleted]("GI.Gtk.Objects.TextChildAnchor#g:method:getDeleted"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getWidgets]("GI.Gtk.Objects.TextChildAnchor#g:method:getWidgets").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveTextChildAnchorMethod            ,
#endif

-- ** getDeleted #method:getDeleted#

#if defined(ENABLE_OVERLOADING)
    TextChildAnchorGetDeletedMethodInfo     ,
#endif
    textChildAnchorGetDeleted               ,


-- ** getWidgets #method:getWidgets#

#if defined(ENABLE_OVERLOADING)
    TextChildAnchorGetWidgetsMethodInfo     ,
#endif
    textChildAnchorGetWidgets               ,


-- ** new #method:new#

    textChildAnchorNew                      ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype TextChildAnchor = TextChildAnchor (SP.ManagedPtr TextChildAnchor)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextChildAnchor where
    toManagedPtr (TextChildAnchor p) = p

foreign import ccall "gtk_text_child_anchor_get_type"
    c_gtk_text_child_anchor_get_type :: IO B.Types.GType

instance B.Types.TypedObject TextChildAnchor where
    glibType = c_gtk_text_child_anchor_get_type

instance B.Types.GObject TextChildAnchor

-- | Type class for types which can be safely cast to `TextChildAnchor`, for instance with `toTextChildAnchor`.
class (SP.GObject o, O.IsDescendantOf TextChildAnchor o) => IsTextChildAnchor o
instance (SP.GObject o, O.IsDescendantOf TextChildAnchor o) => IsTextChildAnchor o

instance O.HasParentTypes TextChildAnchor
type instance O.ParentTypes TextChildAnchor = '[GObject.Object.Object]

-- | Cast to `TextChildAnchor`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTextChildAnchor :: (MIO.MonadIO m, IsTextChildAnchor o) => o -> m TextChildAnchor
toTextChildAnchor = MIO.liftIO . B.ManagedPtr.unsafeCastTo TextChildAnchor

-- | Convert 'TextChildAnchor' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextChildAnchor) where
    gvalueGType_ = c_gtk_text_child_anchor_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TextChildAnchor)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TextChildAnchor)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TextChildAnchor ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTextChildAnchorMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextChildAnchorMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTextChildAnchorMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTextChildAnchorMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTextChildAnchorMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTextChildAnchorMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTextChildAnchorMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTextChildAnchorMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTextChildAnchorMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTextChildAnchorMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTextChildAnchorMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTextChildAnchorMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTextChildAnchorMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTextChildAnchorMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTextChildAnchorMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTextChildAnchorMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTextChildAnchorMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTextChildAnchorMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTextChildAnchorMethod "getDeleted" o = TextChildAnchorGetDeletedMethodInfo
    ResolveTextChildAnchorMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTextChildAnchorMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTextChildAnchorMethod "getWidgets" o = TextChildAnchorGetWidgetsMethodInfo
    ResolveTextChildAnchorMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTextChildAnchorMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTextChildAnchorMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTextChildAnchorMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextChildAnchorMethod t TextChildAnchor, O.OverloadedMethod info TextChildAnchor p) => OL.IsLabel t (TextChildAnchor -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextChildAnchorMethod t TextChildAnchor, O.OverloadedMethod info TextChildAnchor p, R.HasField t TextChildAnchor p) => R.HasField t TextChildAnchor p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextChildAnchorMethod t TextChildAnchor, O.OverloadedMethodInfo info TextChildAnchor) => OL.IsLabel t (O.MethodProxy info TextChildAnchor) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextChildAnchor
type instance O.AttributeList TextChildAnchor = TextChildAnchorAttributeList
type TextChildAnchorAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TextChildAnchor = TextChildAnchorSignalList
type TextChildAnchorSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method TextChildAnchor::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TextChildAnchor" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_child_anchor_new" gtk_text_child_anchor_new :: 
    IO (Ptr TextChildAnchor)

-- | Creates a new t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor'. Usually you would then insert
-- it into a t'GI.Gtk.Objects.TextBuffer.TextBuffer' with 'GI.Gtk.Objects.TextBuffer.textBufferInsertChildAnchor'.
-- To perform the creation and insertion in one step, use the
-- convenience function 'GI.Gtk.Objects.TextBuffer.textBufferCreateChildAnchor'.
textChildAnchorNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m TextChildAnchor
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor'
textChildAnchorNew  = liftIO $ do
    result <- gtk_text_child_anchor_new
    checkUnexpectedReturnNULL "textChildAnchorNew" result
    result' <- (wrapObject TextChildAnchor) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TextChildAnchor::get_deleted
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "anchor"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextChildAnchor" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextChildAnchor"
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

foreign import ccall "gtk_text_child_anchor_get_deleted" gtk_text_child_anchor_get_deleted :: 
    Ptr TextChildAnchor ->                  -- anchor : TInterface (Name {namespace = "Gtk", name = "TextChildAnchor"})
    IO CInt

-- | Determines whether a child anchor has been deleted from
-- the buffer. Keep in mind that the child anchor will be
-- unreferenced when removed from the buffer, so you need to
-- hold your own reference (with 'GI.GObject.Objects.Object.objectRef') if you plan
-- to use this function — otherwise all deleted child anchors
-- will also be finalized.
textChildAnchorGetDeleted ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextChildAnchor a) =>
    a
    -- ^ /@anchor@/: a t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the child anchor has been deleted from its buffer
textChildAnchorGetDeleted anchor = liftIO $ do
    anchor' <- unsafeManagedPtrCastPtr anchor
    result <- gtk_text_child_anchor_get_deleted anchor'
    let result' = (/= 0) result
    touchManagedPtr anchor
    return result'

#if defined(ENABLE_OVERLOADING)
data TextChildAnchorGetDeletedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextChildAnchor a) => O.OverloadedMethod TextChildAnchorGetDeletedMethodInfo a signature where
    overloadedMethod = textChildAnchorGetDeleted

instance O.OverloadedMethodInfo TextChildAnchorGetDeletedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextChildAnchor.textChildAnchorGetDeleted",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextChildAnchor.html#v:textChildAnchorGetDeleted"
        })


#endif

-- method TextChildAnchor::get_widgets
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "anchor"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextChildAnchor" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextChildAnchor"
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
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Widget" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_child_anchor_get_widgets" gtk_text_child_anchor_get_widgets :: 
    Ptr TextChildAnchor ->                  -- anchor : TInterface (Name {namespace = "Gtk", name = "TextChildAnchor"})
    IO (Ptr (GList (Ptr Gtk.Widget.Widget)))

-- | Gets a list of all widgets anchored at this child anchor.
-- The returned list should be freed with @/g_list_free()/@.
textChildAnchorGetWidgets ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextChildAnchor a) =>
    a
    -- ^ /@anchor@/: a t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor'
    -> m [Gtk.Widget.Widget]
    -- ^ __Returns:__ list of widgets anchored at /@anchor@/
textChildAnchorGetWidgets anchor = liftIO $ do
    anchor' <- unsafeManagedPtrCastPtr anchor
    result <- gtk_text_child_anchor_get_widgets anchor'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.Widget.Widget) result'
    g_list_free result
    touchManagedPtr anchor
    return result''

#if defined(ENABLE_OVERLOADING)
data TextChildAnchorGetWidgetsMethodInfo
instance (signature ~ (m [Gtk.Widget.Widget]), MonadIO m, IsTextChildAnchor a) => O.OverloadedMethod TextChildAnchorGetWidgetsMethodInfo a signature where
    overloadedMethod = textChildAnchorGetWidgets

instance O.OverloadedMethodInfo TextChildAnchorGetWidgetsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextChildAnchor.textChildAnchorGetWidgets",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextChildAnchor.html#v:textChildAnchorGetWidgets"
        })


#endif


