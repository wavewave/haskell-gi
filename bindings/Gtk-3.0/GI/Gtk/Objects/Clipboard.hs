{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Clipboard
    ( 
#if defined(ENABLE_OVERLOADING)
    ClipboardWaitForRichTextMethodInfo      ,
#endif

-- * Exported types
    Clipboard(..)                           ,
    IsClipboard                             ,
    toClipboard                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [clear]("GI.Gtk.Objects.Clipboard#g:method:clear"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [requestContents]("GI.Gtk.Objects.Clipboard#g:method:requestContents"), [requestImage]("GI.Gtk.Objects.Clipboard#g:method:requestImage"), [requestRichText]("GI.Gtk.Objects.Clipboard#g:method:requestRichText"), [requestTargets]("GI.Gtk.Objects.Clipboard#g:method:requestTargets"), [requestText]("GI.Gtk.Objects.Clipboard#g:method:requestText"), [requestUris]("GI.Gtk.Objects.Clipboard#g:method:requestUris"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [store]("GI.Gtk.Objects.Clipboard#g:method:store"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [waitForContents]("GI.Gtk.Objects.Clipboard#g:method:waitForContents"), [waitForImage]("GI.Gtk.Objects.Clipboard#g:method:waitForImage"), [waitForRichText]("GI.Gtk.Objects.Clipboard#g:method:waitForRichText"), [waitForTargets]("GI.Gtk.Objects.Clipboard#g:method:waitForTargets"), [waitForText]("GI.Gtk.Objects.Clipboard#g:method:waitForText"), [waitForUris]("GI.Gtk.Objects.Clipboard#g:method:waitForUris"), [waitIsImageAvailable]("GI.Gtk.Objects.Clipboard#g:method:waitIsImageAvailable"), [waitIsRichTextAvailable]("GI.Gtk.Objects.Clipboard#g:method:waitIsRichTextAvailable"), [waitIsTargetAvailable]("GI.Gtk.Objects.Clipboard#g:method:waitIsTargetAvailable"), [waitIsTextAvailable]("GI.Gtk.Objects.Clipboard#g:method:waitIsTextAvailable"), [waitIsUrisAvailable]("GI.Gtk.Objects.Clipboard#g:method:waitIsUrisAvailable"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDisplay]("GI.Gtk.Objects.Clipboard#g:method:getDisplay"), [getOwner]("GI.Gtk.Objects.Clipboard#g:method:getOwner"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setCanStore]("GI.Gtk.Objects.Clipboard#g:method:setCanStore"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setImage]("GI.Gtk.Objects.Clipboard#g:method:setImage"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setText]("GI.Gtk.Objects.Clipboard#g:method:setText").

#if defined(ENABLE_OVERLOADING)
    ResolveClipboardMethod                  ,
#endif

-- ** clear #method:clear#

#if defined(ENABLE_OVERLOADING)
    ClipboardClearMethodInfo                ,
#endif
    clipboardClear                          ,


-- ** get #method:get#

    clipboardGet                            ,


-- ** getDefault #method:getDefault#

    clipboardGetDefault                     ,


-- ** getDisplay #method:getDisplay#

#if defined(ENABLE_OVERLOADING)
    ClipboardGetDisplayMethodInfo           ,
#endif
    clipboardGetDisplay                     ,


-- ** getForDisplay #method:getForDisplay#

    clipboardGetForDisplay                  ,


-- ** getOwner #method:getOwner#

#if defined(ENABLE_OVERLOADING)
    ClipboardGetOwnerMethodInfo             ,
#endif
    clipboardGetOwner                       ,


-- ** requestContents #method:requestContents#

#if defined(ENABLE_OVERLOADING)
    ClipboardRequestContentsMethodInfo      ,
#endif
    clipboardRequestContents                ,


-- ** requestImage #method:requestImage#

#if defined(ENABLE_OVERLOADING)
    ClipboardRequestImageMethodInfo         ,
#endif
    clipboardRequestImage                   ,


-- ** requestRichText #method:requestRichText#

#if defined(ENABLE_OVERLOADING)
    ClipboardRequestRichTextMethodInfo      ,
#endif
    clipboardRequestRichText                ,


-- ** requestTargets #method:requestTargets#

#if defined(ENABLE_OVERLOADING)
    ClipboardRequestTargetsMethodInfo       ,
#endif
    clipboardRequestTargets                 ,


-- ** requestText #method:requestText#

#if defined(ENABLE_OVERLOADING)
    ClipboardRequestTextMethodInfo          ,
#endif
    clipboardRequestText                    ,


-- ** requestUris #method:requestUris#

#if defined(ENABLE_OVERLOADING)
    ClipboardRequestUrisMethodInfo          ,
#endif
    clipboardRequestUris                    ,


-- ** setCanStore #method:setCanStore#

#if defined(ENABLE_OVERLOADING)
    ClipboardSetCanStoreMethodInfo          ,
#endif
    clipboardSetCanStore                    ,


-- ** setImage #method:setImage#

#if defined(ENABLE_OVERLOADING)
    ClipboardSetImageMethodInfo             ,
#endif
    clipboardSetImage                       ,


-- ** setText #method:setText#

#if defined(ENABLE_OVERLOADING)
    ClipboardSetTextMethodInfo              ,
#endif
    clipboardSetText                        ,


-- ** store #method:store#

#if defined(ENABLE_OVERLOADING)
    ClipboardStoreMethodInfo                ,
#endif
    clipboardStore                          ,


-- ** waitForContents #method:waitForContents#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitForContentsMethodInfo      ,
#endif
    clipboardWaitForContents                ,


-- ** waitForImage #method:waitForImage#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitForImageMethodInfo         ,
#endif
    clipboardWaitForImage                   ,


-- ** waitForTargets #method:waitForTargets#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitForTargetsMethodInfo       ,
#endif
    clipboardWaitForTargets                 ,


-- ** waitForText #method:waitForText#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitForTextMethodInfo          ,
#endif
    clipboardWaitForText                    ,


-- ** waitForUris #method:waitForUris#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitForUrisMethodInfo          ,
#endif
    clipboardWaitForUris                    ,


-- ** waitIsImageAvailable #method:waitIsImageAvailable#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitIsImageAvailableMethodInfo ,
#endif
    clipboardWaitIsImageAvailable           ,


-- ** waitIsRichTextAvailable #method:waitIsRichTextAvailable#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitIsRichTextAvailableMethodInfo,
#endif
    clipboardWaitIsRichTextAvailable        ,


-- ** waitIsTargetAvailable #method:waitIsTargetAvailable#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitIsTargetAvailableMethodInfo,
#endif
    clipboardWaitIsTargetAvailable          ,


-- ** waitIsTextAvailable #method:waitIsTextAvailable#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitIsTextAvailableMethodInfo  ,
#endif
    clipboardWaitIsTextAvailable            ,


-- ** waitIsUrisAvailable #method:waitIsUrisAvailable#

#if defined(ENABLE_OVERLOADING)
    ClipboardWaitIsUrisAvailableMethodInfo  ,
#endif
    clipboardWaitIsUrisAvailable            ,




 -- * Signals


-- ** ownerChange #signal:ownerChange#

    ClipboardOwnerChangeCallback            ,
#if defined(ENABLE_OVERLOADING)
    ClipboardOwnerChangeSignalInfo          ,
#endif
    afterClipboardOwnerChange               ,
    onClipboardOwnerChange                  ,




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
import qualified GI.Gdk.Objects.Display as Gdk.Display
import qualified GI.Gdk.Structs.Atom as Gdk.Atom
import qualified GI.Gdk.Structs.EventOwnerChange as Gdk.EventOwnerChange
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextBuffer as Gtk.TextBuffer
import {-# SOURCE #-} qualified GI.Gtk.Structs.SelectionData as Gtk.SelectionData
import {-# SOURCE #-} qualified GI.Gtk.Structs.TargetEntry as Gtk.TargetEntry

-- | Memory-managed wrapper type.
newtype Clipboard = Clipboard (SP.ManagedPtr Clipboard)
    deriving (Eq)

instance SP.ManagedPtrNewtype Clipboard where
    toManagedPtr (Clipboard p) = p

foreign import ccall "gtk_clipboard_get_type"
    c_gtk_clipboard_get_type :: IO B.Types.GType

instance B.Types.TypedObject Clipboard where
    glibType = c_gtk_clipboard_get_type

instance B.Types.GObject Clipboard

-- | Type class for types which can be safely cast to `Clipboard`, for instance with `toClipboard`.
class (SP.GObject o, O.IsDescendantOf Clipboard o) => IsClipboard o
instance (SP.GObject o, O.IsDescendantOf Clipboard o) => IsClipboard o

instance O.HasParentTypes Clipboard
type instance O.ParentTypes Clipboard = '[GObject.Object.Object]

-- | Cast to `Clipboard`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toClipboard :: (MIO.MonadIO m, IsClipboard o) => o -> m Clipboard
toClipboard = MIO.liftIO . B.ManagedPtr.unsafeCastTo Clipboard

-- | Convert 'Clipboard' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Clipboard) where
    gvalueGType_ = c_gtk_clipboard_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Clipboard)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Clipboard)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Clipboard ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveClipboardMethod (t :: Symbol) (o :: *) :: * where
    ResolveClipboardMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveClipboardMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveClipboardMethod "clear" o = ClipboardClearMethodInfo
    ResolveClipboardMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveClipboardMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveClipboardMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveClipboardMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveClipboardMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveClipboardMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveClipboardMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveClipboardMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveClipboardMethod "requestContents" o = ClipboardRequestContentsMethodInfo
    ResolveClipboardMethod "requestImage" o = ClipboardRequestImageMethodInfo
    ResolveClipboardMethod "requestRichText" o = ClipboardRequestRichTextMethodInfo
    ResolveClipboardMethod "requestTargets" o = ClipboardRequestTargetsMethodInfo
    ResolveClipboardMethod "requestText" o = ClipboardRequestTextMethodInfo
    ResolveClipboardMethod "requestUris" o = ClipboardRequestUrisMethodInfo
    ResolveClipboardMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveClipboardMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveClipboardMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveClipboardMethod "store" o = ClipboardStoreMethodInfo
    ResolveClipboardMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveClipboardMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveClipboardMethod "waitForContents" o = ClipboardWaitForContentsMethodInfo
    ResolveClipboardMethod "waitForImage" o = ClipboardWaitForImageMethodInfo
    ResolveClipboardMethod "waitForRichText" o = ClipboardWaitForRichTextMethodInfo
    ResolveClipboardMethod "waitForTargets" o = ClipboardWaitForTargetsMethodInfo
    ResolveClipboardMethod "waitForText" o = ClipboardWaitForTextMethodInfo
    ResolveClipboardMethod "waitForUris" o = ClipboardWaitForUrisMethodInfo
    ResolveClipboardMethod "waitIsImageAvailable" o = ClipboardWaitIsImageAvailableMethodInfo
    ResolveClipboardMethod "waitIsRichTextAvailable" o = ClipboardWaitIsRichTextAvailableMethodInfo
    ResolveClipboardMethod "waitIsTargetAvailable" o = ClipboardWaitIsTargetAvailableMethodInfo
    ResolveClipboardMethod "waitIsTextAvailable" o = ClipboardWaitIsTextAvailableMethodInfo
    ResolveClipboardMethod "waitIsUrisAvailable" o = ClipboardWaitIsUrisAvailableMethodInfo
    ResolveClipboardMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveClipboardMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveClipboardMethod "getDisplay" o = ClipboardGetDisplayMethodInfo
    ResolveClipboardMethod "getOwner" o = ClipboardGetOwnerMethodInfo
    ResolveClipboardMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveClipboardMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveClipboardMethod "setCanStore" o = ClipboardSetCanStoreMethodInfo
    ResolveClipboardMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveClipboardMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveClipboardMethod "setImage" o = ClipboardSetImageMethodInfo
    ResolveClipboardMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveClipboardMethod "setText" o = ClipboardSetTextMethodInfo
    ResolveClipboardMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveClipboardMethod t Clipboard, O.OverloadedMethod info Clipboard p) => OL.IsLabel t (Clipboard -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveClipboardMethod t Clipboard, O.OverloadedMethod info Clipboard p, R.HasField t Clipboard p) => R.HasField t Clipboard p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveClipboardMethod t Clipboard, O.OverloadedMethodInfo info Clipboard) => OL.IsLabel t (O.MethodProxy info Clipboard) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Clipboard::owner-change
-- | /No description available in the introspection data./
type ClipboardOwnerChangeCallback =
    Gdk.EventOwnerChange.EventOwnerChange
    -> IO ()

type C_ClipboardOwnerChangeCallback =
    Ptr Clipboard ->                        -- object
    Ptr Gdk.EventOwnerChange.EventOwnerChange ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ClipboardOwnerChangeCallback`.
foreign import ccall "wrapper"
    mk_ClipboardOwnerChangeCallback :: C_ClipboardOwnerChangeCallback -> IO (FunPtr C_ClipboardOwnerChangeCallback)

wrap_ClipboardOwnerChangeCallback :: 
    GObject a => (a -> ClipboardOwnerChangeCallback) ->
    C_ClipboardOwnerChangeCallback
wrap_ClipboardOwnerChangeCallback gi'cb gi'selfPtr event _ = do
    event' <- (newPtr Gdk.EventOwnerChange.EventOwnerChange) event
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  event'


-- | Connect a signal handler for the [ownerChange](#signal:ownerChange) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' clipboard #ownerChange callback
-- @
-- 
-- 
onClipboardOwnerChange :: (IsClipboard a, MonadIO m) => a -> ((?self :: a) => ClipboardOwnerChangeCallback) -> m SignalHandlerId
onClipboardOwnerChange obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ClipboardOwnerChangeCallback wrapped
    wrapped'' <- mk_ClipboardOwnerChangeCallback wrapped'
    connectSignalFunPtr obj "owner-change" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [ownerChange](#signal:ownerChange) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' clipboard #ownerChange callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterClipboardOwnerChange :: (IsClipboard a, MonadIO m) => a -> ((?self :: a) => ClipboardOwnerChangeCallback) -> m SignalHandlerId
afterClipboardOwnerChange obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ClipboardOwnerChangeCallback wrapped
    wrapped'' <- mk_ClipboardOwnerChangeCallback wrapped'
    connectSignalFunPtr obj "owner-change" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ClipboardOwnerChangeSignalInfo
instance SignalInfo ClipboardOwnerChangeSignalInfo where
    type HaskellCallbackType ClipboardOwnerChangeSignalInfo = ClipboardOwnerChangeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ClipboardOwnerChangeCallback cb
        cb'' <- mk_ClipboardOwnerChangeCallback cb'
        connectSignalFunPtr obj "owner-change" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard::owner-change"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#g:signal:ownerChange"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Clipboard
type instance O.AttributeList Clipboard = ClipboardAttributeList
type ClipboardAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Clipboard = ClipboardSignalList
type ClipboardSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("ownerChange", ClipboardOwnerChangeSignalInfo)] :: [(Symbol, *)])

#endif

-- method Clipboard::clear
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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

foreign import ccall "gtk_clipboard_clear" gtk_clipboard_clear :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO ()

-- | /No description available in the introspection data./
clipboardClear ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m ()
clipboardClear clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    gtk_clipboard_clear clipboard'
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardClearMethodInfo
instance (signature ~ (m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardClearMethodInfo a signature where
    overloadedMethod = clipboardClear

instance O.OverloadedMethodInfo ClipboardClearMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardClear",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardClear"
        })


#endif

-- method Clipboard::get_display
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Display" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_get_display" gtk_clipboard_get_display :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO (Ptr Gdk.Display.Display)

-- | /No description available in the introspection data./
clipboardGetDisplay ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m Gdk.Display.Display
clipboardGetDisplay clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    result <- gtk_clipboard_get_display clipboard'
    checkUnexpectedReturnNULL "clipboardGetDisplay" result
    result' <- (newObject Gdk.Display.Display) result
    touchManagedPtr clipboard
    return result'

#if defined(ENABLE_OVERLOADING)
data ClipboardGetDisplayMethodInfo
instance (signature ~ (m Gdk.Display.Display), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardGetDisplayMethodInfo a signature where
    overloadedMethod = clipboardGetDisplay

instance O.OverloadedMethodInfo ClipboardGetDisplayMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardGetDisplay",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardGetDisplay"
        })


#endif

-- method Clipboard::get_owner
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
-- returnType: Just (TInterface Name { namespace = "GObject" , name = "Object" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_get_owner" gtk_clipboard_get_owner :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO (Ptr GObject.Object.Object)

-- | /No description available in the introspection data./
clipboardGetOwner ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m GObject.Object.Object
clipboardGetOwner clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    result <- gtk_clipboard_get_owner clipboard'
    checkUnexpectedReturnNULL "clipboardGetOwner" result
    result' <- (newObject GObject.Object.Object) result
    touchManagedPtr clipboard
    return result'

#if defined(ENABLE_OVERLOADING)
data ClipboardGetOwnerMethodInfo
instance (signature ~ (m GObject.Object.Object), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardGetOwnerMethodInfo a signature where
    overloadedMethod = clipboardGetOwner

instance O.OverloadedMethodInfo ClipboardGetOwnerMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardGetOwner",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardGetOwner"
        })


#endif

-- method Clipboard::request_contents
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "target"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
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
--           { argCName = "callback"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ClipboardReceivedFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeAsync
--           , argClosure = 3
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
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

foreign import ccall "gtk_clipboard_request_contents" gtk_clipboard_request_contents :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr Gdk.Atom.Atom ->                    -- target : TInterface (Name {namespace = "Gdk", name = "Atom"})
    FunPtr Gtk.Callbacks.C_ClipboardReceivedFunc -> -- callback : TInterface (Name {namespace = "Gtk", name = "ClipboardReceivedFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | /No description available in the introspection data./
clipboardRequestContents ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> Gdk.Atom.Atom
    -> Gtk.Callbacks.ClipboardReceivedFunc
    -> m ()
clipboardRequestContents clipboard target callback = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    target' <- unsafeManagedPtrGetPtr target
    ptrcallback <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_ClipboardReceivedFunc))
    callback' <- Gtk.Callbacks.mk_ClipboardReceivedFunc (Gtk.Callbacks.wrap_ClipboardReceivedFunc (Just ptrcallback) (Gtk.Callbacks.drop_closures_ClipboardReceivedFunc callback))
    poke ptrcallback callback'
    let userData = nullPtr
    gtk_clipboard_request_contents clipboard' target' callback' userData
    touchManagedPtr clipboard
    touchManagedPtr target
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardRequestContentsMethodInfo
instance (signature ~ (Gdk.Atom.Atom -> Gtk.Callbacks.ClipboardReceivedFunc -> m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardRequestContentsMethodInfo a signature where
    overloadedMethod = clipboardRequestContents

instance O.OverloadedMethodInfo ClipboardRequestContentsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardRequestContents",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardRequestContents"
        })


#endif

-- method Clipboard::request_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "callback"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ClipboardImageReceivedFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeAsync
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
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

foreign import ccall "gtk_clipboard_request_image" gtk_clipboard_request_image :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    FunPtr Gtk.Callbacks.C_ClipboardImageReceivedFunc -> -- callback : TInterface (Name {namespace = "Gtk", name = "ClipboardImageReceivedFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | /No description available in the introspection data./
clipboardRequestImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> Gtk.Callbacks.ClipboardImageReceivedFunc
    -> m ()
clipboardRequestImage clipboard callback = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    ptrcallback <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_ClipboardImageReceivedFunc))
    callback' <- Gtk.Callbacks.mk_ClipboardImageReceivedFunc (Gtk.Callbacks.wrap_ClipboardImageReceivedFunc (Just ptrcallback) (Gtk.Callbacks.drop_closures_ClipboardImageReceivedFunc callback))
    poke ptrcallback callback'
    let userData = nullPtr
    gtk_clipboard_request_image clipboard' callback' userData
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardRequestImageMethodInfo
instance (signature ~ (Gtk.Callbacks.ClipboardImageReceivedFunc -> m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardRequestImageMethodInfo a signature where
    overloadedMethod = clipboardRequestImage

instance O.OverloadedMethodInfo ClipboardRequestImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardRequestImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardRequestImage"
        })


#endif

-- method Clipboard::request_rich_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
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
--           { argCName = "callback"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ClipboardRichTextReceivedFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeAsync
--           , argClosure = 3
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
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

foreign import ccall "gtk_clipboard_request_rich_text" gtk_clipboard_request_rich_text :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr Gtk.TextBuffer.TextBuffer ->        -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    FunPtr Gtk.Callbacks.C_ClipboardRichTextReceivedFunc -> -- callback : TInterface (Name {namespace = "Gtk", name = "ClipboardRichTextReceivedFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | /No description available in the introspection data./
clipboardRequestRichText ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a, Gtk.TextBuffer.IsTextBuffer b) =>
    a
    -> b
    -> Gtk.Callbacks.ClipboardRichTextReceivedFunc
    -> m ()
clipboardRequestRichText clipboard buffer callback = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    buffer' <- unsafeManagedPtrCastPtr buffer
    ptrcallback <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_ClipboardRichTextReceivedFunc))
    callback' <- Gtk.Callbacks.mk_ClipboardRichTextReceivedFunc (Gtk.Callbacks.wrap_ClipboardRichTextReceivedFunc (Just ptrcallback) (Gtk.Callbacks.drop_closures_ClipboardRichTextReceivedFunc callback))
    poke ptrcallback callback'
    let userData = nullPtr
    gtk_clipboard_request_rich_text clipboard' buffer' callback' userData
    touchManagedPtr clipboard
    touchManagedPtr buffer
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardRequestRichTextMethodInfo
instance (signature ~ (b -> Gtk.Callbacks.ClipboardRichTextReceivedFunc -> m ()), MonadIO m, IsClipboard a, Gtk.TextBuffer.IsTextBuffer b) => O.OverloadedMethod ClipboardRequestRichTextMethodInfo a signature where
    overloadedMethod = clipboardRequestRichText

instance O.OverloadedMethodInfo ClipboardRequestRichTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardRequestRichText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardRequestRichText"
        })


#endif

-- method Clipboard::request_targets
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "callback"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ClipboardTargetsReceivedFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeAsync
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
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

foreign import ccall "gtk_clipboard_request_targets" gtk_clipboard_request_targets :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    FunPtr Gtk.Callbacks.C_ClipboardTargetsReceivedFunc -> -- callback : TInterface (Name {namespace = "Gtk", name = "ClipboardTargetsReceivedFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | /No description available in the introspection data./
clipboardRequestTargets ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> Gtk.Callbacks.ClipboardTargetsReceivedFunc
    -> m ()
clipboardRequestTargets clipboard callback = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    ptrcallback <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_ClipboardTargetsReceivedFunc))
    callback' <- Gtk.Callbacks.mk_ClipboardTargetsReceivedFunc (Gtk.Callbacks.wrap_ClipboardTargetsReceivedFunc (Just ptrcallback) (Gtk.Callbacks.drop_closures_ClipboardTargetsReceivedFunc callback))
    poke ptrcallback callback'
    let userData = nullPtr
    gtk_clipboard_request_targets clipboard' callback' userData
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardRequestTargetsMethodInfo
instance (signature ~ (Gtk.Callbacks.ClipboardTargetsReceivedFunc -> m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardRequestTargetsMethodInfo a signature where
    overloadedMethod = clipboardRequestTargets

instance O.OverloadedMethodInfo ClipboardRequestTargetsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardRequestTargets",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardRequestTargets"
        })


#endif

-- method Clipboard::request_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "callback"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ClipboardTextReceivedFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeAsync
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
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

foreign import ccall "gtk_clipboard_request_text" gtk_clipboard_request_text :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    FunPtr Gtk.Callbacks.C_ClipboardTextReceivedFunc -> -- callback : TInterface (Name {namespace = "Gtk", name = "ClipboardTextReceivedFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | /No description available in the introspection data./
clipboardRequestText ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> Gtk.Callbacks.ClipboardTextReceivedFunc
    -> m ()
clipboardRequestText clipboard callback = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    ptrcallback <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_ClipboardTextReceivedFunc))
    callback' <- Gtk.Callbacks.mk_ClipboardTextReceivedFunc (Gtk.Callbacks.wrap_ClipboardTextReceivedFunc (Just ptrcallback) (Gtk.Callbacks.drop_closures_ClipboardTextReceivedFunc callback))
    poke ptrcallback callback'
    let userData = nullPtr
    gtk_clipboard_request_text clipboard' callback' userData
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardRequestTextMethodInfo
instance (signature ~ (Gtk.Callbacks.ClipboardTextReceivedFunc -> m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardRequestTextMethodInfo a signature where
    overloadedMethod = clipboardRequestText

instance O.OverloadedMethodInfo ClipboardRequestTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardRequestText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardRequestText"
        })


#endif

-- method Clipboard::request_uris
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "callback"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ClipboardURIReceivedFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeAsync
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
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

foreign import ccall "gtk_clipboard_request_uris" gtk_clipboard_request_uris :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    FunPtr Gtk.Callbacks.C_ClipboardURIReceivedFunc -> -- callback : TInterface (Name {namespace = "Gtk", name = "ClipboardURIReceivedFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | /No description available in the introspection data./
clipboardRequestUris ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> Gtk.Callbacks.ClipboardURIReceivedFunc
    -> m ()
clipboardRequestUris clipboard callback = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    ptrcallback <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_ClipboardURIReceivedFunc))
    callback' <- Gtk.Callbacks.mk_ClipboardURIReceivedFunc (Gtk.Callbacks.wrap_ClipboardURIReceivedFunc (Just ptrcallback) (Gtk.Callbacks.drop_closures_ClipboardURIReceivedFunc callback))
    poke ptrcallback callback'
    let userData = nullPtr
    gtk_clipboard_request_uris clipboard' callback' userData
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardRequestUrisMethodInfo
instance (signature ~ (Gtk.Callbacks.ClipboardURIReceivedFunc -> m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardRequestUrisMethodInfo a signature where
    overloadedMethod = clipboardRequestUris

instance O.OverloadedMethodInfo ClipboardRequestUrisMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardRequestUris",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardRequestUris"
        })


#endif

-- method Clipboard::set_can_store
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 2
--                 (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
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
-- Lengths: [ Arg
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_set_can_store" gtk_clipboard_set_can_store :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr Gtk.TargetEntry.TargetEntry ->      -- targets : TCArray False (-1) 2 (TInterface (Name {namespace = "Gtk", name = "TargetEntry"}))
    Int32 ->                                -- n_targets : TBasicType TInt
    IO ()

-- | /No description available in the introspection data./
clipboardSetCanStore ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> Maybe ([Gtk.TargetEntry.TargetEntry])
    -> m ()
clipboardSetCanStore clipboard targets = liftIO $ do
    let nTargets = case targets of
            Nothing -> 0
            Just jTargets -> fromIntegral $ P.length jTargets
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    maybeTargets <- case targets of
        Nothing -> return nullPtr
        Just jTargets -> do
            jTargets' <- mapM unsafeManagedPtrGetPtr jTargets
            jTargets'' <- packBlockArray 16 jTargets'
            return jTargets''
    gtk_clipboard_set_can_store clipboard' maybeTargets nTargets
    touchManagedPtr clipboard
    whenJust targets (mapM_ touchManagedPtr)
    freeMem maybeTargets
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardSetCanStoreMethodInfo
instance (signature ~ (Maybe ([Gtk.TargetEntry.TargetEntry]) -> m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardSetCanStoreMethodInfo a signature where
    overloadedMethod = clipboardSetCanStore

instance O.OverloadedMethodInfo ClipboardSetCanStoreMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardSetCanStore",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardSetCanStore"
        })


#endif

-- method Clipboard::set_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "pixbuf"
--           , argType =
--               TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }
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

foreign import ccall "gtk_clipboard_set_image" gtk_clipboard_set_image :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

-- | /No description available in the introspection data./
clipboardSetImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -> b
    -> m ()
clipboardSetImage clipboard pixbuf = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    pixbuf' <- unsafeManagedPtrCastPtr pixbuf
    gtk_clipboard_set_image clipboard' pixbuf'
    touchManagedPtr clipboard
    touchManagedPtr pixbuf
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardSetImageMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsClipboard a, GdkPixbuf.Pixbuf.IsPixbuf b) => O.OverloadedMethod ClipboardSetImageMethodInfo a signature where
    overloadedMethod = clipboardSetImage

instance O.OverloadedMethodInfo ClipboardSetImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardSetImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardSetImage"
        })


#endif

-- method Clipboard::set_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "text"
--           , argType = TBasicType TUTF8
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
--           { argCName = "len"
--           , argType = TBasicType TInt
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

foreign import ccall "gtk_clipboard_set_text" gtk_clipboard_set_text :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    CString ->                              -- text : TBasicType TUTF8
    Int32 ->                                -- len : TBasicType TInt
    IO ()

-- | /No description available in the introspection data./
clipboardSetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> T.Text
    -> Int32
    -> m ()
clipboardSetText clipboard text len = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    text' <- textToCString text
    gtk_clipboard_set_text clipboard' text' len
    touchManagedPtr clipboard
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardSetTextMethodInfo
instance (signature ~ (T.Text -> Int32 -> m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardSetTextMethodInfo a signature where
    overloadedMethod = clipboardSetText

instance O.OverloadedMethodInfo ClipboardSetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardSetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardSetText"
        })


#endif

-- method Clipboard::store
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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

foreign import ccall "gtk_clipboard_store" gtk_clipboard_store :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO ()

-- | /No description available in the introspection data./
clipboardStore ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m ()
clipboardStore clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    gtk_clipboard_store clipboard'
    touchManagedPtr clipboard
    return ()

#if defined(ENABLE_OVERLOADING)
data ClipboardStoreMethodInfo
instance (signature ~ (m ()), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardStoreMethodInfo a signature where
    overloadedMethod = clipboardStore

instance O.OverloadedMethodInfo ClipboardStoreMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardStore",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardStore"
        })


#endif

-- method Clipboard::wait_for_contents
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "target"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
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
--               (TInterface Name { namespace = "Gtk" , name = "SelectionData" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_for_contents" gtk_clipboard_wait_for_contents :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr Gdk.Atom.Atom ->                    -- target : TInterface (Name {namespace = "Gdk", name = "Atom"})
    IO (Ptr Gtk.SelectionData.SelectionData)

-- | /No description available in the introspection data./
clipboardWaitForContents ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> Gdk.Atom.Atom
    -> m (Maybe Gtk.SelectionData.SelectionData)
clipboardWaitForContents clipboard target = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    target' <- unsafeManagedPtrGetPtr target
    result <- gtk_clipboard_wait_for_contents clipboard' target'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapBoxed Gtk.SelectionData.SelectionData) result'
        return result''
    touchManagedPtr clipboard
    touchManagedPtr target
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForContentsMethodInfo
instance (signature ~ (Gdk.Atom.Atom -> m (Maybe Gtk.SelectionData.SelectionData)), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitForContentsMethodInfo a signature where
    overloadedMethod = clipboardWaitForContents

instance O.OverloadedMethodInfo ClipboardWaitForContentsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitForContents",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitForContents"
        })


#endif

-- method Clipboard::wait_for_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_for_image" gtk_clipboard_wait_for_image :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | /No description available in the introspection data./
clipboardWaitForImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
clipboardWaitForImage clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    result <- gtk_clipboard_wait_for_image clipboard'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result'
        return result''
    touchManagedPtr clipboard
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForImageMethodInfo
instance (signature ~ (m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitForImageMethodInfo a signature where
    overloadedMethod = clipboardWaitForImage

instance O.OverloadedMethodInfo ClipboardWaitForImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitForImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitForImage"
        })


#endif

-- XXX Could not generate method Clipboard::wait_for_rich_text
-- Not implemented: Don't know how to allocate "format" of type TInterface (Name {namespace = "Gdk", name = "Atom"})
#if defined(ENABLE_OVERLOADING)
-- XXX: Dummy instance, since code generation failed.
-- Please file a bug at http://github.com/haskell-gi/haskell-gi.
data ClipboardWaitForRichTextMethodInfo
instance (p ~ (), o ~ O.UnsupportedMethodError "waitForRichText" Clipboard) => O.OverloadedMethod ClipboardWaitForRichTextMethodInfo o p where
    overloadedMethod = undefined

instance (o ~ O.UnsupportedMethodError "waitForRichText" Clipboard) => O.OverloadedMethodInfo ClipboardWaitForRichTextMethodInfo o where
    overloadedMethodInfo = undefined

#endif

-- method Clipboard::wait_for_targets
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkClipboard" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 2
--                 (TInterface Name { namespace = "Gdk" , name = "Atom" })
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location\n          to store an array of targets. The result stored here must\n          be freed with g_free()."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferContainer
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store number of items in @targets."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just "location to store number of items in @targets."
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_for_targets" gtk_clipboard_wait_for_targets :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr (Ptr (Ptr Gdk.Atom.Atom)) ->        -- targets : TCArray False (-1) 2 (TInterface (Name {namespace = "Gdk", name = "Atom"}))
    Ptr Int32 ->                            -- n_targets : TBasicType TInt
    IO CInt

-- | Returns a list of targets that are present on the clipboard, or 'P.Nothing'
-- if there aren’t any targets available. The returned list must be
-- freed with 'GI.GLib.Functions.free'.
-- This function waits for the data to be received using the main
-- loop, so events, timeouts, etc, may be dispatched during the wait.
-- 
-- /Since: 2.4/
clipboardWaitForTargets ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -- ^ /@clipboard@/: a t'GI.Gtk.Objects.Clipboard.Clipboard'
    -> m ((Bool, [Gdk.Atom.Atom]))
    -- ^ __Returns:__ 'P.True' if any targets are present on the clipboard,
    --               otherwise 'P.False'.
clipboardWaitForTargets clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    targets <- callocMem :: IO (Ptr (Ptr (Ptr Gdk.Atom.Atom)))
    nTargets <- allocMem :: IO (Ptr Int32)
    result <- gtk_clipboard_wait_for_targets clipboard' targets nTargets
    nTargets' <- peek nTargets
    let result' = (/= 0) result
    targets' <- peek targets
    targets'' <- (unpackPtrArrayWithLength nTargets') targets'
    targets''' <- mapM (newPtr Gdk.Atom.Atom) targets''
    freeMem targets'
    touchManagedPtr clipboard
    freeMem targets
    freeMem nTargets
    return (result', targets''')

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForTargetsMethodInfo
instance (signature ~ (m ((Bool, [Gdk.Atom.Atom]))), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitForTargetsMethodInfo a signature where
    overloadedMethod = clipboardWaitForTargets

instance O.OverloadedMethodInfo ClipboardWaitForTargetsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitForTargets",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitForTargets"
        })


#endif

-- method Clipboard::wait_for_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_for_text" gtk_clipboard_wait_for_text :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO CString

-- | /No description available in the introspection data./
clipboardWaitForText ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m (Maybe T.Text)
clipboardWaitForText clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    result <- gtk_clipboard_wait_for_text clipboard'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr clipboard
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForTextMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitForTextMethodInfo a signature where
    overloadedMethod = clipboardWaitForText

instance O.OverloadedMethodInfo ClipboardWaitForTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitForText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitForText"
        })


#endif

-- method Clipboard::wait_for_uris
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
-- returnType: Just (TCArray True (-1) (-1) (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_for_uris" gtk_clipboard_wait_for_uris :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO (Ptr CString)

-- | /No description available in the introspection data./
clipboardWaitForUris ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m (Maybe [T.Text])
clipboardWaitForUris clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    result <- gtk_clipboard_wait_for_uris clipboard'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- unpackZeroTerminatedUTF8CArray result'
        mapZeroTerminatedCArray freeMem result'
        freeMem result'
        return result''
    touchManagedPtr clipboard
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForUrisMethodInfo
instance (signature ~ (m (Maybe [T.Text])), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitForUrisMethodInfo a signature where
    overloadedMethod = clipboardWaitForUris

instance O.OverloadedMethodInfo ClipboardWaitForUrisMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitForUris",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitForUris"
        })


#endif

-- method Clipboard::wait_is_image_available
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_is_image_available" gtk_clipboard_wait_is_image_available :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO CInt

-- | /No description available in the introspection data./
clipboardWaitIsImageAvailable ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m Bool
clipboardWaitIsImageAvailable clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    result <- gtk_clipboard_wait_is_image_available clipboard'
    let result' = (/= 0) result
    touchManagedPtr clipboard
    return result'

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsImageAvailableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitIsImageAvailableMethodInfo a signature where
    overloadedMethod = clipboardWaitIsImageAvailable

instance O.OverloadedMethodInfo ClipboardWaitIsImageAvailableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitIsImageAvailable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitIsImageAvailable"
        })


#endif

-- method Clipboard::wait_is_rich_text_available
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_is_rich_text_available" gtk_clipboard_wait_is_rich_text_available :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr Gtk.TextBuffer.TextBuffer ->        -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO CInt

-- | /No description available in the introspection data./
clipboardWaitIsRichTextAvailable ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a, Gtk.TextBuffer.IsTextBuffer b) =>
    a
    -> b
    -> m Bool
clipboardWaitIsRichTextAvailable clipboard buffer = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_clipboard_wait_is_rich_text_available clipboard' buffer'
    let result' = (/= 0) result
    touchManagedPtr clipboard
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsRichTextAvailableMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsClipboard a, Gtk.TextBuffer.IsTextBuffer b) => O.OverloadedMethod ClipboardWaitIsRichTextAvailableMethodInfo a signature where
    overloadedMethod = clipboardWaitIsRichTextAvailable

instance O.OverloadedMethodInfo ClipboardWaitIsRichTextAvailableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitIsRichTextAvailable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitIsRichTextAvailable"
        })


#endif

-- method Clipboard::wait_is_target_available
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
--           { argCName = "target"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_is_target_available" gtk_clipboard_wait_is_target_available :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    Ptr Gdk.Atom.Atom ->                    -- target : TInterface (Name {namespace = "Gdk", name = "Atom"})
    IO CInt

-- | /No description available in the introspection data./
clipboardWaitIsTargetAvailable ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> Gdk.Atom.Atom
    -> m Bool
clipboardWaitIsTargetAvailable clipboard target = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    target' <- unsafeManagedPtrGetPtr target
    result <- gtk_clipboard_wait_is_target_available clipboard' target'
    let result' = (/= 0) result
    touchManagedPtr clipboard
    touchManagedPtr target
    return result'

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsTargetAvailableMethodInfo
instance (signature ~ (Gdk.Atom.Atom -> m Bool), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitIsTargetAvailableMethodInfo a signature where
    overloadedMethod = clipboardWaitIsTargetAvailable

instance O.OverloadedMethodInfo ClipboardWaitIsTargetAvailableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitIsTargetAvailable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitIsTargetAvailable"
        })


#endif

-- method Clipboard::wait_is_text_available
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_is_text_available" gtk_clipboard_wait_is_text_available :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO CInt

-- | /No description available in the introspection data./
clipboardWaitIsTextAvailable ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m Bool
clipboardWaitIsTextAvailable clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    result <- gtk_clipboard_wait_is_text_available clipboard'
    let result' = (/= 0) result
    touchManagedPtr clipboard
    return result'

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsTextAvailableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitIsTextAvailableMethodInfo a signature where
    overloadedMethod = clipboardWaitIsTextAvailable

instance O.OverloadedMethodInfo ClipboardWaitIsTextAvailableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitIsTextAvailable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitIsTextAvailable"
        })


#endif

-- method Clipboard::wait_is_uris_available
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "clipboard"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Clipboard" }
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_wait_is_uris_available" gtk_clipboard_wait_is_uris_available :: 
    Ptr Clipboard ->                        -- clipboard : TInterface (Name {namespace = "Gtk", name = "Clipboard"})
    IO CInt

-- | /No description available in the introspection data./
clipboardWaitIsUrisAvailable ::
    (B.CallStack.HasCallStack, MonadIO m, IsClipboard a) =>
    a
    -> m Bool
clipboardWaitIsUrisAvailable clipboard = liftIO $ do
    clipboard' <- unsafeManagedPtrCastPtr clipboard
    result <- gtk_clipboard_wait_is_uris_available clipboard'
    let result' = (/= 0) result
    touchManagedPtr clipboard
    return result'

#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsUrisAvailableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsClipboard a) => O.OverloadedMethod ClipboardWaitIsUrisAvailableMethodInfo a signature where
    overloadedMethod = clipboardWaitIsUrisAvailable

instance O.OverloadedMethodInfo ClipboardWaitIsUrisAvailableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Clipboard.clipboardWaitIsUrisAvailable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Clipboard.html#v:clipboardWaitIsUrisAvailable"
        })


#endif

-- method Clipboard::get
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "selection"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Clipboard" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_get" gtk_clipboard_get :: 
    Ptr Gdk.Atom.Atom ->                    -- selection : TInterface (Name {namespace = "Gdk", name = "Atom"})
    IO (Ptr Clipboard)

-- | /No description available in the introspection data./
clipboardGet ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gdk.Atom.Atom
    -> m Clipboard
clipboardGet selection = liftIO $ do
    selection' <- unsafeManagedPtrGetPtr selection
    result <- gtk_clipboard_get selection'
    checkUnexpectedReturnNULL "clipboardGet" result
    result' <- (newObject Clipboard) result
    touchManagedPtr selection
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Clipboard::get_default
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "display"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Display" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GdkDisplay for which the clipboard is to be retrieved."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Clipboard" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_get_default" gtk_clipboard_get_default :: 
    Ptr Gdk.Display.Display ->              -- display : TInterface (Name {namespace = "Gdk", name = "Display"})
    IO (Ptr Clipboard)

-- | Returns the default clipboard object for use with cut\/copy\/paste menu items
-- and keyboard shortcuts.
-- 
-- /Since: 3.16/
clipboardGetDefault ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Display.IsDisplay a) =>
    a
    -- ^ /@display@/: the t'GI.Gdk.Objects.Display.Display' for which the clipboard is to be retrieved.
    -> m Clipboard
    -- ^ __Returns:__ the default clipboard object.
clipboardGetDefault display = liftIO $ do
    display' <- unsafeManagedPtrCastPtr display
    result <- gtk_clipboard_get_default display'
    checkUnexpectedReturnNULL "clipboardGetDefault" result
    result' <- (newObject Clipboard) result
    touchManagedPtr display
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Clipboard::get_for_display
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "display"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Display" }
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
--           { argCName = "selection"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Atom" }
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Clipboard" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_clipboard_get_for_display" gtk_clipboard_get_for_display :: 
    Ptr Gdk.Display.Display ->              -- display : TInterface (Name {namespace = "Gdk", name = "Display"})
    Ptr Gdk.Atom.Atom ->                    -- selection : TInterface (Name {namespace = "Gdk", name = "Atom"})
    IO (Ptr Clipboard)

-- | /No description available in the introspection data./
clipboardGetForDisplay ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Display.IsDisplay a) =>
    a
    -> Gdk.Atom.Atom
    -> m Clipboard
clipboardGetForDisplay display selection = liftIO $ do
    display' <- unsafeManagedPtrCastPtr display
    selection' <- unsafeManagedPtrGetPtr selection
    result <- gtk_clipboard_get_for_display display' selection'
    checkUnexpectedReturnNULL "clipboardGetForDisplay" result
    result' <- (newObject Clipboard) result
    touchManagedPtr display
    touchManagedPtr selection
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


