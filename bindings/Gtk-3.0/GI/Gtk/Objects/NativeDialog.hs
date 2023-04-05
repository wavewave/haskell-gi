{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Native dialogs are platform dialogs that don\'t use t'GI.Gtk.Objects.Dialog.Dialog' or
-- t'GI.Gtk.Objects.Window.Window'. They are used in order to integrate better with a
-- platform, by looking the same as other native applications and
-- supporting platform specific features.
-- 
-- The t'GI.Gtk.Objects.Dialog.Dialog' functions cannot be used on such objects, but we
-- need a similar API in order to drive them. The t'GI.Gtk.Objects.NativeDialog.NativeDialog'
-- object is an API that allows you to do this. It allows you to set
-- various common properties on the dialog, as well as show and hide
-- it and get a [NativeDialog::response]("GI.Gtk.Objects.NativeDialog#g:signal:response") signal when the user finished
-- with the dialog.
-- 
-- There is also a 'GI.Gtk.Objects.NativeDialog.nativeDialogRun' helper that makes it easy
-- to run any native dialog in a modal way with a recursive mainloop,
-- similar to 'GI.Gtk.Objects.Dialog.dialogRun'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.NativeDialog
    ( 

-- * Exported types
    NativeDialog(..)                        ,
    IsNativeDialog                          ,
    toNativeDialog                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [destroy]("GI.Gtk.Objects.NativeDialog#g:method:destroy"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hide]("GI.Gtk.Objects.NativeDialog#g:method:hide"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [run]("GI.Gtk.Objects.NativeDialog#g:method:run"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [show]("GI.Gtk.Objects.NativeDialog#g:method:show"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getModal]("GI.Gtk.Objects.NativeDialog#g:method:getModal"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getTitle]("GI.Gtk.Objects.NativeDialog#g:method:getTitle"), [getTransientFor]("GI.Gtk.Objects.NativeDialog#g:method:getTransientFor"), [getVisible]("GI.Gtk.Objects.NativeDialog#g:method:getVisible").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setModal]("GI.Gtk.Objects.NativeDialog#g:method:setModal"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setTitle]("GI.Gtk.Objects.NativeDialog#g:method:setTitle"), [setTransientFor]("GI.Gtk.Objects.NativeDialog#g:method:setTransientFor").

#if defined(ENABLE_OVERLOADING)
    ResolveNativeDialogMethod               ,
#endif

-- ** destroy #method:destroy#

#if defined(ENABLE_OVERLOADING)
    NativeDialogDestroyMethodInfo           ,
#endif
    nativeDialogDestroy                     ,


-- ** getModal #method:getModal#

#if defined(ENABLE_OVERLOADING)
    NativeDialogGetModalMethodInfo          ,
#endif
    nativeDialogGetModal                    ,


-- ** getTitle #method:getTitle#

#if defined(ENABLE_OVERLOADING)
    NativeDialogGetTitleMethodInfo          ,
#endif
    nativeDialogGetTitle                    ,


-- ** getTransientFor #method:getTransientFor#

#if defined(ENABLE_OVERLOADING)
    NativeDialogGetTransientForMethodInfo   ,
#endif
    nativeDialogGetTransientFor             ,


-- ** getVisible #method:getVisible#

#if defined(ENABLE_OVERLOADING)
    NativeDialogGetVisibleMethodInfo        ,
#endif
    nativeDialogGetVisible                  ,


-- ** hide #method:hide#

#if defined(ENABLE_OVERLOADING)
    NativeDialogHideMethodInfo              ,
#endif
    nativeDialogHide                        ,


-- ** run #method:run#

#if defined(ENABLE_OVERLOADING)
    NativeDialogRunMethodInfo               ,
#endif
    nativeDialogRun                         ,


-- ** setModal #method:setModal#

#if defined(ENABLE_OVERLOADING)
    NativeDialogSetModalMethodInfo          ,
#endif
    nativeDialogSetModal                    ,


-- ** setTitle #method:setTitle#

#if defined(ENABLE_OVERLOADING)
    NativeDialogSetTitleMethodInfo          ,
#endif
    nativeDialogSetTitle                    ,


-- ** setTransientFor #method:setTransientFor#

#if defined(ENABLE_OVERLOADING)
    NativeDialogSetTransientForMethodInfo   ,
#endif
    nativeDialogSetTransientFor             ,


-- ** show #method:show#

#if defined(ENABLE_OVERLOADING)
    NativeDialogShowMethodInfo              ,
#endif
    nativeDialogShow                        ,




 -- * Properties


-- ** modal #attr:modal#
-- | Whether the window should be modal with respect to its transient parent.
-- 
-- /Since: 3.20/

#if defined(ENABLE_OVERLOADING)
    NativeDialogModalPropertyInfo           ,
#endif
    constructNativeDialogModal              ,
    getNativeDialogModal                    ,
#if defined(ENABLE_OVERLOADING)
    nativeDialogModal                       ,
#endif
    setNativeDialogModal                    ,


-- ** title #attr:title#
-- | The title of the dialog window
-- 
-- /Since: 3.20/

#if defined(ENABLE_OVERLOADING)
    NativeDialogTitlePropertyInfo           ,
#endif
    constructNativeDialogTitle              ,
    getNativeDialogTitle                    ,
#if defined(ENABLE_OVERLOADING)
    nativeDialogTitle                       ,
#endif
    setNativeDialogTitle                    ,


-- ** transientFor #attr:transientFor#
-- | The transient parent of the dialog, or 'P.Nothing' for none.
-- 
-- /Since: 3.20/

#if defined(ENABLE_OVERLOADING)
    NativeDialogTransientForPropertyInfo    ,
#endif
    clearNativeDialogTransientFor           ,
    constructNativeDialogTransientFor       ,
    getNativeDialogTransientFor             ,
#if defined(ENABLE_OVERLOADING)
    nativeDialogTransientFor                ,
#endif
    setNativeDialogTransientFor             ,


-- ** visible #attr:visible#
-- | Whether the window is currenlty visible.
-- 
-- /Since: 3.20/

#if defined(ENABLE_OVERLOADING)
    NativeDialogVisiblePropertyInfo         ,
#endif
    constructNativeDialogVisible            ,
    getNativeDialogVisible                  ,
#if defined(ENABLE_OVERLOADING)
    nativeDialogVisible                     ,
#endif
    setNativeDialogVisible                  ,




 -- * Signals


-- ** response #signal:response#

    NativeDialogResponseCallback            ,
#if defined(ENABLE_OVERLOADING)
    NativeDialogResponseSignalInfo          ,
#endif
    afterNativeDialogResponse               ,
    onNativeDialogResponse                  ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype NativeDialog = NativeDialog (SP.ManagedPtr NativeDialog)
    deriving (Eq)

instance SP.ManagedPtrNewtype NativeDialog where
    toManagedPtr (NativeDialog p) = p

foreign import ccall "gtk_native_dialog_get_type"
    c_gtk_native_dialog_get_type :: IO B.Types.GType

instance B.Types.TypedObject NativeDialog where
    glibType = c_gtk_native_dialog_get_type

instance B.Types.GObject NativeDialog

-- | Type class for types which can be safely cast to `NativeDialog`, for instance with `toNativeDialog`.
class (SP.GObject o, O.IsDescendantOf NativeDialog o) => IsNativeDialog o
instance (SP.GObject o, O.IsDescendantOf NativeDialog o) => IsNativeDialog o

instance O.HasParentTypes NativeDialog
type instance O.ParentTypes NativeDialog = '[GObject.Object.Object]

-- | Cast to `NativeDialog`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toNativeDialog :: (MIO.MonadIO m, IsNativeDialog o) => o -> m NativeDialog
toNativeDialog = MIO.liftIO . B.ManagedPtr.unsafeCastTo NativeDialog

-- | Convert 'NativeDialog' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe NativeDialog) where
    gvalueGType_ = c_gtk_native_dialog_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr NativeDialog)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr NativeDialog)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject NativeDialog ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveNativeDialogMethod (t :: Symbol) (o :: *) :: * where
    ResolveNativeDialogMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveNativeDialogMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveNativeDialogMethod "destroy" o = NativeDialogDestroyMethodInfo
    ResolveNativeDialogMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveNativeDialogMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveNativeDialogMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveNativeDialogMethod "hide" o = NativeDialogHideMethodInfo
    ResolveNativeDialogMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveNativeDialogMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveNativeDialogMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveNativeDialogMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveNativeDialogMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveNativeDialogMethod "run" o = NativeDialogRunMethodInfo
    ResolveNativeDialogMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveNativeDialogMethod "show" o = NativeDialogShowMethodInfo
    ResolveNativeDialogMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveNativeDialogMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveNativeDialogMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveNativeDialogMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveNativeDialogMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveNativeDialogMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveNativeDialogMethod "getModal" o = NativeDialogGetModalMethodInfo
    ResolveNativeDialogMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveNativeDialogMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveNativeDialogMethod "getTitle" o = NativeDialogGetTitleMethodInfo
    ResolveNativeDialogMethod "getTransientFor" o = NativeDialogGetTransientForMethodInfo
    ResolveNativeDialogMethod "getVisible" o = NativeDialogGetVisibleMethodInfo
    ResolveNativeDialogMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveNativeDialogMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveNativeDialogMethod "setModal" o = NativeDialogSetModalMethodInfo
    ResolveNativeDialogMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveNativeDialogMethod "setTitle" o = NativeDialogSetTitleMethodInfo
    ResolveNativeDialogMethod "setTransientFor" o = NativeDialogSetTransientForMethodInfo
    ResolveNativeDialogMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveNativeDialogMethod t NativeDialog, O.OverloadedMethod info NativeDialog p) => OL.IsLabel t (NativeDialog -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveNativeDialogMethod t NativeDialog, O.OverloadedMethod info NativeDialog p, R.HasField t NativeDialog p) => R.HasField t NativeDialog p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveNativeDialogMethod t NativeDialog, O.OverloadedMethodInfo info NativeDialog) => OL.IsLabel t (O.MethodProxy info NativeDialog) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal NativeDialog::response
-- | Emitted when the user responds to the dialog.
-- 
-- When this is called the dialog has been hidden.
-- 
-- If you call 'GI.Gtk.Objects.NativeDialog.nativeDialogHide' before the user responds to
-- the dialog this signal will not be emitted.
-- 
-- /Since: 3.20/
type NativeDialogResponseCallback =
    Int32
    -- ^ /@responseId@/: the response ID
    -> IO ()

type C_NativeDialogResponseCallback =
    Ptr NativeDialog ->                     -- object
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_NativeDialogResponseCallback`.
foreign import ccall "wrapper"
    mk_NativeDialogResponseCallback :: C_NativeDialogResponseCallback -> IO (FunPtr C_NativeDialogResponseCallback)

wrap_NativeDialogResponseCallback :: 
    GObject a => (a -> NativeDialogResponseCallback) ->
    C_NativeDialogResponseCallback
wrap_NativeDialogResponseCallback gi'cb gi'selfPtr responseId _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  responseId


-- | Connect a signal handler for the [response](#signal:response) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' nativeDialog #response callback
-- @
-- 
-- 
onNativeDialogResponse :: (IsNativeDialog a, MonadIO m) => a -> ((?self :: a) => NativeDialogResponseCallback) -> m SignalHandlerId
onNativeDialogResponse obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NativeDialogResponseCallback wrapped
    wrapped'' <- mk_NativeDialogResponseCallback wrapped'
    connectSignalFunPtr obj "response" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [response](#signal:response) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' nativeDialog #response callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNativeDialogResponse :: (IsNativeDialog a, MonadIO m) => a -> ((?self :: a) => NativeDialogResponseCallback) -> m SignalHandlerId
afterNativeDialogResponse obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NativeDialogResponseCallback wrapped
    wrapped'' <- mk_NativeDialogResponseCallback wrapped'
    connectSignalFunPtr obj "response" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NativeDialogResponseSignalInfo
instance SignalInfo NativeDialogResponseSignalInfo where
    type HaskellCallbackType NativeDialogResponseSignalInfo = NativeDialogResponseCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NativeDialogResponseCallback cb
        cb'' <- mk_NativeDialogResponseCallback cb'
        connectSignalFunPtr obj "response" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog::response"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#g:signal:response"})

#endif

-- VVV Prop "modal"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@modal@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' nativeDialog #modal
-- @
getNativeDialogModal :: (MonadIO m, IsNativeDialog o) => o -> m Bool
getNativeDialogModal obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "modal"

-- | Set the value of the “@modal@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' nativeDialog [ #modal 'Data.GI.Base.Attributes.:=' value ]
-- @
setNativeDialogModal :: (MonadIO m, IsNativeDialog o) => o -> Bool -> m ()
setNativeDialogModal obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "modal" val

-- | Construct a `GValueConstruct` with valid value for the “@modal@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNativeDialogModal :: (IsNativeDialog o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructNativeDialogModal val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "modal" val

#if defined(ENABLE_OVERLOADING)
data NativeDialogModalPropertyInfo
instance AttrInfo NativeDialogModalPropertyInfo where
    type AttrAllowedOps NativeDialogModalPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NativeDialogModalPropertyInfo = IsNativeDialog
    type AttrSetTypeConstraint NativeDialogModalPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint NativeDialogModalPropertyInfo = (~) Bool
    type AttrTransferType NativeDialogModalPropertyInfo = Bool
    type AttrGetType NativeDialogModalPropertyInfo = Bool
    type AttrLabel NativeDialogModalPropertyInfo = "modal"
    type AttrOrigin NativeDialogModalPropertyInfo = NativeDialog
    attrGet = getNativeDialogModal
    attrSet = setNativeDialogModal
    attrTransfer _ v = do
        return v
    attrConstruct = constructNativeDialogModal
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.modal"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#g:attr:modal"
        })
#endif

-- VVV Prop "title"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' nativeDialog #title
-- @
getNativeDialogTitle :: (MonadIO m, IsNativeDialog o) => o -> m (Maybe T.Text)
getNativeDialogTitle obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "title"

-- | Set the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' nativeDialog [ #title 'Data.GI.Base.Attributes.:=' value ]
-- @
setNativeDialogTitle :: (MonadIO m, IsNativeDialog o) => o -> T.Text -> m ()
setNativeDialogTitle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "title" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@title@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNativeDialogTitle :: (IsNativeDialog o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructNativeDialogTitle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "title" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data NativeDialogTitlePropertyInfo
instance AttrInfo NativeDialogTitlePropertyInfo where
    type AttrAllowedOps NativeDialogTitlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NativeDialogTitlePropertyInfo = IsNativeDialog
    type AttrSetTypeConstraint NativeDialogTitlePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint NativeDialogTitlePropertyInfo = (~) T.Text
    type AttrTransferType NativeDialogTitlePropertyInfo = T.Text
    type AttrGetType NativeDialogTitlePropertyInfo = (Maybe T.Text)
    type AttrLabel NativeDialogTitlePropertyInfo = "title"
    type AttrOrigin NativeDialogTitlePropertyInfo = NativeDialog
    attrGet = getNativeDialogTitle
    attrSet = setNativeDialogTitle
    attrTransfer _ v = do
        return v
    attrConstruct = constructNativeDialogTitle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.title"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#g:attr:title"
        })
#endif

-- VVV Prop "transient-for"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Window"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@transient-for@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' nativeDialog #transientFor
-- @
getNativeDialogTransientFor :: (MonadIO m, IsNativeDialog o) => o -> m (Maybe Gtk.Window.Window)
getNativeDialogTransientFor obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "transient-for" Gtk.Window.Window

-- | Set the value of the “@transient-for@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' nativeDialog [ #transientFor 'Data.GI.Base.Attributes.:=' value ]
-- @
setNativeDialogTransientFor :: (MonadIO m, IsNativeDialog o, Gtk.Window.IsWindow a) => o -> a -> m ()
setNativeDialogTransientFor obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "transient-for" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@transient-for@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNativeDialogTransientFor :: (IsNativeDialog o, MIO.MonadIO m, Gtk.Window.IsWindow a) => a -> m (GValueConstruct o)
constructNativeDialogTransientFor val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "transient-for" (P.Just val)

-- | Set the value of the “@transient-for@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #transientFor
-- @
clearNativeDialogTransientFor :: (MonadIO m, IsNativeDialog o) => o -> m ()
clearNativeDialogTransientFor obj = liftIO $ B.Properties.setObjectPropertyObject obj "transient-for" (Nothing :: Maybe Gtk.Window.Window)

#if defined(ENABLE_OVERLOADING)
data NativeDialogTransientForPropertyInfo
instance AttrInfo NativeDialogTransientForPropertyInfo where
    type AttrAllowedOps NativeDialogTransientForPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint NativeDialogTransientForPropertyInfo = IsNativeDialog
    type AttrSetTypeConstraint NativeDialogTransientForPropertyInfo = Gtk.Window.IsWindow
    type AttrTransferTypeConstraint NativeDialogTransientForPropertyInfo = Gtk.Window.IsWindow
    type AttrTransferType NativeDialogTransientForPropertyInfo = Gtk.Window.Window
    type AttrGetType NativeDialogTransientForPropertyInfo = (Maybe Gtk.Window.Window)
    type AttrLabel NativeDialogTransientForPropertyInfo = "transient-for"
    type AttrOrigin NativeDialogTransientForPropertyInfo = NativeDialog
    attrGet = getNativeDialogTransientFor
    attrSet = setNativeDialogTransientFor
    attrTransfer _ v = do
        unsafeCastTo Gtk.Window.Window v
    attrConstruct = constructNativeDialogTransientFor
    attrClear = clearNativeDialogTransientFor
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.transientFor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#g:attr:transientFor"
        })
#endif

-- VVV Prop "visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' nativeDialog #visible
-- @
getNativeDialogVisible :: (MonadIO m, IsNativeDialog o) => o -> m Bool
getNativeDialogVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "visible"

-- | Set the value of the “@visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' nativeDialog [ #visible 'Data.GI.Base.Attributes.:=' value ]
-- @
setNativeDialogVisible :: (MonadIO m, IsNativeDialog o) => o -> Bool -> m ()
setNativeDialogVisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "visible" val

-- | Construct a `GValueConstruct` with valid value for the “@visible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNativeDialogVisible :: (IsNativeDialog o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructNativeDialogVisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "visible" val

#if defined(ENABLE_OVERLOADING)
data NativeDialogVisiblePropertyInfo
instance AttrInfo NativeDialogVisiblePropertyInfo where
    type AttrAllowedOps NativeDialogVisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NativeDialogVisiblePropertyInfo = IsNativeDialog
    type AttrSetTypeConstraint NativeDialogVisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint NativeDialogVisiblePropertyInfo = (~) Bool
    type AttrTransferType NativeDialogVisiblePropertyInfo = Bool
    type AttrGetType NativeDialogVisiblePropertyInfo = Bool
    type AttrLabel NativeDialogVisiblePropertyInfo = "visible"
    type AttrOrigin NativeDialogVisiblePropertyInfo = NativeDialog
    attrGet = getNativeDialogVisible
    attrSet = setNativeDialogVisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructNativeDialogVisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.visible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#g:attr:visible"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList NativeDialog
type instance O.AttributeList NativeDialog = NativeDialogAttributeList
type NativeDialogAttributeList = ('[ '("modal", NativeDialogModalPropertyInfo), '("title", NativeDialogTitlePropertyInfo), '("transientFor", NativeDialogTransientForPropertyInfo), '("visible", NativeDialogVisiblePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
nativeDialogModal :: AttrLabelProxy "modal"
nativeDialogModal = AttrLabelProxy

nativeDialogTitle :: AttrLabelProxy "title"
nativeDialogTitle = AttrLabelProxy

nativeDialogTransientFor :: AttrLabelProxy "transientFor"
nativeDialogTransientFor = AttrLabelProxy

nativeDialogVisible :: AttrLabelProxy "visible"
nativeDialogVisible = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList NativeDialog = NativeDialogSignalList
type NativeDialogSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("response", NativeDialogResponseSignalInfo)] :: [(Symbol, *)])

#endif

-- method NativeDialog::destroy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
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

foreign import ccall "gtk_native_dialog_destroy" gtk_native_dialog_destroy :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    IO ()

-- | Destroys a dialog.
-- 
-- When a dialog is destroyed, it will break any references it holds
-- to other objects. If it is visible it will be hidden and any underlying
-- window system resources will be destroyed.
-- 
-- Note that this does not release any reference to the object (as opposed to
-- destroying a GtkWindow) because there is no reference from the windowing
-- system to the t'GI.Gtk.Objects.NativeDialog.NativeDialog'.
-- 
-- /Since: 3.20/
nativeDialogDestroy ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> m ()
nativeDialogDestroy self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    gtk_native_dialog_destroy self'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data NativeDialogDestroyMethodInfo
instance (signature ~ (m ()), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogDestroyMethodInfo a signature where
    overloadedMethod = nativeDialogDestroy

instance O.OverloadedMethodInfo NativeDialogDestroyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogDestroy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogDestroy"
        })


#endif

-- method NativeDialog::get_modal
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
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

foreign import ccall "gtk_native_dialog_get_modal" gtk_native_dialog_get_modal :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    IO CInt

-- | Returns whether the dialog is modal. See 'GI.Gtk.Objects.NativeDialog.nativeDialogSetModal'.
-- 
-- /Since: 3.20/
nativeDialogGetModal ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the dialog is set to be modal
nativeDialogGetModal self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_native_dialog_get_modal self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data NativeDialogGetModalMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogGetModalMethodInfo a signature where
    overloadedMethod = nativeDialogGetModal

instance O.OverloadedMethodInfo NativeDialogGetModalMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogGetModal",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogGetModal"
        })


#endif

-- method NativeDialog::get_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
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

foreign import ccall "gtk_native_dialog_get_title" gtk_native_dialog_get_title :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    IO CString

-- | Gets the title of the t'GI.Gtk.Objects.NativeDialog.NativeDialog'.
-- 
-- /Since: 3.20/
nativeDialogGetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the title of the dialog, or 'P.Nothing' if none has
    --    been set explicitly. The returned string is owned by the widget
    --    and must not be modified or freed.
nativeDialogGetTitle self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_native_dialog_get_title self'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr self
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NativeDialogGetTitleMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogGetTitleMethodInfo a signature where
    overloadedMethod = nativeDialogGetTitle

instance O.OverloadedMethodInfo NativeDialogGetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogGetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogGetTitle"
        })


#endif

-- method NativeDialog::get_transient_for
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_native_dialog_get_transient_for" gtk_native_dialog_get_transient_for :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    IO (Ptr Gtk.Window.Window)

-- | Fetches the transient parent for this window. See
-- 'GI.Gtk.Objects.NativeDialog.nativeDialogSetTransientFor'.
-- 
-- /Since: 3.20/
nativeDialogGetTransientFor ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> m (Maybe Gtk.Window.Window)
    -- ^ __Returns:__ the transient parent for this window,
    -- or 'P.Nothing' if no transient parent has been set.
nativeDialogGetTransientFor self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_native_dialog_get_transient_for self'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Window.Window) result'
        return result''
    touchManagedPtr self
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NativeDialogGetTransientForMethodInfo
instance (signature ~ (m (Maybe Gtk.Window.Window)), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogGetTransientForMethodInfo a signature where
    overloadedMethod = nativeDialogGetTransientFor

instance O.OverloadedMethodInfo NativeDialogGetTransientForMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogGetTransientFor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogGetTransientFor"
        })


#endif

-- method NativeDialog::get_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
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

foreign import ccall "gtk_native_dialog_get_visible" gtk_native_dialog_get_visible :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    IO CInt

-- | Determines whether the dialog is visible.
-- 
-- /Since: 3.20/
nativeDialogGetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the dialog is visible
nativeDialogGetVisible self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_native_dialog_get_visible self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data NativeDialogGetVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogGetVisibleMethodInfo a signature where
    overloadedMethod = nativeDialogGetVisible

instance O.OverloadedMethodInfo NativeDialogGetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogGetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogGetVisible"
        })


#endif

-- method NativeDialog::hide
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
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

foreign import ccall "gtk_native_dialog_hide" gtk_native_dialog_hide :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    IO ()

-- | Hides the dialog if it is visilbe, aborting any interaction. Once this
-- is called the  [NativeDialog::response]("GI.Gtk.Objects.NativeDialog#g:signal:response") signal will not be emitted
-- until after the next call to 'GI.Gtk.Objects.NativeDialog.nativeDialogShow'.
-- 
-- If the dialog is not visible this does nothing.
-- 
-- /Since: 3.20/
nativeDialogHide ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> m ()
nativeDialogHide self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    gtk_native_dialog_hide self'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data NativeDialogHideMethodInfo
instance (signature ~ (m ()), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogHideMethodInfo a signature where
    overloadedMethod = nativeDialogHide

instance O.OverloadedMethodInfo NativeDialogHideMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogHide",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogHide"
        })


#endif

-- method NativeDialog::run
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
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

foreign import ccall "gtk_native_dialog_run" gtk_native_dialog_run :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    IO Int32

-- | Blocks in a recursive main loop until /@self@/ emits the
-- [NativeDialog::response]("GI.Gtk.Objects.NativeDialog#g:signal:response") signal. It then returns the response ID
-- from the [response](#g:signal:response) signal emission.
-- 
-- Before entering the recursive main loop, 'GI.Gtk.Objects.NativeDialog.nativeDialogRun'
-- calls 'GI.Gtk.Objects.NativeDialog.nativeDialogShow' on the dialog for you.
-- 
-- After 'GI.Gtk.Objects.NativeDialog.nativeDialogRun' returns, then dialog will be hidden.
-- 
-- Typical usage of this function might be:
-- 
-- === /C code/
-- >
-- >  gint result = gtk_native_dialog_run (GTK_NATIVE_DIALOG (dialog));
-- >  switch (result)
-- >    {
-- >      case GTK_RESPONSE_ACCEPT:
-- >         do_application_specific_something ();
-- >         break;
-- >      default:
-- >         do_nothing_since_dialog_was_cancelled ();
-- >         break;
-- >    }
-- >  g_object_unref (dialog);
-- 
-- 
-- Note that even though the recursive main loop gives the effect of a
-- modal dialog (it prevents the user from interacting with other
-- windows in the same window group while the dialog is run), callbacks
-- such as timeouts, IO channel watches, DND drops, etc, will
-- be triggered during a 'GI.Gtk.Objects.NativeDialog.nativeDialogRun' call.
-- 
-- /Since: 3.20/
nativeDialogRun ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> m Int32
    -- ^ __Returns:__ response ID
nativeDialogRun self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_native_dialog_run self'
    touchManagedPtr self
    return result

#if defined(ENABLE_OVERLOADING)
data NativeDialogRunMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogRunMethodInfo a signature where
    overloadedMethod = nativeDialogRun

instance O.OverloadedMethodInfo NativeDialogRunMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogRun",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogRun"
        })


#endif

-- method NativeDialog::set_modal
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modal"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the window is modal"
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

foreign import ccall "gtk_native_dialog_set_modal" gtk_native_dialog_set_modal :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    CInt ->                                 -- modal : TBasicType TBoolean
    IO ()

-- | Sets a dialog modal or non-modal. Modal dialogs prevent interaction
-- with other windows in the same application. To keep modal dialogs
-- on top of main application windows, use
-- 'GI.Gtk.Objects.NativeDialog.nativeDialogSetTransientFor' to make the dialog transient for the
-- parent; most [window managers][gtk-X11-arch]
-- will then disallow lowering the dialog below the parent.
-- 
-- /Since: 3.20/
nativeDialogSetModal ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> Bool
    -- ^ /@modal@/: whether the window is modal
    -> m ()
nativeDialogSetModal self modal = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let modal' = (fromIntegral . fromEnum) modal
    gtk_native_dialog_set_modal self' modal'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data NativeDialogSetModalMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogSetModalMethodInfo a signature where
    overloadedMethod = nativeDialogSetModal

instance O.OverloadedMethodInfo NativeDialogSetModalMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogSetModal",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogSetModal"
        })


#endif

-- method NativeDialog::set_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "title"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "title of the dialog"
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

foreign import ccall "gtk_native_dialog_set_title" gtk_native_dialog_set_title :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    CString ->                              -- title : TBasicType TUTF8
    IO ()

-- | Sets the title of the t'GI.Gtk.Objects.NativeDialog.NativeDialog'.
-- 
-- /Since: 3.20/
nativeDialogSetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> T.Text
    -- ^ /@title@/: title of the dialog
    -> m ()
nativeDialogSetTitle self title = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    title' <- textToCString title
    gtk_native_dialog_set_title self' title'
    touchManagedPtr self
    freeMem title'
    return ()

#if defined(ENABLE_OVERLOADING)
data NativeDialogSetTitleMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogSetTitleMethodInfo a signature where
    overloadedMethod = nativeDialogSetTitle

instance O.OverloadedMethodInfo NativeDialogSetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogSetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogSetTitle"
        })


#endif

-- method NativeDialog::set_transient_for
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "parent window, or %NULL"
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

foreign import ccall "gtk_native_dialog_set_transient_for" gtk_native_dialog_set_transient_for :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Dialog windows should be set transient for the main application
-- window they were spawned from. This allows
-- [window managers][gtk-X11-arch] to e.g. keep the
-- dialog on top of the main window, or center the dialog over the
-- main window.
-- 
-- Passing 'P.Nothing' for /@parent@/ unsets the current transient window.
-- 
-- /Since: 3.20/
nativeDialogSetTransientFor ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a, Gtk.Window.IsWindow b) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> Maybe (b)
    -- ^ /@parent@/: parent window, or 'P.Nothing'
    -> m ()
nativeDialogSetTransientFor self parent = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    gtk_native_dialog_set_transient_for self' maybeParent
    touchManagedPtr self
    whenJust parent touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data NativeDialogSetTransientForMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsNativeDialog a, Gtk.Window.IsWindow b) => O.OverloadedMethod NativeDialogSetTransientForMethodInfo a signature where
    overloadedMethod = nativeDialogSetTransientFor

instance O.OverloadedMethodInfo NativeDialogSetTransientForMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogSetTransientFor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogSetTransientFor"
        })


#endif

-- method NativeDialog::show
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "NativeDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNativeDialog" , sinceVersion = Nothing }
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

foreign import ccall "gtk_native_dialog_show" gtk_native_dialog_show :: 
    Ptr NativeDialog ->                     -- self : TInterface (Name {namespace = "Gtk", name = "NativeDialog"})
    IO ()

-- | Shows the dialog on the display, allowing the user to interact with
-- it. When the user accepts the state of the dialog the dialog will
-- be automatically hidden and the [NativeDialog::response]("GI.Gtk.Objects.NativeDialog#g:signal:response") signal
-- will be emitted.
-- 
-- Multiple calls while the dialog is visible will be ignored.
-- 
-- /Since: 3.20/
nativeDialogShow ::
    (B.CallStack.HasCallStack, MonadIO m, IsNativeDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.NativeDialog.NativeDialog'
    -> m ()
nativeDialogShow self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    gtk_native_dialog_show self'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data NativeDialogShowMethodInfo
instance (signature ~ (m ()), MonadIO m, IsNativeDialog a) => O.OverloadedMethod NativeDialogShowMethodInfo a signature where
    overloadedMethod = nativeDialogShow

instance O.OverloadedMethodInfo NativeDialogShowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.NativeDialog.nativeDialogShow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-NativeDialog.html#v:nativeDialogShow"
        })


#endif


