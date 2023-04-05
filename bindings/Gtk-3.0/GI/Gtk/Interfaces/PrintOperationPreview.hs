{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.PrintOperationPreview
    ( 

-- * Exported types
    PrintOperationPreview(..)               ,
    IsPrintOperationPreview                 ,
    toPrintOperationPreview                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [endPreview]("GI.Gtk.Interfaces.PrintOperationPreview#g:method:endPreview"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isSelected]("GI.Gtk.Interfaces.PrintOperationPreview#g:method:isSelected"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [renderPage]("GI.Gtk.Interfaces.PrintOperationPreview#g:method:renderPage"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolvePrintOperationPreviewMethod      ,
#endif

-- ** endPreview #method:endPreview#

#if defined(ENABLE_OVERLOADING)
    PrintOperationPreviewEndPreviewMethodInfo,
#endif
    printOperationPreviewEndPreview         ,


-- ** isSelected #method:isSelected#

#if defined(ENABLE_OVERLOADING)
    PrintOperationPreviewIsSelectedMethodInfo,
#endif
    printOperationPreviewIsSelected         ,


-- ** renderPage #method:renderPage#

#if defined(ENABLE_OVERLOADING)
    PrintOperationPreviewRenderPageMethodInfo,
#endif
    printOperationPreviewRenderPage         ,




 -- * Signals


-- ** gotPageSize #signal:gotPageSize#

    PrintOperationPreviewGotPageSizeCallback,
#if defined(ENABLE_OVERLOADING)
    PrintOperationPreviewGotPageSizeSignalInfo,
#endif
    afterPrintOperationPreviewGotPageSize   ,
    onPrintOperationPreviewGotPageSize      ,


-- ** ready #signal:ready#

    PrintOperationPreviewReadyCallback      ,
#if defined(ENABLE_OVERLOADING)
    PrintOperationPreviewReadySignalInfo    ,
#endif
    afterPrintOperationPreviewReady         ,
    onPrintOperationPreviewReady            ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.PageSetup as Gtk.PageSetup
import {-# SOURCE #-} qualified GI.Gtk.Objects.PrintContext as Gtk.PrintContext

-- interface PrintOperationPreview 
-- | Memory-managed wrapper type.
newtype PrintOperationPreview = PrintOperationPreview (SP.ManagedPtr PrintOperationPreview)
    deriving (Eq)

instance SP.ManagedPtrNewtype PrintOperationPreview where
    toManagedPtr (PrintOperationPreview p) = p

foreign import ccall "gtk_print_operation_preview_get_type"
    c_gtk_print_operation_preview_get_type :: IO B.Types.GType

instance B.Types.TypedObject PrintOperationPreview where
    glibType = c_gtk_print_operation_preview_get_type

instance B.Types.GObject PrintOperationPreview

-- | Type class for types which can be safely cast to `PrintOperationPreview`, for instance with `toPrintOperationPreview`.
class (SP.GObject o, O.IsDescendantOf PrintOperationPreview o) => IsPrintOperationPreview o
instance (SP.GObject o, O.IsDescendantOf PrintOperationPreview o) => IsPrintOperationPreview o

instance O.HasParentTypes PrintOperationPreview
type instance O.ParentTypes PrintOperationPreview = '[GObject.Object.Object]

-- | Cast to `PrintOperationPreview`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPrintOperationPreview :: (MIO.MonadIO m, IsPrintOperationPreview o) => o -> m PrintOperationPreview
toPrintOperationPreview = MIO.liftIO . B.ManagedPtr.unsafeCastTo PrintOperationPreview

-- | Convert 'PrintOperationPreview' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PrintOperationPreview) where
    gvalueGType_ = c_gtk_print_operation_preview_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PrintOperationPreview)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PrintOperationPreview)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PrintOperationPreview ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PrintOperationPreview
type instance O.AttributeList PrintOperationPreview = PrintOperationPreviewAttributeList
type PrintOperationPreviewAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolvePrintOperationPreviewMethod (t :: Symbol) (o :: *) :: * where
    ResolvePrintOperationPreviewMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePrintOperationPreviewMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePrintOperationPreviewMethod "endPreview" o = PrintOperationPreviewEndPreviewMethodInfo
    ResolvePrintOperationPreviewMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePrintOperationPreviewMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePrintOperationPreviewMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePrintOperationPreviewMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePrintOperationPreviewMethod "isSelected" o = PrintOperationPreviewIsSelectedMethodInfo
    ResolvePrintOperationPreviewMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePrintOperationPreviewMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePrintOperationPreviewMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePrintOperationPreviewMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePrintOperationPreviewMethod "renderPage" o = PrintOperationPreviewRenderPageMethodInfo
    ResolvePrintOperationPreviewMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePrintOperationPreviewMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePrintOperationPreviewMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePrintOperationPreviewMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePrintOperationPreviewMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePrintOperationPreviewMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePrintOperationPreviewMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePrintOperationPreviewMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePrintOperationPreviewMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePrintOperationPreviewMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePrintOperationPreviewMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePrintOperationPreviewMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePrintOperationPreviewMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePrintOperationPreviewMethod t PrintOperationPreview, O.OverloadedMethod info PrintOperationPreview p) => OL.IsLabel t (PrintOperationPreview -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePrintOperationPreviewMethod t PrintOperationPreview, O.OverloadedMethod info PrintOperationPreview p, R.HasField t PrintOperationPreview p) => R.HasField t PrintOperationPreview p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePrintOperationPreviewMethod t PrintOperationPreview, O.OverloadedMethodInfo info PrintOperationPreview) => OL.IsLabel t (O.MethodProxy info PrintOperationPreview) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method PrintOperationPreview::end_preview
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "preview"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "PrintOperationPreview" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperationPreview"
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

foreign import ccall "gtk_print_operation_preview_end_preview" gtk_print_operation_preview_end_preview :: 
    Ptr PrintOperationPreview ->            -- preview : TInterface (Name {namespace = "Gtk", name = "PrintOperationPreview"})
    IO ()

-- | Ends a preview.
-- 
-- This function must be called to finish a custom print preview.
-- 
-- /Since: 2.10/
printOperationPreviewEndPreview ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperationPreview a) =>
    a
    -- ^ /@preview@/: a t'GI.Gtk.Interfaces.PrintOperationPreview.PrintOperationPreview'
    -> m ()
printOperationPreviewEndPreview preview = liftIO $ do
    preview' <- unsafeManagedPtrCastPtr preview
    gtk_print_operation_preview_end_preview preview'
    touchManagedPtr preview
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationPreviewEndPreviewMethodInfo
instance (signature ~ (m ()), MonadIO m, IsPrintOperationPreview a) => O.OverloadedMethod PrintOperationPreviewEndPreviewMethodInfo a signature where
    overloadedMethod = printOperationPreviewEndPreview

instance O.OverloadedMethodInfo PrintOperationPreviewEndPreviewMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewEndPreview",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-PrintOperationPreview.html#v:printOperationPreviewEndPreview"
        })


#endif

-- method PrintOperationPreview::is_selected
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "preview"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "PrintOperationPreview" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperationPreview"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_nr"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page number" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_operation_preview_is_selected" gtk_print_operation_preview_is_selected :: 
    Ptr PrintOperationPreview ->            -- preview : TInterface (Name {namespace = "Gtk", name = "PrintOperationPreview"})
    Int32 ->                                -- page_nr : TBasicType TInt
    IO CInt

-- | Returns whether the given page is included in the set of pages that
-- have been selected for printing.
-- 
-- /Since: 2.10/
printOperationPreviewIsSelected ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperationPreview a) =>
    a
    -- ^ /@preview@/: a t'GI.Gtk.Interfaces.PrintOperationPreview.PrintOperationPreview'
    -> Int32
    -- ^ /@pageNr@/: a page number
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the page has been selected for printing
printOperationPreviewIsSelected preview pageNr = liftIO $ do
    preview' <- unsafeManagedPtrCastPtr preview
    result <- gtk_print_operation_preview_is_selected preview' pageNr
    let result' = (/= 0) result
    touchManagedPtr preview
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintOperationPreviewIsSelectedMethodInfo
instance (signature ~ (Int32 -> m Bool), MonadIO m, IsPrintOperationPreview a) => O.OverloadedMethod PrintOperationPreviewIsSelectedMethodInfo a signature where
    overloadedMethod = printOperationPreviewIsSelected

instance O.OverloadedMethodInfo PrintOperationPreviewIsSelectedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewIsSelected",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-PrintOperationPreview.html#v:printOperationPreviewIsSelected"
        })


#endif

-- method PrintOperationPreview::render_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "preview"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "PrintOperationPreview" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintOperationPreview"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_nr"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the page to render" , sinceVersion = Nothing }
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

foreign import ccall "gtk_print_operation_preview_render_page" gtk_print_operation_preview_render_page :: 
    Ptr PrintOperationPreview ->            -- preview : TInterface (Name {namespace = "Gtk", name = "PrintOperationPreview"})
    Int32 ->                                -- page_nr : TBasicType TInt
    IO ()

-- | Renders a page to the preview, using the print context that
-- was passed to the [PrintOperation::preview]("GI.Gtk.Objects.PrintOperation#g:signal:preview") handler together
-- with /@preview@/.
-- 
-- A custom iprint preview should use this function in its [expose](#g:signal:expose)
-- handler to render the currently selected page.
-- 
-- Note that this function requires a suitable cairo context to
-- be associated with the print context.
-- 
-- /Since: 2.10/
printOperationPreviewRenderPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintOperationPreview a) =>
    a
    -- ^ /@preview@/: a t'GI.Gtk.Interfaces.PrintOperationPreview.PrintOperationPreview'
    -> Int32
    -- ^ /@pageNr@/: the page to render
    -> m ()
printOperationPreviewRenderPage preview pageNr = liftIO $ do
    preview' <- unsafeManagedPtrCastPtr preview
    gtk_print_operation_preview_render_page preview' pageNr
    touchManagedPtr preview
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintOperationPreviewRenderPageMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsPrintOperationPreview a) => O.OverloadedMethod PrintOperationPreviewRenderPageMethodInfo a signature where
    overloadedMethod = printOperationPreviewRenderPage

instance O.OverloadedMethodInfo PrintOperationPreviewRenderPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.PrintOperationPreview.printOperationPreviewRenderPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-PrintOperationPreview.html#v:printOperationPreviewRenderPage"
        })


#endif

-- signal PrintOperationPreview::got-page-size
-- | The [gotPageSize](#g:signal:gotPageSize) signal is emitted once for each page
-- that gets rendered to the preview.
-- 
-- A handler for this signal should update the /@context@/
-- according to /@pageSetup@/ and set up a suitable cairo
-- context, using 'GI.Gtk.Objects.PrintContext.printContextSetCairoContext'.
type PrintOperationPreviewGotPageSizeCallback =
    Gtk.PrintContext.PrintContext
    -- ^ /@context@/: the current t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> Gtk.PageSetup.PageSetup
    -- ^ /@pageSetup@/: the t'GI.Gtk.Objects.PageSetup.PageSetup' for the current page
    -> IO ()

type C_PrintOperationPreviewGotPageSizeCallback =
    Ptr PrintOperationPreview ->            -- object
    Ptr Gtk.PrintContext.PrintContext ->
    Ptr Gtk.PageSetup.PageSetup ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationPreviewGotPageSizeCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationPreviewGotPageSizeCallback :: C_PrintOperationPreviewGotPageSizeCallback -> IO (FunPtr C_PrintOperationPreviewGotPageSizeCallback)

wrap_PrintOperationPreviewGotPageSizeCallback :: 
    GObject a => (a -> PrintOperationPreviewGotPageSizeCallback) ->
    C_PrintOperationPreviewGotPageSizeCallback
wrap_PrintOperationPreviewGotPageSizeCallback gi'cb gi'selfPtr context pageSetup _ = do
    context' <- (newObject Gtk.PrintContext.PrintContext) context
    pageSetup' <- (newObject Gtk.PageSetup.PageSetup) pageSetup
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  context' pageSetup'


-- | Connect a signal handler for the [gotPageSize](#signal:gotPageSize) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperationPreview #gotPageSize callback
-- @
-- 
-- 
onPrintOperationPreviewGotPageSize :: (IsPrintOperationPreview a, MonadIO m) => a -> ((?self :: a) => PrintOperationPreviewGotPageSizeCallback) -> m SignalHandlerId
onPrintOperationPreviewGotPageSize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationPreviewGotPageSizeCallback wrapped
    wrapped'' <- mk_PrintOperationPreviewGotPageSizeCallback wrapped'
    connectSignalFunPtr obj "got-page-size" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [gotPageSize](#signal:gotPageSize) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperationPreview #gotPageSize callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationPreviewGotPageSize :: (IsPrintOperationPreview a, MonadIO m) => a -> ((?self :: a) => PrintOperationPreviewGotPageSizeCallback) -> m SignalHandlerId
afterPrintOperationPreviewGotPageSize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationPreviewGotPageSizeCallback wrapped
    wrapped'' <- mk_PrintOperationPreviewGotPageSizeCallback wrapped'
    connectSignalFunPtr obj "got-page-size" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationPreviewGotPageSizeSignalInfo
instance SignalInfo PrintOperationPreviewGotPageSizeSignalInfo where
    type HaskellCallbackType PrintOperationPreviewGotPageSizeSignalInfo = PrintOperationPreviewGotPageSizeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationPreviewGotPageSizeCallback cb
        cb'' <- mk_PrintOperationPreviewGotPageSizeCallback cb'
        connectSignalFunPtr obj "got-page-size" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.PrintOperationPreview::got-page-size"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-PrintOperationPreview.html#g:signal:gotPageSize"})

#endif

-- signal PrintOperationPreview::ready
-- | The [ready](#g:signal:ready) signal gets emitted once per preview operation,
-- before the first page is rendered.
-- 
-- A handler for this signal can be used for setup tasks.
type PrintOperationPreviewReadyCallback =
    Gtk.PrintContext.PrintContext
    -- ^ /@context@/: the current t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> IO ()

type C_PrintOperationPreviewReadyCallback =
    Ptr PrintOperationPreview ->            -- object
    Ptr Gtk.PrintContext.PrintContext ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_PrintOperationPreviewReadyCallback`.
foreign import ccall "wrapper"
    mk_PrintOperationPreviewReadyCallback :: C_PrintOperationPreviewReadyCallback -> IO (FunPtr C_PrintOperationPreviewReadyCallback)

wrap_PrintOperationPreviewReadyCallback :: 
    GObject a => (a -> PrintOperationPreviewReadyCallback) ->
    C_PrintOperationPreviewReadyCallback
wrap_PrintOperationPreviewReadyCallback gi'cb gi'selfPtr context _ = do
    context' <- (newObject Gtk.PrintContext.PrintContext) context
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  context'


-- | Connect a signal handler for the [ready](#signal:ready) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' printOperationPreview #ready callback
-- @
-- 
-- 
onPrintOperationPreviewReady :: (IsPrintOperationPreview a, MonadIO m) => a -> ((?self :: a) => PrintOperationPreviewReadyCallback) -> m SignalHandlerId
onPrintOperationPreviewReady obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationPreviewReadyCallback wrapped
    wrapped'' <- mk_PrintOperationPreviewReadyCallback wrapped'
    connectSignalFunPtr obj "ready" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [ready](#signal:ready) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' printOperationPreview #ready callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPrintOperationPreviewReady :: (IsPrintOperationPreview a, MonadIO m) => a -> ((?self :: a) => PrintOperationPreviewReadyCallback) -> m SignalHandlerId
afterPrintOperationPreviewReady obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PrintOperationPreviewReadyCallback wrapped
    wrapped'' <- mk_PrintOperationPreviewReadyCallback wrapped'
    connectSignalFunPtr obj "ready" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PrintOperationPreviewReadySignalInfo
instance SignalInfo PrintOperationPreviewReadySignalInfo where
    type HaskellCallbackType PrintOperationPreviewReadySignalInfo = PrintOperationPreviewReadyCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PrintOperationPreviewReadyCallback cb
        cb'' <- mk_PrintOperationPreviewReadyCallback cb'
        connectSignalFunPtr obj "ready" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.PrintOperationPreview::ready"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-PrintOperationPreview.html#g:signal:ready"})

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PrintOperationPreview = PrintOperationPreviewSignalList
type PrintOperationPreviewSignalList = ('[ '("gotPageSize", PrintOperationPreviewGotPageSizeSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("ready", PrintOperationPreviewReadySignalInfo)] :: [(Symbol, *)])

#endif


