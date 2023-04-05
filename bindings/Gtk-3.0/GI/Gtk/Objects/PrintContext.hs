{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkPrintContext encapsulates context information that is required when
-- drawing pages for printing, such as the cairo context and important
-- parameters like page size and resolution. It also lets you easily
-- create t'GI.Pango.Objects.Layout.Layout' and t'GI.Pango.Objects.Context.Context' objects that match the font metrics
-- of the cairo surface.
-- 
-- GtkPrintContext objects gets passed to the [PrintOperation::beginPrint]("GI.Gtk.Objects.PrintOperation#g:signal:beginPrint"),
-- [PrintOperation::endPrint]("GI.Gtk.Objects.PrintOperation#g:signal:endPrint"), [PrintOperation::requestPageSetup]("GI.Gtk.Objects.PrintOperation#g:signal:requestPageSetup") and
-- [PrintOperation::drawPage]("GI.Gtk.Objects.PrintOperation#g:signal:drawPage") signals on the t'GI.Gtk.Objects.PrintOperation.PrintOperation'.
-- 
-- ## Using GtkPrintContext in a [PrintOperation::drawPage]("GI.Gtk.Objects.PrintOperation#g:signal:drawPage") callback
-- 
-- 
-- === /C code/
-- >
-- >static void
-- >draw_page (GtkPrintOperation *operation,
-- >	   GtkPrintContext   *context,
-- >	   int                page_nr)
-- >{
-- >  cairo_t *cr;
-- >  PangoLayout *layout;
-- >  PangoFontDescription *desc;
-- >
-- >  cr = gtk_print_context_get_cairo_context (context);
-- >
-- >  // Draw a red rectangle, as wide as the paper (inside the margins)
-- >  cairo_set_source_rgb (cr, 1.0, 0, 0);
-- >  cairo_rectangle (cr, 0, 0, gtk_print_context_get_width (context), 50);
-- >
-- >  cairo_fill (cr);
-- >
-- >  // Draw some lines
-- >  cairo_move_to (cr, 20, 10);
-- >  cairo_line_to (cr, 40, 20);
-- >  cairo_arc (cr, 60, 60, 20, 0, M_PI);
-- >  cairo_line_to (cr, 80, 20);
-- >
-- >  cairo_set_source_rgb (cr, 0, 0, 0);
-- >  cairo_set_line_width (cr, 5);
-- >  cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
-- >  cairo_set_line_join (cr, CAIRO_LINE_JOIN_ROUND);
-- >
-- >  cairo_stroke (cr);
-- >
-- >  // Draw some text
-- >  layout = gtk_print_context_create_pango_layout (context);
-- >  pango_layout_set_text (layout, "Hello World! Printing is easy", -1);
-- >  desc = pango_font_description_from_string ("sans 28");
-- >  pango_layout_set_font_description (layout, desc);
-- >  pango_font_description_free (desc);
-- >
-- >  cairo_move_to (cr, 30, 20);
-- >  pango_cairo_layout_path (cr, layout);
-- >
-- >  // Font Outline
-- >  cairo_set_source_rgb (cr, 0.93, 1.0, 0.47);
-- >  cairo_set_line_width (cr, 0.5);
-- >  cairo_stroke_preserve (cr);
-- >
-- >  // Font Fill
-- >  cairo_set_source_rgb (cr, 0, 0.0, 1.0);
-- >  cairo_fill (cr);
-- >
-- >  g_object_unref (layout);
-- >}
-- 
-- 
-- Printing support was added in GTK+ 2.10.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.PrintContext
    ( 

-- * Exported types
    PrintContext(..)                        ,
    IsPrintContext                          ,
    toPrintContext                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [createPangoContext]("GI.Gtk.Objects.PrintContext#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.PrintContext#g:method:createPangoLayout"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getCairoContext]("GI.Gtk.Objects.PrintContext#g:method:getCairoContext"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDpiX]("GI.Gtk.Objects.PrintContext#g:method:getDpiX"), [getDpiY]("GI.Gtk.Objects.PrintContext#g:method:getDpiY"), [getHardMargins]("GI.Gtk.Objects.PrintContext#g:method:getHardMargins"), [getHeight]("GI.Gtk.Objects.PrintContext#g:method:getHeight"), [getPageSetup]("GI.Gtk.Objects.PrintContext#g:method:getPageSetup"), [getPangoFontmap]("GI.Gtk.Objects.PrintContext#g:method:getPangoFontmap"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getWidth]("GI.Gtk.Objects.PrintContext#g:method:getWidth").
-- 
-- ==== Setters
-- [setCairoContext]("GI.Gtk.Objects.PrintContext#g:method:setCairoContext"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolvePrintContextMethod               ,
#endif

-- ** createPangoContext #method:createPangoContext#

#if defined(ENABLE_OVERLOADING)
    PrintContextCreatePangoContextMethodInfo,
#endif
    printContextCreatePangoContext          ,


-- ** createPangoLayout #method:createPangoLayout#

#if defined(ENABLE_OVERLOADING)
    PrintContextCreatePangoLayoutMethodInfo ,
#endif
    printContextCreatePangoLayout           ,


-- ** getCairoContext #method:getCairoContext#

#if defined(ENABLE_OVERLOADING)
    PrintContextGetCairoContextMethodInfo   ,
#endif
    printContextGetCairoContext             ,


-- ** getDpiX #method:getDpiX#

#if defined(ENABLE_OVERLOADING)
    PrintContextGetDpiXMethodInfo           ,
#endif
    printContextGetDpiX                     ,


-- ** getDpiY #method:getDpiY#

#if defined(ENABLE_OVERLOADING)
    PrintContextGetDpiYMethodInfo           ,
#endif
    printContextGetDpiY                     ,


-- ** getHardMargins #method:getHardMargins#

#if defined(ENABLE_OVERLOADING)
    PrintContextGetHardMarginsMethodInfo    ,
#endif
    printContextGetHardMargins              ,


-- ** getHeight #method:getHeight#

#if defined(ENABLE_OVERLOADING)
    PrintContextGetHeightMethodInfo         ,
#endif
    printContextGetHeight                   ,


-- ** getPageSetup #method:getPageSetup#

#if defined(ENABLE_OVERLOADING)
    PrintContextGetPageSetupMethodInfo      ,
#endif
    printContextGetPageSetup                ,


-- ** getPangoFontmap #method:getPangoFontmap#

#if defined(ENABLE_OVERLOADING)
    PrintContextGetPangoFontmapMethodInfo   ,
#endif
    printContextGetPangoFontmap             ,


-- ** getWidth #method:getWidth#

#if defined(ENABLE_OVERLOADING)
    PrintContextGetWidthMethodInfo          ,
#endif
    printContextGetWidth                    ,


-- ** setCairoContext #method:setCairoContext#

#if defined(ENABLE_OVERLOADING)
    PrintContextSetCairoContextMethodInfo   ,
#endif
    printContextSetCairoContext             ,




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

import qualified GI.Cairo.Structs.Context as Cairo.Context
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.PageSetup as Gtk.PageSetup
import qualified GI.Pango.Objects.Context as Pango.Context
import qualified GI.Pango.Objects.FontMap as Pango.FontMap
import qualified GI.Pango.Objects.Layout as Pango.Layout

-- | Memory-managed wrapper type.
newtype PrintContext = PrintContext (SP.ManagedPtr PrintContext)
    deriving (Eq)

instance SP.ManagedPtrNewtype PrintContext where
    toManagedPtr (PrintContext p) = p

foreign import ccall "gtk_print_context_get_type"
    c_gtk_print_context_get_type :: IO B.Types.GType

instance B.Types.TypedObject PrintContext where
    glibType = c_gtk_print_context_get_type

instance B.Types.GObject PrintContext

-- | Type class for types which can be safely cast to `PrintContext`, for instance with `toPrintContext`.
class (SP.GObject o, O.IsDescendantOf PrintContext o) => IsPrintContext o
instance (SP.GObject o, O.IsDescendantOf PrintContext o) => IsPrintContext o

instance O.HasParentTypes PrintContext
type instance O.ParentTypes PrintContext = '[GObject.Object.Object]

-- | Cast to `PrintContext`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPrintContext :: (MIO.MonadIO m, IsPrintContext o) => o -> m PrintContext
toPrintContext = MIO.liftIO . B.ManagedPtr.unsafeCastTo PrintContext

-- | Convert 'PrintContext' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PrintContext) where
    gvalueGType_ = c_gtk_print_context_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PrintContext)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PrintContext)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PrintContext ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePrintContextMethod (t :: Symbol) (o :: *) :: * where
    ResolvePrintContextMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePrintContextMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePrintContextMethod "createPangoContext" o = PrintContextCreatePangoContextMethodInfo
    ResolvePrintContextMethod "createPangoLayout" o = PrintContextCreatePangoLayoutMethodInfo
    ResolvePrintContextMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePrintContextMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePrintContextMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePrintContextMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePrintContextMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePrintContextMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePrintContextMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePrintContextMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePrintContextMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePrintContextMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePrintContextMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePrintContextMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePrintContextMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePrintContextMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePrintContextMethod "getCairoContext" o = PrintContextGetCairoContextMethodInfo
    ResolvePrintContextMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePrintContextMethod "getDpiX" o = PrintContextGetDpiXMethodInfo
    ResolvePrintContextMethod "getDpiY" o = PrintContextGetDpiYMethodInfo
    ResolvePrintContextMethod "getHardMargins" o = PrintContextGetHardMarginsMethodInfo
    ResolvePrintContextMethod "getHeight" o = PrintContextGetHeightMethodInfo
    ResolvePrintContextMethod "getPageSetup" o = PrintContextGetPageSetupMethodInfo
    ResolvePrintContextMethod "getPangoFontmap" o = PrintContextGetPangoFontmapMethodInfo
    ResolvePrintContextMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePrintContextMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePrintContextMethod "getWidth" o = PrintContextGetWidthMethodInfo
    ResolvePrintContextMethod "setCairoContext" o = PrintContextSetCairoContextMethodInfo
    ResolvePrintContextMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePrintContextMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePrintContextMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePrintContextMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePrintContextMethod t PrintContext, O.OverloadedMethod info PrintContext p) => OL.IsLabel t (PrintContext -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePrintContextMethod t PrintContext, O.OverloadedMethod info PrintContext p, R.HasField t PrintContext p) => R.HasField t PrintContext p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePrintContextMethod t PrintContext, O.OverloadedMethodInfo info PrintContext) => OL.IsLabel t (O.MethodProxy info PrintContext) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PrintContext
type instance O.AttributeList PrintContext = PrintContextAttributeList
type PrintContextAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PrintContext = PrintContextSignalList
type PrintContextSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method PrintContext::create_pango_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "Context" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_create_pango_context" gtk_print_context_create_pango_context :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO (Ptr Pango.Context.Context)

-- | Creates a new t'GI.Pango.Objects.Context.Context' that can be used with the
-- t'GI.Gtk.Objects.PrintContext.PrintContext'.
-- 
-- /Since: 2.10/
printContextCreatePangoContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Pango.Context.Context
    -- ^ __Returns:__ a new Pango context for /@context@/
printContextCreatePangoContext context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_create_pango_context context'
    checkUnexpectedReturnNULL "printContextCreatePangoContext" result
    result' <- (wrapObject Pango.Context.Context) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextCreatePangoContextMethodInfo
instance (signature ~ (m Pango.Context.Context), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextCreatePangoContextMethodInfo a signature where
    overloadedMethod = printContextCreatePangoContext

instance O.OverloadedMethodInfo PrintContextCreatePangoContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextCreatePangoContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextCreatePangoContext"
        })


#endif

-- method PrintContext::create_pango_layout
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "Layout" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_create_pango_layout" gtk_print_context_create_pango_layout :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO (Ptr Pango.Layout.Layout)

-- | Creates a new t'GI.Pango.Objects.Layout.Layout' that is suitable for use
-- with the t'GI.Gtk.Objects.PrintContext.PrintContext'.
-- 
-- /Since: 2.10/
printContextCreatePangoLayout ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Pango.Layout.Layout
    -- ^ __Returns:__ a new Pango layout for /@context@/
printContextCreatePangoLayout context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_create_pango_layout context'
    checkUnexpectedReturnNULL "printContextCreatePangoLayout" result
    result' <- (wrapObject Pango.Layout.Layout) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextCreatePangoLayoutMethodInfo
instance (signature ~ (m Pango.Layout.Layout), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextCreatePangoLayoutMethodInfo a signature where
    overloadedMethod = printContextCreatePangoLayout

instance O.OverloadedMethodInfo PrintContextCreatePangoLayoutMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextCreatePangoLayout",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextCreatePangoLayout"
        })


#endif

-- method PrintContext::get_cairo_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "cairo" , name = "Context" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_get_cairo_context" gtk_print_context_get_cairo_context :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO (Ptr Cairo.Context.Context)

-- | Obtains the cairo context that is associated with the
-- t'GI.Gtk.Objects.PrintContext.PrintContext'.
-- 
-- /Since: 2.10/
printContextGetCairoContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Cairo.Context.Context
    -- ^ __Returns:__ the cairo context of /@context@/
printContextGetCairoContext context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_get_cairo_context context'
    checkUnexpectedReturnNULL "printContextGetCairoContext" result
    result' <- (newBoxed Cairo.Context.Context) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextGetCairoContextMethodInfo
instance (signature ~ (m Cairo.Context.Context), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextGetCairoContextMethodInfo a signature where
    overloadedMethod = printContextGetCairoContext

instance O.OverloadedMethodInfo PrintContextGetCairoContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextGetCairoContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextGetCairoContext"
        })


#endif

-- method PrintContext::get_dpi_x
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_get_dpi_x" gtk_print_context_get_dpi_x :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO CDouble

-- | Obtains the horizontal resolution of the t'GI.Gtk.Objects.PrintContext.PrintContext',
-- in dots per inch.
-- 
-- /Since: 2.10/
printContextGetDpiX ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Double
    -- ^ __Returns:__ the horizontal resolution of /@context@/
printContextGetDpiX context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_get_dpi_x context'
    let result' = realToFrac result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextGetDpiXMethodInfo
instance (signature ~ (m Double), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextGetDpiXMethodInfo a signature where
    overloadedMethod = printContextGetDpiX

instance O.OverloadedMethodInfo PrintContextGetDpiXMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextGetDpiX",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextGetDpiX"
        })


#endif

-- method PrintContext::get_dpi_y
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_get_dpi_y" gtk_print_context_get_dpi_y :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO CDouble

-- | Obtains the vertical resolution of the t'GI.Gtk.Objects.PrintContext.PrintContext',
-- in dots per inch.
-- 
-- /Since: 2.10/
printContextGetDpiY ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Double
    -- ^ __Returns:__ the vertical resolution of /@context@/
printContextGetDpiY context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_get_dpi_y context'
    let result' = realToFrac result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextGetDpiYMethodInfo
instance (signature ~ (m Double), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextGetDpiYMethodInfo a signature where
    overloadedMethod = printContextGetDpiY

instance O.OverloadedMethodInfo PrintContextGetDpiYMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextGetDpiY",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextGetDpiY"
        })


#endif

-- method PrintContext::get_hard_margins
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "top"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "top hardware printer margin"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "bottom"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "bottom hardware printer margin"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "left"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "left hardware printer margin"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "right"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "right hardware printer margin"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_get_hard_margins" gtk_print_context_get_hard_margins :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    Ptr CDouble ->                          -- top : TBasicType TDouble
    Ptr CDouble ->                          -- bottom : TBasicType TDouble
    Ptr CDouble ->                          -- left : TBasicType TDouble
    Ptr CDouble ->                          -- right : TBasicType TDouble
    IO CInt

-- | Obtains the hardware printer margins of the t'GI.Gtk.Objects.PrintContext.PrintContext', in units.
-- 
-- /Since: 2.20/
printContextGetHardMargins ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m ((Bool, Double, Double, Double, Double))
    -- ^ __Returns:__ 'P.True' if the hard margins were retrieved
printContextGetHardMargins context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    top <- allocMem :: IO (Ptr CDouble)
    bottom <- allocMem :: IO (Ptr CDouble)
    left <- allocMem :: IO (Ptr CDouble)
    right <- allocMem :: IO (Ptr CDouble)
    result <- gtk_print_context_get_hard_margins context' top bottom left right
    let result' = (/= 0) result
    top' <- peek top
    let top'' = realToFrac top'
    bottom' <- peek bottom
    let bottom'' = realToFrac bottom'
    left' <- peek left
    let left'' = realToFrac left'
    right' <- peek right
    let right'' = realToFrac right'
    touchManagedPtr context
    freeMem top
    freeMem bottom
    freeMem left
    freeMem right
    return (result', top'', bottom'', left'', right'')

#if defined(ENABLE_OVERLOADING)
data PrintContextGetHardMarginsMethodInfo
instance (signature ~ (m ((Bool, Double, Double, Double, Double))), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextGetHardMarginsMethodInfo a signature where
    overloadedMethod = printContextGetHardMargins

instance O.OverloadedMethodInfo PrintContextGetHardMarginsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextGetHardMargins",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextGetHardMargins"
        })


#endif

-- method PrintContext::get_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_get_height" gtk_print_context_get_height :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO CDouble

-- | Obtains the height of the t'GI.Gtk.Objects.PrintContext.PrintContext', in pixels.
-- 
-- /Since: 2.10/
printContextGetHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Double
    -- ^ __Returns:__ the height of /@context@/
printContextGetHeight context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_get_height context'
    let result' = realToFrac result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextGetHeightMethodInfo
instance (signature ~ (m Double), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextGetHeightMethodInfo a signature where
    overloadedMethod = printContextGetHeight

instance O.OverloadedMethodInfo PrintContextGetHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextGetHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextGetHeight"
        })


#endif

-- method PrintContext::get_page_setup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PageSetup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_get_page_setup" gtk_print_context_get_page_setup :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO (Ptr Gtk.PageSetup.PageSetup)

-- | Obtains the t'GI.Gtk.Objects.PageSetup.PageSetup' that determines the page
-- dimensions of the t'GI.Gtk.Objects.PrintContext.PrintContext'.
-- 
-- /Since: 2.10/
printContextGetPageSetup ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Gtk.PageSetup.PageSetup
    -- ^ __Returns:__ the page setup of /@context@/
printContextGetPageSetup context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_get_page_setup context'
    checkUnexpectedReturnNULL "printContextGetPageSetup" result
    result' <- (newObject Gtk.PageSetup.PageSetup) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextGetPageSetupMethodInfo
instance (signature ~ (m Gtk.PageSetup.PageSetup), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextGetPageSetupMethodInfo a signature where
    overloadedMethod = printContextGetPageSetup

instance O.OverloadedMethodInfo PrintContextGetPageSetupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextGetPageSetup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextGetPageSetup"
        })


#endif

-- method PrintContext::get_pango_fontmap
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "FontMap" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_get_pango_fontmap" gtk_print_context_get_pango_fontmap :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO (Ptr Pango.FontMap.FontMap)

-- | Returns a t'GI.Pango.Objects.FontMap.FontMap' that is suitable for use
-- with the t'GI.Gtk.Objects.PrintContext.PrintContext'.
-- 
-- /Since: 2.10/
printContextGetPangoFontmap ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Pango.FontMap.FontMap
    -- ^ __Returns:__ the font map of /@context@/
printContextGetPangoFontmap context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_get_pango_fontmap context'
    checkUnexpectedReturnNULL "printContextGetPangoFontmap" result
    result' <- (newObject Pango.FontMap.FontMap) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextGetPangoFontmapMethodInfo
instance (signature ~ (m Pango.FontMap.FontMap), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextGetPangoFontmapMethodInfo a signature where
    overloadedMethod = printContextGetPangoFontmap

instance O.OverloadedMethodInfo PrintContextGetPangoFontmapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextGetPangoFontmap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextGetPangoFontmap"
        })


#endif

-- method PrintContext::get_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_print_context_get_width" gtk_print_context_get_width :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    IO CDouble

-- | Obtains the width of the t'GI.Gtk.Objects.PrintContext.PrintContext', in pixels.
-- 
-- /Since: 2.10/
printContextGetWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> m Double
    -- ^ __Returns:__ the width of /@context@/
printContextGetWidth context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_print_context_get_width context'
    let result' = realToFrac result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data PrintContextGetWidthMethodInfo
instance (signature ~ (m Double), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextGetWidthMethodInfo a signature where
    overloadedMethod = printContextGetWidth

instance O.OverloadedMethodInfo PrintContextGetWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextGetWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextGetWidth"
        })


#endif

-- method PrintContext::set_cairo_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PrintContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPrintContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the cairo context" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dpi_x"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the horizontal resolution to use with @cr"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dpi_y"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the vertical resolution to use with @cr"
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

foreign import ccall "gtk_print_context_set_cairo_context" gtk_print_context_set_cairo_context :: 
    Ptr PrintContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "PrintContext"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    CDouble ->                              -- dpi_x : TBasicType TDouble
    CDouble ->                              -- dpi_y : TBasicType TDouble
    IO ()

-- | Sets a new cairo context on a print context.
-- 
-- This function is intended to be used when implementing
-- an internal print preview, it is not needed for printing,
-- since GTK+ itself creates a suitable cairo context in that
-- case.
-- 
-- /Since: 2.10/
printContextSetCairoContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsPrintContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.PrintContext.PrintContext'
    -> Cairo.Context.Context
    -- ^ /@cr@/: the cairo context
    -> Double
    -- ^ /@dpiX@/: the horizontal resolution to use with /@cr@/
    -> Double
    -- ^ /@dpiY@/: the vertical resolution to use with /@cr@/
    -> m ()
printContextSetCairoContext context cr dpiX dpiY = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    cr' <- unsafeManagedPtrGetPtr cr
    let dpiX' = realToFrac dpiX
    let dpiY' = realToFrac dpiY
    gtk_print_context_set_cairo_context context' cr' dpiX' dpiY'
    touchManagedPtr context
    touchManagedPtr cr
    return ()

#if defined(ENABLE_OVERLOADING)
data PrintContextSetCairoContextMethodInfo
instance (signature ~ (Cairo.Context.Context -> Double -> Double -> m ()), MonadIO m, IsPrintContext a) => O.OverloadedMethod PrintContextSetCairoContextMethodInfo a signature where
    overloadedMethod = printContextSetCairoContext

instance O.OverloadedMethodInfo PrintContextSetCairoContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PrintContext.printContextSetCairoContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PrintContext.html#v:printContextSetCairoContext"
        })


#endif


