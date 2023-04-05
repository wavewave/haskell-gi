{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkPageSetup object stores the page size, orientation and margins.
-- The idea is that you can get one of these from the page setup dialog
-- and then pass it to the t'GI.Gtk.Objects.PrintOperation.PrintOperation' when printing.
-- The benefit of splitting this out of the t'GI.Gtk.Objects.PrintSettings.PrintSettings' is that
-- these affect the actual layout of the page, and thus need to be set
-- long before user prints.
-- 
-- ## Margins ## {@/print/@-margins}
-- The margins specified in this object are the “print margins”, i.e. the
-- parts of the page that the printer cannot print on. These are different
-- from the layout margins that a word processor uses; they are typically
-- used to determine the minimal size for the layout
-- margins.
-- 
-- To obtain a t'GI.Gtk.Objects.PageSetup.PageSetup' use 'GI.Gtk.Objects.PageSetup.pageSetupNew' to get the defaults,
-- or use 'GI.Gtk.Functions.printRunPageSetupDialog' to show the page setup dialog
-- and receive the resulting page setup.
-- 
-- == A page setup dialog
-- 
-- 
-- === /C code/
-- >
-- >static GtkPrintSettings *settings = NULL;
-- >static GtkPageSetup *page_setup = NULL;
-- >
-- >static void
-- >do_page_setup (void)
-- >{
-- >  GtkPageSetup *new_page_setup;
-- >
-- >  if (settings == NULL)
-- >    settings = gtk_print_settings_new ();
-- >
-- >  new_page_setup = gtk_print_run_page_setup_dialog (GTK_WINDOW (main_window),
-- >                                                    page_setup, settings);
-- >
-- >  if (page_setup)
-- >    g_object_unref (page_setup);
-- >
-- >  page_setup = new_page_setup;
-- >}
-- 
-- 
-- Printing support was added in GTK+ 2.10.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.PageSetup
    ( 

-- * Exported types
    PageSetup(..)                           ,
    IsPageSetup                             ,
    toPageSetup                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [copy]("GI.Gtk.Objects.PageSetup#g:method:copy"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [loadFile]("GI.Gtk.Objects.PageSetup#g:method:loadFile"), [loadKeyFile]("GI.Gtk.Objects.PageSetup#g:method:loadKeyFile"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toFile]("GI.Gtk.Objects.PageSetup#g:method:toFile"), [toGvariant]("GI.Gtk.Objects.PageSetup#g:method:toGvariant"), [toKeyFile]("GI.Gtk.Objects.PageSetup#g:method:toKeyFile"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBottomMargin]("GI.Gtk.Objects.PageSetup#g:method:getBottomMargin"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getLeftMargin]("GI.Gtk.Objects.PageSetup#g:method:getLeftMargin"), [getOrientation]("GI.Gtk.Objects.PageSetup#g:method:getOrientation"), [getPageHeight]("GI.Gtk.Objects.PageSetup#g:method:getPageHeight"), [getPageWidth]("GI.Gtk.Objects.PageSetup#g:method:getPageWidth"), [getPaperHeight]("GI.Gtk.Objects.PageSetup#g:method:getPaperHeight"), [getPaperSize]("GI.Gtk.Objects.PageSetup#g:method:getPaperSize"), [getPaperWidth]("GI.Gtk.Objects.PageSetup#g:method:getPaperWidth"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRightMargin]("GI.Gtk.Objects.PageSetup#g:method:getRightMargin"), [getTopMargin]("GI.Gtk.Objects.PageSetup#g:method:getTopMargin").
-- 
-- ==== Setters
-- [setBottomMargin]("GI.Gtk.Objects.PageSetup#g:method:setBottomMargin"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setLeftMargin]("GI.Gtk.Objects.PageSetup#g:method:setLeftMargin"), [setOrientation]("GI.Gtk.Objects.PageSetup#g:method:setOrientation"), [setPaperSize]("GI.Gtk.Objects.PageSetup#g:method:setPaperSize"), [setPaperSizeAndDefaultMargins]("GI.Gtk.Objects.PageSetup#g:method:setPaperSizeAndDefaultMargins"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRightMargin]("GI.Gtk.Objects.PageSetup#g:method:setRightMargin"), [setTopMargin]("GI.Gtk.Objects.PageSetup#g:method:setTopMargin").

#if defined(ENABLE_OVERLOADING)
    ResolvePageSetupMethod                  ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    PageSetupCopyMethodInfo                 ,
#endif
    pageSetupCopy                           ,


-- ** getBottomMargin #method:getBottomMargin#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetBottomMarginMethodInfo      ,
#endif
    pageSetupGetBottomMargin                ,


-- ** getLeftMargin #method:getLeftMargin#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetLeftMarginMethodInfo        ,
#endif
    pageSetupGetLeftMargin                  ,


-- ** getOrientation #method:getOrientation#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetOrientationMethodInfo       ,
#endif
    pageSetupGetOrientation                 ,


-- ** getPageHeight #method:getPageHeight#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetPageHeightMethodInfo        ,
#endif
    pageSetupGetPageHeight                  ,


-- ** getPageWidth #method:getPageWidth#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetPageWidthMethodInfo         ,
#endif
    pageSetupGetPageWidth                   ,


-- ** getPaperHeight #method:getPaperHeight#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetPaperHeightMethodInfo       ,
#endif
    pageSetupGetPaperHeight                 ,


-- ** getPaperSize #method:getPaperSize#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetPaperSizeMethodInfo         ,
#endif
    pageSetupGetPaperSize                   ,


-- ** getPaperWidth #method:getPaperWidth#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetPaperWidthMethodInfo        ,
#endif
    pageSetupGetPaperWidth                  ,


-- ** getRightMargin #method:getRightMargin#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetRightMarginMethodInfo       ,
#endif
    pageSetupGetRightMargin                 ,


-- ** getTopMargin #method:getTopMargin#

#if defined(ENABLE_OVERLOADING)
    PageSetupGetTopMarginMethodInfo         ,
#endif
    pageSetupGetTopMargin                   ,


-- ** loadFile #method:loadFile#

#if defined(ENABLE_OVERLOADING)
    PageSetupLoadFileMethodInfo             ,
#endif
    pageSetupLoadFile                       ,


-- ** loadKeyFile #method:loadKeyFile#

#if defined(ENABLE_OVERLOADING)
    PageSetupLoadKeyFileMethodInfo          ,
#endif
    pageSetupLoadKeyFile                    ,


-- ** new #method:new#

    pageSetupNew                            ,


-- ** newFromFile #method:newFromFile#

    pageSetupNewFromFile                    ,


-- ** newFromGvariant #method:newFromGvariant#

    pageSetupNewFromGvariant                ,


-- ** newFromKeyFile #method:newFromKeyFile#

    pageSetupNewFromKeyFile                 ,


-- ** setBottomMargin #method:setBottomMargin#

#if defined(ENABLE_OVERLOADING)
    PageSetupSetBottomMarginMethodInfo      ,
#endif
    pageSetupSetBottomMargin                ,


-- ** setLeftMargin #method:setLeftMargin#

#if defined(ENABLE_OVERLOADING)
    PageSetupSetLeftMarginMethodInfo        ,
#endif
    pageSetupSetLeftMargin                  ,


-- ** setOrientation #method:setOrientation#

#if defined(ENABLE_OVERLOADING)
    PageSetupSetOrientationMethodInfo       ,
#endif
    pageSetupSetOrientation                 ,


-- ** setPaperSize #method:setPaperSize#

#if defined(ENABLE_OVERLOADING)
    PageSetupSetPaperSizeMethodInfo         ,
#endif
    pageSetupSetPaperSize                   ,


-- ** setPaperSizeAndDefaultMargins #method:setPaperSizeAndDefaultMargins#

#if defined(ENABLE_OVERLOADING)
    PageSetupSetPaperSizeAndDefaultMarginsMethodInfo,
#endif
    pageSetupSetPaperSizeAndDefaultMargins  ,


-- ** setRightMargin #method:setRightMargin#

#if defined(ENABLE_OVERLOADING)
    PageSetupSetRightMarginMethodInfo       ,
#endif
    pageSetupSetRightMargin                 ,


-- ** setTopMargin #method:setTopMargin#

#if defined(ENABLE_OVERLOADING)
    PageSetupSetTopMarginMethodInfo         ,
#endif
    pageSetupSetTopMargin                   ,


-- ** toFile #method:toFile#

#if defined(ENABLE_OVERLOADING)
    PageSetupToFileMethodInfo               ,
#endif
    pageSetupToFile                         ,


-- ** toGvariant #method:toGvariant#

#if defined(ENABLE_OVERLOADING)
    PageSetupToGvariantMethodInfo           ,
#endif
    pageSetupToGvariant                     ,


-- ** toKeyFile #method:toKeyFile#

#if defined(ENABLE_OVERLOADING)
    PageSetupToKeyFileMethodInfo            ,
#endif
    pageSetupToKeyFile                      ,




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

import qualified GI.GLib.Structs.KeyFile as GLib.KeyFile
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Structs.PaperSize as Gtk.PaperSize

-- | Memory-managed wrapper type.
newtype PageSetup = PageSetup (SP.ManagedPtr PageSetup)
    deriving (Eq)

instance SP.ManagedPtrNewtype PageSetup where
    toManagedPtr (PageSetup p) = p

foreign import ccall "gtk_page_setup_get_type"
    c_gtk_page_setup_get_type :: IO B.Types.GType

instance B.Types.TypedObject PageSetup where
    glibType = c_gtk_page_setup_get_type

instance B.Types.GObject PageSetup

-- | Type class for types which can be safely cast to `PageSetup`, for instance with `toPageSetup`.
class (SP.GObject o, O.IsDescendantOf PageSetup o) => IsPageSetup o
instance (SP.GObject o, O.IsDescendantOf PageSetup o) => IsPageSetup o

instance O.HasParentTypes PageSetup
type instance O.ParentTypes PageSetup = '[GObject.Object.Object]

-- | Cast to `PageSetup`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPageSetup :: (MIO.MonadIO m, IsPageSetup o) => o -> m PageSetup
toPageSetup = MIO.liftIO . B.ManagedPtr.unsafeCastTo PageSetup

-- | Convert 'PageSetup' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PageSetup) where
    gvalueGType_ = c_gtk_page_setup_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PageSetup)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PageSetup)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PageSetup ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePageSetupMethod (t :: Symbol) (o :: *) :: * where
    ResolvePageSetupMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePageSetupMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePageSetupMethod "copy" o = PageSetupCopyMethodInfo
    ResolvePageSetupMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePageSetupMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePageSetupMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePageSetupMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePageSetupMethod "loadFile" o = PageSetupLoadFileMethodInfo
    ResolvePageSetupMethod "loadKeyFile" o = PageSetupLoadKeyFileMethodInfo
    ResolvePageSetupMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePageSetupMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePageSetupMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePageSetupMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePageSetupMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePageSetupMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePageSetupMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePageSetupMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePageSetupMethod "toFile" o = PageSetupToFileMethodInfo
    ResolvePageSetupMethod "toGvariant" o = PageSetupToGvariantMethodInfo
    ResolvePageSetupMethod "toKeyFile" o = PageSetupToKeyFileMethodInfo
    ResolvePageSetupMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePageSetupMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePageSetupMethod "getBottomMargin" o = PageSetupGetBottomMarginMethodInfo
    ResolvePageSetupMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePageSetupMethod "getLeftMargin" o = PageSetupGetLeftMarginMethodInfo
    ResolvePageSetupMethod "getOrientation" o = PageSetupGetOrientationMethodInfo
    ResolvePageSetupMethod "getPageHeight" o = PageSetupGetPageHeightMethodInfo
    ResolvePageSetupMethod "getPageWidth" o = PageSetupGetPageWidthMethodInfo
    ResolvePageSetupMethod "getPaperHeight" o = PageSetupGetPaperHeightMethodInfo
    ResolvePageSetupMethod "getPaperSize" o = PageSetupGetPaperSizeMethodInfo
    ResolvePageSetupMethod "getPaperWidth" o = PageSetupGetPaperWidthMethodInfo
    ResolvePageSetupMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePageSetupMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePageSetupMethod "getRightMargin" o = PageSetupGetRightMarginMethodInfo
    ResolvePageSetupMethod "getTopMargin" o = PageSetupGetTopMarginMethodInfo
    ResolvePageSetupMethod "setBottomMargin" o = PageSetupSetBottomMarginMethodInfo
    ResolvePageSetupMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePageSetupMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePageSetupMethod "setLeftMargin" o = PageSetupSetLeftMarginMethodInfo
    ResolvePageSetupMethod "setOrientation" o = PageSetupSetOrientationMethodInfo
    ResolvePageSetupMethod "setPaperSize" o = PageSetupSetPaperSizeMethodInfo
    ResolvePageSetupMethod "setPaperSizeAndDefaultMargins" o = PageSetupSetPaperSizeAndDefaultMarginsMethodInfo
    ResolvePageSetupMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePageSetupMethod "setRightMargin" o = PageSetupSetRightMarginMethodInfo
    ResolvePageSetupMethod "setTopMargin" o = PageSetupSetTopMarginMethodInfo
    ResolvePageSetupMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePageSetupMethod t PageSetup, O.OverloadedMethod info PageSetup p) => OL.IsLabel t (PageSetup -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePageSetupMethod t PageSetup, O.OverloadedMethod info PageSetup p, R.HasField t PageSetup p) => R.HasField t PageSetup p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePageSetupMethod t PageSetup, O.OverloadedMethodInfo info PageSetup) => OL.IsLabel t (O.MethodProxy info PageSetup) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PageSetup
type instance O.AttributeList PageSetup = PageSetupAttributeList
type PageSetupAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PageSetup = PageSetupSignalList
type PageSetupSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method PageSetup::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PageSetup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_new" gtk_page_setup_new :: 
    IO (Ptr PageSetup)

-- | Creates a new t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m PageSetup
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.PageSetup.PageSetup'.
pageSetupNew  = liftIO $ do
    result <- gtk_page_setup_new
    checkUnexpectedReturnNULL "pageSetupNew" result
    result' <- (wrapObject PageSetup) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method PageSetup::new_from_file
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "file_name"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the filename to read the page setup from"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PageSetup" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_page_setup_new_from_file" gtk_page_setup_new_from_file :: 
    CString ->                              -- file_name : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr PageSetup)

-- | Reads the page setup from the file /@fileName@/. Returns a
-- new t'GI.Gtk.Objects.PageSetup.PageSetup' object with the restored page setup,
-- or 'P.Nothing' if an error occurred. See 'GI.Gtk.Objects.PageSetup.pageSetupToFile'.
-- 
-- /Since: 2.12/
pageSetupNewFromFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Char]
    -- ^ /@fileName@/: the filename to read the page setup from
    -> m PageSetup
    -- ^ __Returns:__ the restored t'GI.Gtk.Objects.PageSetup.PageSetup' /(Can throw 'Data.GI.Base.GError.GError')/
pageSetupNewFromFile fileName = liftIO $ do
    fileName' <- stringToCString fileName
    onException (do
        result <- propagateGError $ gtk_page_setup_new_from_file fileName'
        checkUnexpectedReturnNULL "pageSetupNewFromFile" result
        result' <- (wrapObject PageSetup) result
        freeMem fileName'
        return result'
     ) (do
        freeMem fileName'
     )

#if defined(ENABLE_OVERLOADING)
#endif

-- method PageSetup::new_from_gvariant
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "variant"
--           , argType = TVariant
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an a{sv} #GVariant" , sinceVersion = Nothing }
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

foreign import ccall "gtk_page_setup_new_from_gvariant" gtk_page_setup_new_from_gvariant :: 
    Ptr GVariant ->                         -- variant : TVariant
    IO (Ptr PageSetup)

-- | Desrialize a page setup from an a{sv} variant in
-- the format produced by 'GI.Gtk.Objects.PageSetup.pageSetupToGvariant'.
-- 
-- /Since: 3.22/
pageSetupNewFromGvariant ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GVariant
    -- ^ /@variant@/: an a{sv} t'GVariant'
    -> m PageSetup
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.PageSetup.PageSetup' object
pageSetupNewFromGvariant variant = liftIO $ do
    variant' <- unsafeManagedPtrGetPtr variant
    result <- gtk_page_setup_new_from_gvariant variant'
    checkUnexpectedReturnNULL "pageSetupNewFromGvariant" result
    result' <- (wrapObject PageSetup) result
    touchManagedPtr variant
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method PageSetup::new_from_key_file
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "key_file"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "KeyFile" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GKeyFile to retrieve the page_setup from"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the name of the group in the key_file to read, or %NULL\n             to use the default name \8220Page Setup\8221"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PageSetup" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_page_setup_new_from_key_file" gtk_page_setup_new_from_key_file :: 
    Ptr GLib.KeyFile.KeyFile ->             -- key_file : TInterface (Name {namespace = "GLib", name = "KeyFile"})
    CString ->                              -- group_name : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr PageSetup)

-- | Reads the page setup from the group /@groupName@/ in the key file
-- /@keyFile@/. Returns a new t'GI.Gtk.Objects.PageSetup.PageSetup' object with the restored
-- page setup, or 'P.Nothing' if an error occurred.
-- 
-- /Since: 2.12/
pageSetupNewFromKeyFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GLib.KeyFile.KeyFile
    -- ^ /@keyFile@/: the t'GI.GLib.Structs.KeyFile.KeyFile' to retrieve the page_setup from
    -> Maybe (T.Text)
    -- ^ /@groupName@/: the name of the group in the key_file to read, or 'P.Nothing'
    --              to use the default name “Page Setup”
    -> m PageSetup
    -- ^ __Returns:__ the restored t'GI.Gtk.Objects.PageSetup.PageSetup' /(Can throw 'Data.GI.Base.GError.GError')/
pageSetupNewFromKeyFile keyFile groupName = liftIO $ do
    keyFile' <- unsafeManagedPtrGetPtr keyFile
    maybeGroupName <- case groupName of
        Nothing -> return nullPtr
        Just jGroupName -> do
            jGroupName' <- textToCString jGroupName
            return jGroupName'
    onException (do
        result <- propagateGError $ gtk_page_setup_new_from_key_file keyFile' maybeGroupName
        checkUnexpectedReturnNULL "pageSetupNewFromKeyFile" result
        result' <- (wrapObject PageSetup) result
        touchManagedPtr keyFile
        freeMem maybeGroupName
        return result'
     ) (do
        freeMem maybeGroupName
     )

#if defined(ENABLE_OVERLOADING)
#endif

-- method PageSetup::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "other"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkPageSetup to copy"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PageSetup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_copy" gtk_page_setup_copy :: 
    Ptr PageSetup ->                        -- other : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    IO (Ptr PageSetup)

-- | Copies a t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupCopy ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@other@/: the t'GI.Gtk.Objects.PageSetup.PageSetup' to copy
    -> m PageSetup
    -- ^ __Returns:__ a copy of /@other@/
pageSetupCopy other = liftIO $ do
    other' <- unsafeManagedPtrCastPtr other
    result <- gtk_page_setup_copy other'
    checkUnexpectedReturnNULL "pageSetupCopy" result
    result' <- (wrapObject PageSetup) result
    touchManagedPtr other
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupCopyMethodInfo
instance (signature ~ (m PageSetup), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupCopyMethodInfo a signature where
    overloadedMethod = pageSetupCopy

instance O.OverloadedMethodInfo PageSetupCopyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupCopy"
        })


#endif

-- method PageSetup::get_bottom_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_bottom_margin" gtk_page_setup_get_bottom_margin :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Gets the bottom margin in units of /@unit@/.
-- 
-- /Since: 2.10/
pageSetupGetBottomMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the bottom margin
pageSetupGetBottomMargin setup unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_page_setup_get_bottom_margin setup' unit'
    let result' = realToFrac result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetBottomMarginMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetBottomMarginMethodInfo a signature where
    overloadedMethod = pageSetupGetBottomMargin

instance O.OverloadedMethodInfo PageSetupGetBottomMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetBottomMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetBottomMargin"
        })


#endif

-- method PageSetup::get_left_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_left_margin" gtk_page_setup_get_left_margin :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Gets the left margin in units of /@unit@/.
-- 
-- /Since: 2.10/
pageSetupGetLeftMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the left margin
pageSetupGetLeftMargin setup unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_page_setup_get_left_margin setup' unit'
    let result' = realToFrac result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetLeftMarginMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetLeftMarginMethodInfo a signature where
    overloadedMethod = pageSetupGetLeftMargin

instance O.OverloadedMethodInfo PageSetupGetLeftMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetLeftMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetLeftMargin"
        })


#endif

-- method PageSetup::get_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "PageOrientation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_orientation" gtk_page_setup_get_orientation :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    IO CUInt

-- | Gets the page orientation of the t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupGetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> m Gtk.Enums.PageOrientation
    -- ^ __Returns:__ the page orientation
pageSetupGetOrientation setup = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    result <- gtk_page_setup_get_orientation setup'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetOrientationMethodInfo
instance (signature ~ (m Gtk.Enums.PageOrientation), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetOrientationMethodInfo a signature where
    overloadedMethod = pageSetupGetOrientation

instance O.OverloadedMethodInfo PageSetupGetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetOrientation"
        })


#endif

-- method PageSetup::get_page_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_page_height" gtk_page_setup_get_page_height :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Returns the page height in units of /@unit@/.
-- 
-- Note that this function takes orientation and
-- margins into consideration.
-- See 'GI.Gtk.Objects.PageSetup.pageSetupGetPaperHeight'.
-- 
-- /Since: 2.10/
pageSetupGetPageHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the page height.
pageSetupGetPageHeight setup unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_page_setup_get_page_height setup' unit'
    let result' = realToFrac result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetPageHeightMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetPageHeightMethodInfo a signature where
    overloadedMethod = pageSetupGetPageHeight

instance O.OverloadedMethodInfo PageSetupGetPageHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetPageHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetPageHeight"
        })


#endif

-- method PageSetup::get_page_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_page_width" gtk_page_setup_get_page_width :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Returns the page width in units of /@unit@/.
-- 
-- Note that this function takes orientation and
-- margins into consideration.
-- See 'GI.Gtk.Objects.PageSetup.pageSetupGetPaperWidth'.
-- 
-- /Since: 2.10/
pageSetupGetPageWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the page width.
pageSetupGetPageWidth setup unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_page_setup_get_page_width setup' unit'
    let result' = realToFrac result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetPageWidthMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetPageWidthMethodInfo a signature where
    overloadedMethod = pageSetupGetPageWidth

instance O.OverloadedMethodInfo PageSetupGetPageWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetPageWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetPageWidth"
        })


#endif

-- method PageSetup::get_paper_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_paper_height" gtk_page_setup_get_paper_height :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Returns the paper height in units of /@unit@/.
-- 
-- Note that this function takes orientation, but
-- not margins into consideration.
-- See 'GI.Gtk.Objects.PageSetup.pageSetupGetPageHeight'.
-- 
-- /Since: 2.10/
pageSetupGetPaperHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the paper height.
pageSetupGetPaperHeight setup unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_page_setup_get_paper_height setup' unit'
    let result' = realToFrac result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetPaperHeightMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetPaperHeightMethodInfo a signature where
    overloadedMethod = pageSetupGetPaperHeight

instance O.OverloadedMethodInfo PageSetupGetPaperHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetPaperHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetPaperHeight"
        })


#endif

-- method PageSetup::get_paper_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PaperSize" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_paper_size" gtk_page_setup_get_paper_size :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    IO (Ptr Gtk.PaperSize.PaperSize)

-- | Gets the paper size of the t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupGetPaperSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> m Gtk.PaperSize.PaperSize
    -- ^ __Returns:__ the paper size
pageSetupGetPaperSize setup = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    result <- gtk_page_setup_get_paper_size setup'
    checkUnexpectedReturnNULL "pageSetupGetPaperSize" result
    result' <- (newBoxed Gtk.PaperSize.PaperSize) result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetPaperSizeMethodInfo
instance (signature ~ (m Gtk.PaperSize.PaperSize), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetPaperSizeMethodInfo a signature where
    overloadedMethod = pageSetupGetPaperSize

instance O.OverloadedMethodInfo PageSetupGetPaperSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetPaperSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetPaperSize"
        })


#endif

-- method PageSetup::get_paper_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_paper_width" gtk_page_setup_get_paper_width :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Returns the paper width in units of /@unit@/.
-- 
-- Note that this function takes orientation, but
-- not margins into consideration.
-- See 'GI.Gtk.Objects.PageSetup.pageSetupGetPageWidth'.
-- 
-- /Since: 2.10/
pageSetupGetPaperWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the paper width.
pageSetupGetPaperWidth setup unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_page_setup_get_paper_width setup' unit'
    let result' = realToFrac result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetPaperWidthMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetPaperWidthMethodInfo a signature where
    overloadedMethod = pageSetupGetPaperWidth

instance O.OverloadedMethodInfo PageSetupGetPaperWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetPaperWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetPaperWidth"
        })


#endif

-- method PageSetup::get_right_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_right_margin" gtk_page_setup_get_right_margin :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Gets the right margin in units of /@unit@/.
-- 
-- /Since: 2.10/
pageSetupGetRightMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the right margin
pageSetupGetRightMargin setup unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_page_setup_get_right_margin setup' unit'
    let result' = realToFrac result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetRightMarginMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetRightMarginMethodInfo a signature where
    overloadedMethod = pageSetupGetRightMargin

instance O.OverloadedMethodInfo PageSetupGetRightMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetRightMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetRightMargin"
        })


#endif

-- method PageSetup::get_top_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the unit for the return value"
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
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_get_top_margin" gtk_page_setup_get_top_margin :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO CDouble

-- | Gets the top margin in units of /@unit@/.
-- 
-- /Since: 2.10/
pageSetupGetTopMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the unit for the return value
    -> m Double
    -- ^ __Returns:__ the top margin
pageSetupGetTopMargin setup unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let unit' = (fromIntegral . fromEnum) unit
    result <- gtk_page_setup_get_top_margin setup' unit'
    let result' = realToFrac result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupGetTopMarginMethodInfo
instance (signature ~ (Gtk.Enums.Unit -> m Double), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupGetTopMarginMethodInfo a signature where
    overloadedMethod = pageSetupGetTopMargin

instance O.OverloadedMethodInfo PageSetupGetTopMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupGetTopMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupGetTopMargin"
        })


#endif

-- method PageSetup::load_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file_name"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the filename to read the page setup from"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_page_setup_load_file" gtk_page_setup_load_file :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CString ->                              -- file_name : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Reads the page setup from the file /@fileName@/.
-- See 'GI.Gtk.Objects.PageSetup.pageSetupToFile'.
-- 
-- /Since: 2.14/
pageSetupLoadFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> [Char]
    -- ^ /@fileName@/: the filename to read the page setup from
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
pageSetupLoadFile setup fileName = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    fileName' <- stringToCString fileName
    onException (do
        _ <- propagateGError $ gtk_page_setup_load_file setup' fileName'
        touchManagedPtr setup
        freeMem fileName'
        return ()
     ) (do
        freeMem fileName'
     )

#if defined(ENABLE_OVERLOADING)
data PageSetupLoadFileMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupLoadFileMethodInfo a signature where
    overloadedMethod = pageSetupLoadFile

instance O.OverloadedMethodInfo PageSetupLoadFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupLoadFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupLoadFile"
        })


#endif

-- method PageSetup::load_key_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key_file"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "KeyFile" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GKeyFile to retrieve the page_setup from"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the name of the group in the key_file to read, or %NULL\n             to use the default name \8220Page Setup\8221"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_page_setup_load_key_file" gtk_page_setup_load_key_file :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    Ptr GLib.KeyFile.KeyFile ->             -- key_file : TInterface (Name {namespace = "GLib", name = "KeyFile"})
    CString ->                              -- group_name : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Reads the page setup from the group /@groupName@/ in the key file
-- /@keyFile@/.
-- 
-- /Since: 2.14/
pageSetupLoadKeyFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> GLib.KeyFile.KeyFile
    -- ^ /@keyFile@/: the t'GI.GLib.Structs.KeyFile.KeyFile' to retrieve the page_setup from
    -> Maybe (T.Text)
    -- ^ /@groupName@/: the name of the group in the key_file to read, or 'P.Nothing'
    --              to use the default name “Page Setup”
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
pageSetupLoadKeyFile setup keyFile groupName = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    keyFile' <- unsafeManagedPtrGetPtr keyFile
    maybeGroupName <- case groupName of
        Nothing -> return nullPtr
        Just jGroupName -> do
            jGroupName' <- textToCString jGroupName
            return jGroupName'
    onException (do
        _ <- propagateGError $ gtk_page_setup_load_key_file setup' keyFile' maybeGroupName
        touchManagedPtr setup
        touchManagedPtr keyFile
        freeMem maybeGroupName
        return ()
     ) (do
        freeMem maybeGroupName
     )

#if defined(ENABLE_OVERLOADING)
data PageSetupLoadKeyFileMethodInfo
instance (signature ~ (GLib.KeyFile.KeyFile -> Maybe (T.Text) -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupLoadKeyFileMethodInfo a signature where
    overloadedMethod = pageSetupLoadKeyFile

instance O.OverloadedMethodInfo PageSetupLoadKeyFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupLoadKeyFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupLoadKeyFile"
        })


#endif

-- method PageSetup::set_bottom_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "margin"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new bottom margin in units of @unit"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the units for @margin"
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

foreign import ccall "gtk_page_setup_set_bottom_margin" gtk_page_setup_set_bottom_margin :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CDouble ->                              -- margin : TBasicType TDouble
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO ()

-- | Sets the bottom margin of the t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupSetBottomMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Double
    -- ^ /@margin@/: the new bottom margin in units of /@unit@/
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the units for /@margin@/
    -> m ()
pageSetupSetBottomMargin setup margin unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let margin' = realToFrac margin
    let unit' = (fromIntegral . fromEnum) unit
    gtk_page_setup_set_bottom_margin setup' margin' unit'
    touchManagedPtr setup
    return ()

#if defined(ENABLE_OVERLOADING)
data PageSetupSetBottomMarginMethodInfo
instance (signature ~ (Double -> Gtk.Enums.Unit -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupSetBottomMarginMethodInfo a signature where
    overloadedMethod = pageSetupSetBottomMargin

instance O.OverloadedMethodInfo PageSetupSetBottomMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupSetBottomMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupSetBottomMargin"
        })


#endif

-- method PageSetup::set_left_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "margin"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new left margin in units of @unit"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the units for @margin"
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

foreign import ccall "gtk_page_setup_set_left_margin" gtk_page_setup_set_left_margin :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CDouble ->                              -- margin : TBasicType TDouble
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO ()

-- | Sets the left margin of the t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupSetLeftMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Double
    -- ^ /@margin@/: the new left margin in units of /@unit@/
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the units for /@margin@/
    -> m ()
pageSetupSetLeftMargin setup margin unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let margin' = realToFrac margin
    let unit' = (fromIntegral . fromEnum) unit
    gtk_page_setup_set_left_margin setup' margin' unit'
    touchManagedPtr setup
    return ()

#if defined(ENABLE_OVERLOADING)
data PageSetupSetLeftMarginMethodInfo
instance (signature ~ (Double -> Gtk.Enums.Unit -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupSetLeftMarginMethodInfo a signature where
    overloadedMethod = pageSetupSetLeftMargin

instance O.OverloadedMethodInfo PageSetupSetLeftMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupSetLeftMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupSetLeftMargin"
        })


#endif

-- method PageSetup::set_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageOrientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageOrientation value"
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

foreign import ccall "gtk_page_setup_set_orientation" gtk_page_setup_set_orientation :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "PageOrientation"})
    IO ()

-- | Sets the page orientation of the t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupSetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.Enums.PageOrientation
    -- ^ /@orientation@/: a t'GI.Gtk.Enums.PageOrientation' value
    -> m ()
pageSetupSetOrientation setup orientation = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let orientation' = (fromIntegral . fromEnum) orientation
    gtk_page_setup_set_orientation setup' orientation'
    touchManagedPtr setup
    return ()

#if defined(ENABLE_OVERLOADING)
data PageSetupSetOrientationMethodInfo
instance (signature ~ (Gtk.Enums.PageOrientation -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupSetOrientationMethodInfo a signature where
    overloadedMethod = pageSetupSetOrientation

instance O.OverloadedMethodInfo PageSetupSetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupSetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupSetOrientation"
        })


#endif

-- method PageSetup::set_paper_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PaperSize" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaperSize" , sinceVersion = Nothing }
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

foreign import ccall "gtk_page_setup_set_paper_size" gtk_page_setup_set_paper_size :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    Ptr Gtk.PaperSize.PaperSize ->          -- size : TInterface (Name {namespace = "Gtk", name = "PaperSize"})
    IO ()

-- | Sets the paper size of the t'GI.Gtk.Objects.PageSetup.PageSetup' without
-- changing the margins. See
-- 'GI.Gtk.Objects.PageSetup.pageSetupSetPaperSizeAndDefaultMargins'.
-- 
-- /Since: 2.10/
pageSetupSetPaperSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.PaperSize.PaperSize
    -- ^ /@size@/: a t'GI.Gtk.Structs.PaperSize.PaperSize'
    -> m ()
pageSetupSetPaperSize setup size = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    size' <- unsafeManagedPtrGetPtr size
    gtk_page_setup_set_paper_size setup' size'
    touchManagedPtr setup
    touchManagedPtr size
    return ()

#if defined(ENABLE_OVERLOADING)
data PageSetupSetPaperSizeMethodInfo
instance (signature ~ (Gtk.PaperSize.PaperSize -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupSetPaperSizeMethodInfo a signature where
    overloadedMethod = pageSetupSetPaperSize

instance O.OverloadedMethodInfo PageSetupSetPaperSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupSetPaperSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupSetPaperSize"
        })


#endif

-- method PageSetup::set_paper_size_and_default_margins
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PaperSize" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaperSize" , sinceVersion = Nothing }
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

foreign import ccall "gtk_page_setup_set_paper_size_and_default_margins" gtk_page_setup_set_paper_size_and_default_margins :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    Ptr Gtk.PaperSize.PaperSize ->          -- size : TInterface (Name {namespace = "Gtk", name = "PaperSize"})
    IO ()

-- | Sets the paper size of the t'GI.Gtk.Objects.PageSetup.PageSetup' and modifies
-- the margins according to the new paper size.
-- 
-- /Since: 2.10/
pageSetupSetPaperSizeAndDefaultMargins ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Gtk.PaperSize.PaperSize
    -- ^ /@size@/: a t'GI.Gtk.Structs.PaperSize.PaperSize'
    -> m ()
pageSetupSetPaperSizeAndDefaultMargins setup size = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    size' <- unsafeManagedPtrGetPtr size
    gtk_page_setup_set_paper_size_and_default_margins setup' size'
    touchManagedPtr setup
    touchManagedPtr size
    return ()

#if defined(ENABLE_OVERLOADING)
data PageSetupSetPaperSizeAndDefaultMarginsMethodInfo
instance (signature ~ (Gtk.PaperSize.PaperSize -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupSetPaperSizeAndDefaultMarginsMethodInfo a signature where
    overloadedMethod = pageSetupSetPaperSizeAndDefaultMargins

instance O.OverloadedMethodInfo PageSetupSetPaperSizeAndDefaultMarginsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupSetPaperSizeAndDefaultMargins",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupSetPaperSizeAndDefaultMargins"
        })


#endif

-- method PageSetup::set_right_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "margin"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new right margin in units of @unit"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the units for @margin"
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

foreign import ccall "gtk_page_setup_set_right_margin" gtk_page_setup_set_right_margin :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CDouble ->                              -- margin : TBasicType TDouble
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO ()

-- | Sets the right margin of the t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupSetRightMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Double
    -- ^ /@margin@/: the new right margin in units of /@unit@/
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the units for /@margin@/
    -> m ()
pageSetupSetRightMargin setup margin unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let margin' = realToFrac margin
    let unit' = (fromIntegral . fromEnum) unit
    gtk_page_setup_set_right_margin setup' margin' unit'
    touchManagedPtr setup
    return ()

#if defined(ENABLE_OVERLOADING)
data PageSetupSetRightMarginMethodInfo
instance (signature ~ (Double -> Gtk.Enums.Unit -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupSetRightMarginMethodInfo a signature where
    overloadedMethod = pageSetupSetRightMargin

instance O.OverloadedMethodInfo PageSetupSetRightMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupSetRightMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupSetRightMargin"
        })


#endif

-- method PageSetup::set_top_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "margin"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new top margin in units of @unit"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "unit"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Unit" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the units for @margin"
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

foreign import ccall "gtk_page_setup_set_top_margin" gtk_page_setup_set_top_margin :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CDouble ->                              -- margin : TBasicType TDouble
    CUInt ->                                -- unit : TInterface (Name {namespace = "Gtk", name = "Unit"})
    IO ()

-- | Sets the top margin of the t'GI.Gtk.Objects.PageSetup.PageSetup'.
-- 
-- /Since: 2.10/
pageSetupSetTopMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> Double
    -- ^ /@margin@/: the new top margin in units of /@unit@/
    -> Gtk.Enums.Unit
    -- ^ /@unit@/: the units for /@margin@/
    -> m ()
pageSetupSetTopMargin setup margin unit = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    let margin' = realToFrac margin
    let unit' = (fromIntegral . fromEnum) unit
    gtk_page_setup_set_top_margin setup' margin' unit'
    touchManagedPtr setup
    return ()

#if defined(ENABLE_OVERLOADING)
data PageSetupSetTopMarginMethodInfo
instance (signature ~ (Double -> Gtk.Enums.Unit -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupSetTopMarginMethodInfo a signature where
    overloadedMethod = pageSetupSetTopMargin

instance O.OverloadedMethodInfo PageSetupSetTopMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupSetTopMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupSetTopMargin"
        })


#endif

-- method PageSetup::to_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file_name"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the file to save to"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_page_setup_to_file" gtk_page_setup_to_file :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    CString ->                              -- file_name : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | This function saves the information from /@setup@/ to /@fileName@/.
-- 
-- /Since: 2.12/
pageSetupToFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> [Char]
    -- ^ /@fileName@/: the file to save to
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
pageSetupToFile setup fileName = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    fileName' <- stringToCString fileName
    onException (do
        _ <- propagateGError $ gtk_page_setup_to_file setup' fileName'
        touchManagedPtr setup
        freeMem fileName'
        return ()
     ) (do
        freeMem fileName'
     )

#if defined(ENABLE_OVERLOADING)
data PageSetupToFileMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupToFileMethodInfo a signature where
    overloadedMethod = pageSetupToFile

instance O.OverloadedMethodInfo PageSetupToFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupToFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupToFile"
        })


#endif

-- method PageSetup::to_gvariant
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just TVariant
-- throws : False
-- Skip return : False

foreign import ccall "gtk_page_setup_to_gvariant" gtk_page_setup_to_gvariant :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    IO (Ptr GVariant)

-- | Serialize page setup to an a{sv} variant.
-- 
-- /Since: 3.22/
pageSetupToGvariant ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> m GVariant
    -- ^ __Returns:__ a new, floating, t'GVariant'
pageSetupToGvariant setup = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    result <- gtk_page_setup_to_gvariant setup'
    checkUnexpectedReturnNULL "pageSetupToGvariant" result
    result' <- B.GVariant.newGVariantFromPtr result
    touchManagedPtr setup
    return result'

#if defined(ENABLE_OVERLOADING)
data PageSetupToGvariantMethodInfo
instance (signature ~ (m GVariant), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupToGvariantMethodInfo a signature where
    overloadedMethod = pageSetupToGvariant

instance O.OverloadedMethodInfo PageSetupToGvariantMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupToGvariant",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupToGvariant"
        })


#endif

-- method PageSetup::to_key_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "setup"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PageSetup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPageSetup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key_file"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "KeyFile" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GKeyFile to save the page setup to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the group to add the settings to in @key_file,\n     or %NULL to use the default name \8220Page Setup\8221"
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

foreign import ccall "gtk_page_setup_to_key_file" gtk_page_setup_to_key_file :: 
    Ptr PageSetup ->                        -- setup : TInterface (Name {namespace = "Gtk", name = "PageSetup"})
    Ptr GLib.KeyFile.KeyFile ->             -- key_file : TInterface (Name {namespace = "GLib", name = "KeyFile"})
    CString ->                              -- group_name : TBasicType TUTF8
    IO ()

-- | This function adds the page setup from /@setup@/ to /@keyFile@/.
-- 
-- /Since: 2.12/
pageSetupToKeyFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsPageSetup a) =>
    a
    -- ^ /@setup@/: a t'GI.Gtk.Objects.PageSetup.PageSetup'
    -> GLib.KeyFile.KeyFile
    -- ^ /@keyFile@/: the t'GI.GLib.Structs.KeyFile.KeyFile' to save the page setup to
    -> Maybe (T.Text)
    -- ^ /@groupName@/: the group to add the settings to in /@keyFile@/,
    --      or 'P.Nothing' to use the default name “Page Setup”
    -> m ()
pageSetupToKeyFile setup keyFile groupName = liftIO $ do
    setup' <- unsafeManagedPtrCastPtr setup
    keyFile' <- unsafeManagedPtrGetPtr keyFile
    maybeGroupName <- case groupName of
        Nothing -> return nullPtr
        Just jGroupName -> do
            jGroupName' <- textToCString jGroupName
            return jGroupName'
    gtk_page_setup_to_key_file setup' keyFile' maybeGroupName
    touchManagedPtr setup
    touchManagedPtr keyFile
    freeMem maybeGroupName
    return ()

#if defined(ENABLE_OVERLOADING)
data PageSetupToKeyFileMethodInfo
instance (signature ~ (GLib.KeyFile.KeyFile -> Maybe (T.Text) -> m ()), MonadIO m, IsPageSetup a) => O.OverloadedMethod PageSetupToKeyFileMethodInfo a signature where
    overloadedMethod = pageSetupToKeyFile

instance O.OverloadedMethodInfo PageSetupToKeyFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PageSetup.pageSetupToKeyFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PageSetup.html#v:pageSetupToKeyFile"
        })


#endif


