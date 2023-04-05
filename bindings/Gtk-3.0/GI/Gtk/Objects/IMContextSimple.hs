{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkIMContextSimple is a simple input method context supporting table-based
-- input methods. It has a built-in table of compose sequences that is derived
-- from the X11 Compose files.
-- 
-- GtkIMContextSimple reads additional compose sequences from the first of the
-- following files that is found: ~\/.config\/gtk-3.0\/Compose, ~\/.XCompose,
-- \/usr\/share\/X11\/locale\/$locale\/Compose (for locales that have a nontrivial
-- Compose file). The syntax of these files is described in the Compose(5)
-- manual page.
-- 
-- == Unicode characters
-- 
-- GtkIMContextSimple also supports numeric entry of Unicode characters
-- by typing Ctrl-Shift-u, followed by a hexadecimal Unicode codepoint.
-- For example, Ctrl-Shift-u 1 2 3 Enter yields U+0123 LATIN SMALL LETTER
-- G WITH CEDILLA, i.e. ģ.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.IMContextSimple
    ( 

-- * Exported types
    IMContextSimple(..)                     ,
    IsIMContextSimple                       ,
    toIMContextSimple                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addComposeFile]("GI.Gtk.Objects.IMContextSimple#g:method:addComposeFile"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [deleteSurrounding]("GI.Gtk.Objects.IMContext#g:method:deleteSurrounding"), [filterKeypress]("GI.Gtk.Objects.IMContext#g:method:filterKeypress"), [focusIn]("GI.Gtk.Objects.IMContext#g:method:focusIn"), [focusOut]("GI.Gtk.Objects.IMContext#g:method:focusOut"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.IMContext#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getPreeditString]("GI.Gtk.Objects.IMContext#g:method:getPreeditString"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSurrounding]("GI.Gtk.Objects.IMContext#g:method:getSurrounding").
-- 
-- ==== Setters
-- [setClientWindow]("GI.Gtk.Objects.IMContext#g:method:setClientWindow"), [setCursorLocation]("GI.Gtk.Objects.IMContext#g:method:setCursorLocation"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSurrounding]("GI.Gtk.Objects.IMContext#g:method:setSurrounding"), [setUsePreedit]("GI.Gtk.Objects.IMContext#g:method:setUsePreedit").

#if defined(ENABLE_OVERLOADING)
    ResolveIMContextSimpleMethod            ,
#endif

-- ** addComposeFile #method:addComposeFile#

#if defined(ENABLE_OVERLOADING)
    IMContextSimpleAddComposeFileMethodInfo ,
#endif
    iMContextSimpleAddComposeFile           ,


-- ** new #method:new#

    iMContextSimpleNew                      ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.IMContext as Gtk.IMContext

-- | Memory-managed wrapper type.
newtype IMContextSimple = IMContextSimple (SP.ManagedPtr IMContextSimple)
    deriving (Eq)

instance SP.ManagedPtrNewtype IMContextSimple where
    toManagedPtr (IMContextSimple p) = p

foreign import ccall "gtk_im_context_simple_get_type"
    c_gtk_im_context_simple_get_type :: IO B.Types.GType

instance B.Types.TypedObject IMContextSimple where
    glibType = c_gtk_im_context_simple_get_type

instance B.Types.GObject IMContextSimple

-- | Type class for types which can be safely cast to `IMContextSimple`, for instance with `toIMContextSimple`.
class (SP.GObject o, O.IsDescendantOf IMContextSimple o) => IsIMContextSimple o
instance (SP.GObject o, O.IsDescendantOf IMContextSimple o) => IsIMContextSimple o

instance O.HasParentTypes IMContextSimple
type instance O.ParentTypes IMContextSimple = '[Gtk.IMContext.IMContext, GObject.Object.Object]

-- | Cast to `IMContextSimple`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toIMContextSimple :: (MIO.MonadIO m, IsIMContextSimple o) => o -> m IMContextSimple
toIMContextSimple = MIO.liftIO . B.ManagedPtr.unsafeCastTo IMContextSimple

-- | Convert 'IMContextSimple' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe IMContextSimple) where
    gvalueGType_ = c_gtk_im_context_simple_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr IMContextSimple)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr IMContextSimple)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject IMContextSimple ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveIMContextSimpleMethod (t :: Symbol) (o :: *) :: * where
    ResolveIMContextSimpleMethod "addComposeFile" o = IMContextSimpleAddComposeFileMethodInfo
    ResolveIMContextSimpleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveIMContextSimpleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveIMContextSimpleMethod "deleteSurrounding" o = Gtk.IMContext.IMContextDeleteSurroundingMethodInfo
    ResolveIMContextSimpleMethod "filterKeypress" o = Gtk.IMContext.IMContextFilterKeypressMethodInfo
    ResolveIMContextSimpleMethod "focusIn" o = Gtk.IMContext.IMContextFocusInMethodInfo
    ResolveIMContextSimpleMethod "focusOut" o = Gtk.IMContext.IMContextFocusOutMethodInfo
    ResolveIMContextSimpleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveIMContextSimpleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveIMContextSimpleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveIMContextSimpleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveIMContextSimpleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveIMContextSimpleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveIMContextSimpleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveIMContextSimpleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveIMContextSimpleMethod "reset" o = Gtk.IMContext.IMContextResetMethodInfo
    ResolveIMContextSimpleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveIMContextSimpleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveIMContextSimpleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveIMContextSimpleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveIMContextSimpleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveIMContextSimpleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveIMContextSimpleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveIMContextSimpleMethod "getPreeditString" o = Gtk.IMContext.IMContextGetPreeditStringMethodInfo
    ResolveIMContextSimpleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveIMContextSimpleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveIMContextSimpleMethod "getSurrounding" o = Gtk.IMContext.IMContextGetSurroundingMethodInfo
    ResolveIMContextSimpleMethod "setClientWindow" o = Gtk.IMContext.IMContextSetClientWindowMethodInfo
    ResolveIMContextSimpleMethod "setCursorLocation" o = Gtk.IMContext.IMContextSetCursorLocationMethodInfo
    ResolveIMContextSimpleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveIMContextSimpleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveIMContextSimpleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveIMContextSimpleMethod "setSurrounding" o = Gtk.IMContext.IMContextSetSurroundingMethodInfo
    ResolveIMContextSimpleMethod "setUsePreedit" o = Gtk.IMContext.IMContextSetUsePreeditMethodInfo
    ResolveIMContextSimpleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveIMContextSimpleMethod t IMContextSimple, O.OverloadedMethod info IMContextSimple p) => OL.IsLabel t (IMContextSimple -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveIMContextSimpleMethod t IMContextSimple, O.OverloadedMethod info IMContextSimple p, R.HasField t IMContextSimple p) => R.HasField t IMContextSimple p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveIMContextSimpleMethod t IMContextSimple, O.OverloadedMethodInfo info IMContextSimple) => OL.IsLabel t (O.MethodProxy info IMContextSimple) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList IMContextSimple
type instance O.AttributeList IMContextSimple = IMContextSimpleAttributeList
type IMContextSimpleAttributeList = ('[ '("inputHints", Gtk.IMContext.IMContextInputHintsPropertyInfo), '("inputPurpose", Gtk.IMContext.IMContextInputPurposePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList IMContextSimple = IMContextSimpleSignalList
type IMContextSimpleSignalList = ('[ '("commit", Gtk.IMContext.IMContextCommitSignalInfo), '("deleteSurrounding", Gtk.IMContext.IMContextDeleteSurroundingSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("preeditChanged", Gtk.IMContext.IMContextPreeditChangedSignalInfo), '("preeditEnd", Gtk.IMContext.IMContextPreeditEndSignalInfo), '("preeditStart", Gtk.IMContext.IMContextPreeditStartSignalInfo), '("retrieveSurrounding", Gtk.IMContext.IMContextRetrieveSurroundingSignalInfo)] :: [(Symbol, *)])

#endif

-- method IMContextSimple::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "IMContextSimple" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_im_context_simple_new" gtk_im_context_simple_new :: 
    IO (Ptr IMContextSimple)

-- | Creates a new t'GI.Gtk.Objects.IMContextSimple.IMContextSimple'.
iMContextSimpleNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m IMContextSimple
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.IMContextSimple.IMContextSimple'.
iMContextSimpleNew  = liftIO $ do
    result <- gtk_im_context_simple_new
    checkUnexpectedReturnNULL "iMContextSimpleNew" result
    result' <- (wrapObject IMContextSimple) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method IMContextSimple::add_compose_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context_simple"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IMContextSimple" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkIMContextSimple"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "compose_file"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The path of compose file"
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

foreign import ccall "gtk_im_context_simple_add_compose_file" gtk_im_context_simple_add_compose_file :: 
    Ptr IMContextSimple ->                  -- context_simple : TInterface (Name {namespace = "Gtk", name = "IMContextSimple"})
    CString ->                              -- compose_file : TBasicType TUTF8
    IO ()

-- | Adds an additional table from the X11 compose file.
iMContextSimpleAddComposeFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsIMContextSimple a) =>
    a
    -- ^ /@contextSimple@/: A t'GI.Gtk.Objects.IMContextSimple.IMContextSimple'
    -> T.Text
    -- ^ /@composeFile@/: The path of compose file
    -> m ()
iMContextSimpleAddComposeFile contextSimple composeFile = liftIO $ do
    contextSimple' <- unsafeManagedPtrCastPtr contextSimple
    composeFile' <- textToCString composeFile
    gtk_im_context_simple_add_compose_file contextSimple' composeFile'
    touchManagedPtr contextSimple
    freeMem composeFile'
    return ()

#if defined(ENABLE_OVERLOADING)
data IMContextSimpleAddComposeFileMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsIMContextSimple a) => O.OverloadedMethod IMContextSimpleAddComposeFileMethodInfo a signature where
    overloadedMethod = iMContextSimpleAddComposeFile

instance O.OverloadedMethodInfo IMContextSimpleAddComposeFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IMContextSimple.iMContextSimpleAddComposeFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IMContextSimple.html#v:iMContextSimpleAddComposeFile"
        })


#endif


