{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.RcStyle.RcStyle'-struct is used to represent a set
-- of information about the appearance of a widget.
-- This can later be composited together with other
-- t'GI.Gtk.Objects.RcStyle.RcStyle'-structs to form a t'GI.Gtk.Objects.Style.Style'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.RcStyle
    ( 

-- * Exported types
    RcStyle(..)                             ,
    IsRcStyle                               ,
    toRcStyle                               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [copy]("GI.Gtk.Objects.RcStyle#g:method:copy"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveRcStyleMethod                    ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    RcStyleCopyMethodInfo                   ,
#endif
    rcStyleCopy                             ,


-- ** new #method:new#

    rcStyleNew                              ,




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

-- | Memory-managed wrapper type.
newtype RcStyle = RcStyle (SP.ManagedPtr RcStyle)
    deriving (Eq)

instance SP.ManagedPtrNewtype RcStyle where
    toManagedPtr (RcStyle p) = p

foreign import ccall "gtk_rc_style_get_type"
    c_gtk_rc_style_get_type :: IO B.Types.GType

instance B.Types.TypedObject RcStyle where
    glibType = c_gtk_rc_style_get_type

instance B.Types.GObject RcStyle

-- | Type class for types which can be safely cast to `RcStyle`, for instance with `toRcStyle`.
class (SP.GObject o, O.IsDescendantOf RcStyle o) => IsRcStyle o
instance (SP.GObject o, O.IsDescendantOf RcStyle o) => IsRcStyle o

instance O.HasParentTypes RcStyle
type instance O.ParentTypes RcStyle = '[GObject.Object.Object]

-- | Cast to `RcStyle`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toRcStyle :: (MIO.MonadIO m, IsRcStyle o) => o -> m RcStyle
toRcStyle = MIO.liftIO . B.ManagedPtr.unsafeCastTo RcStyle

-- | Convert 'RcStyle' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe RcStyle) where
    gvalueGType_ = c_gtk_rc_style_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr RcStyle)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr RcStyle)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject RcStyle ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveRcStyleMethod (t :: Symbol) (o :: *) :: * where
    ResolveRcStyleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveRcStyleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveRcStyleMethod "copy" o = RcStyleCopyMethodInfo
    ResolveRcStyleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveRcStyleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveRcStyleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveRcStyleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveRcStyleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveRcStyleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveRcStyleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveRcStyleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveRcStyleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveRcStyleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveRcStyleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveRcStyleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveRcStyleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveRcStyleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveRcStyleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveRcStyleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveRcStyleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveRcStyleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveRcStyleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveRcStyleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveRcStyleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRcStyleMethod t RcStyle, O.OverloadedMethod info RcStyle p) => OL.IsLabel t (RcStyle -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRcStyleMethod t RcStyle, O.OverloadedMethod info RcStyle p, R.HasField t RcStyle p) => R.HasField t RcStyle p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRcStyleMethod t RcStyle, O.OverloadedMethodInfo info RcStyle) => OL.IsLabel t (O.MethodProxy info RcStyle) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RcStyle
type instance O.AttributeList RcStyle = RcStyleAttributeList
type RcStyleAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList RcStyle = RcStyleSignalList
type RcStyleSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method RcStyle::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "RcStyle" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_style_new" gtk_rc_style_new :: 
    IO (Ptr RcStyle)

{-# DEPRECATED rcStyleNew ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Creates a new t'GI.Gtk.Objects.RcStyle.RcStyle' with no fields set and
-- a reference count of 1.
rcStyleNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m RcStyle
    -- ^ __Returns:__ the newly-created t'GI.Gtk.Objects.RcStyle.RcStyle'
rcStyleNew  = liftIO $ do
    result <- gtk_rc_style_new
    checkUnexpectedReturnNULL "rcStyleNew" result
    result' <- (wrapObject RcStyle) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RcStyle::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "orig"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RcStyle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the style to copy" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "RcStyle" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_rc_style_copy" gtk_rc_style_copy :: 
    Ptr RcStyle ->                          -- orig : TInterface (Name {namespace = "Gtk", name = "RcStyle"})
    IO (Ptr RcStyle)

{-# DEPRECATED rcStyleCopy ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | Makes a copy of the specified t'GI.Gtk.Objects.RcStyle.RcStyle'. This function
-- will correctly copy an RC style that is a member of a class
-- derived from t'GI.Gtk.Objects.RcStyle.RcStyle'.
rcStyleCopy ::
    (B.CallStack.HasCallStack, MonadIO m, IsRcStyle a) =>
    a
    -- ^ /@orig@/: the style to copy
    -> m RcStyle
    -- ^ __Returns:__ the resulting t'GI.Gtk.Objects.RcStyle.RcStyle'
rcStyleCopy orig = liftIO $ do
    orig' <- unsafeManagedPtrCastPtr orig
    result <- gtk_rc_style_copy orig'
    checkUnexpectedReturnNULL "rcStyleCopy" result
    result' <- (wrapObject RcStyle) result
    touchManagedPtr orig
    return result'

#if defined(ENABLE_OVERLOADING)
data RcStyleCopyMethodInfo
instance (signature ~ (m RcStyle), MonadIO m, IsRcStyle a) => O.OverloadedMethod RcStyleCopyMethodInfo a signature where
    overloadedMethod = rcStyleCopy

instance O.OverloadedMethodInfo RcStyleCopyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RcStyle.rcStyleCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RcStyle.html#v:rcStyleCopy"
        })


#endif


