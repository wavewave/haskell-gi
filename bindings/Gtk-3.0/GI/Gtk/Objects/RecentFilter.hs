{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.RecentFilter.RecentFilter' can be used to restrict the files being shown
-- in a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'.  Files can be filtered based on their name
-- (with 'GI.Gtk.Objects.RecentFilter.recentFilterAddPattern'), on their mime type (with
-- 'GI.Gtk.Objects.FileFilter.fileFilterAddMimeType'), on the application that has
-- registered them (with 'GI.Gtk.Objects.RecentFilter.recentFilterAddApplication'), or by
-- a custom filter function (with 'GI.Gtk.Objects.RecentFilter.recentFilterAddCustom').
-- 
-- Filtering by mime type handles aliasing and subclassing of mime
-- types; e.g. a filter for text\/plain also matches a file with mime
-- type application\/rtf, since application\/rtf is a subclass of text\/plain.
-- Note that t'GI.Gtk.Objects.RecentFilter.RecentFilter' allows wildcards for the subtype of a
-- mime type, so you can e.g. filter for image\/\\*.
-- 
-- Normally, filters are used by adding them to a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser',
-- see 'GI.Gtk.Interfaces.RecentChooser.recentChooserAddFilter', but it is also possible to
-- manually use a filter on a file with 'GI.Gtk.Objects.RecentFilter.recentFilterFilter'.
-- 
-- Recently used files are supported since GTK+ 2.10.
-- 
-- == GtkRecentFilter as GtkBuildable
-- 
-- The GtkRecentFilter implementation of the GtkBuildable interface
-- supports adding rules using the @\<mime-types>@, @\<patterns>@ and
-- @\<applications>@ elements and listing the rules within. Specifying
-- a @\<mime-type>@, @\<pattern>@ or @\<application>@ has the same effect as
-- calling 'GI.Gtk.Objects.RecentFilter.recentFilterAddMimeType',
-- 'GI.Gtk.Objects.RecentFilter.recentFilterAddPattern' or 'GI.Gtk.Objects.RecentFilter.recentFilterAddApplication'.
-- 
-- An example of a UI definition fragment specifying @GtkRecentFilter@
-- rules:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkRecentFilter">
-- >  <mime-types>
-- >    <mime-type>text/plain</mime-type>
-- >    <mime-type>image/png</mime-type>
-- >  </mime-types>
-- >  <patterns>
-- >    <pattern>*.txt</pattern>
-- >    <pattern>*.png</pattern>
-- >  </patterns>
-- >  <applications>
-- >    <application>gimp</application>
-- >    <application>gedit</application>
-- >    <application>glade</application>
-- >  </applications>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.RecentFilter
    ( 

-- * Exported types
    RecentFilter(..)                        ,
    IsRecentFilter                          ,
    toRecentFilter                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addAge]("GI.Gtk.Objects.RecentFilter#g:method:addAge"), [addApplication]("GI.Gtk.Objects.RecentFilter#g:method:addApplication"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addCustom]("GI.Gtk.Objects.RecentFilter#g:method:addCustom"), [addGroup]("GI.Gtk.Objects.RecentFilter#g:method:addGroup"), [addMimeType]("GI.Gtk.Objects.RecentFilter#g:method:addMimeType"), [addPattern]("GI.Gtk.Objects.RecentFilter#g:method:addPattern"), [addPixbufFormats]("GI.Gtk.Objects.RecentFilter#g:method:addPixbufFormats"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [filter]("GI.Gtk.Objects.RecentFilter#g:method:filter"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getName]("GI.Gtk.Objects.RecentFilter#g:method:getName"), [getNeeded]("GI.Gtk.Objects.RecentFilter#g:method:getNeeded"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setName]("GI.Gtk.Objects.RecentFilter#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveRecentFilterMethod               ,
#endif

-- ** addAge #method:addAge#

#if defined(ENABLE_OVERLOADING)
    RecentFilterAddAgeMethodInfo            ,
#endif
    recentFilterAddAge                      ,


-- ** addApplication #method:addApplication#

#if defined(ENABLE_OVERLOADING)
    RecentFilterAddApplicationMethodInfo    ,
#endif
    recentFilterAddApplication              ,


-- ** addCustom #method:addCustom#

#if defined(ENABLE_OVERLOADING)
    RecentFilterAddCustomMethodInfo         ,
#endif
    recentFilterAddCustom                   ,


-- ** addGroup #method:addGroup#

#if defined(ENABLE_OVERLOADING)
    RecentFilterAddGroupMethodInfo          ,
#endif
    recentFilterAddGroup                    ,


-- ** addMimeType #method:addMimeType#

#if defined(ENABLE_OVERLOADING)
    RecentFilterAddMimeTypeMethodInfo       ,
#endif
    recentFilterAddMimeType                 ,


-- ** addPattern #method:addPattern#

#if defined(ENABLE_OVERLOADING)
    RecentFilterAddPatternMethodInfo        ,
#endif
    recentFilterAddPattern                  ,


-- ** addPixbufFormats #method:addPixbufFormats#

#if defined(ENABLE_OVERLOADING)
    RecentFilterAddPixbufFormatsMethodInfo  ,
#endif
    recentFilterAddPixbufFormats            ,


-- ** filter #method:filter#

#if defined(ENABLE_OVERLOADING)
    RecentFilterFilterMethodInfo            ,
#endif
    recentFilterFilter                      ,


-- ** getName #method:getName#

#if defined(ENABLE_OVERLOADING)
    RecentFilterGetNameMethodInfo           ,
#endif
    recentFilterGetName                     ,


-- ** getNeeded #method:getNeeded#

#if defined(ENABLE_OVERLOADING)
    RecentFilterGetNeededMethodInfo         ,
#endif
    recentFilterGetNeeded                   ,


-- ** new #method:new#

    recentFilterNew                         ,


-- ** setName #method:setName#

#if defined(ENABLE_OVERLOADING)
    RecentFilterSetNameMethodInfo           ,
#endif
    recentFilterSetName                     ,




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

import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Structs.RecentFilterInfo as Gtk.RecentFilterInfo

-- | Memory-managed wrapper type.
newtype RecentFilter = RecentFilter (SP.ManagedPtr RecentFilter)
    deriving (Eq)

instance SP.ManagedPtrNewtype RecentFilter where
    toManagedPtr (RecentFilter p) = p

foreign import ccall "gtk_recent_filter_get_type"
    c_gtk_recent_filter_get_type :: IO B.Types.GType

instance B.Types.TypedObject RecentFilter where
    glibType = c_gtk_recent_filter_get_type

instance B.Types.GObject RecentFilter

-- | Type class for types which can be safely cast to `RecentFilter`, for instance with `toRecentFilter`.
class (SP.GObject o, O.IsDescendantOf RecentFilter o) => IsRecentFilter o
instance (SP.GObject o, O.IsDescendantOf RecentFilter o) => IsRecentFilter o

instance O.HasParentTypes RecentFilter
type instance O.ParentTypes RecentFilter = '[GObject.Object.Object, Gtk.Buildable.Buildable]

-- | Cast to `RecentFilter`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toRecentFilter :: (MIO.MonadIO m, IsRecentFilter o) => o -> m RecentFilter
toRecentFilter = MIO.liftIO . B.ManagedPtr.unsafeCastTo RecentFilter

-- | Convert 'RecentFilter' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe RecentFilter) where
    gvalueGType_ = c_gtk_recent_filter_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr RecentFilter)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr RecentFilter)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject RecentFilter ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveRecentFilterMethod (t :: Symbol) (o :: *) :: * where
    ResolveRecentFilterMethod "addAge" o = RecentFilterAddAgeMethodInfo
    ResolveRecentFilterMethod "addApplication" o = RecentFilterAddApplicationMethodInfo
    ResolveRecentFilterMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveRecentFilterMethod "addCustom" o = RecentFilterAddCustomMethodInfo
    ResolveRecentFilterMethod "addGroup" o = RecentFilterAddGroupMethodInfo
    ResolveRecentFilterMethod "addMimeType" o = RecentFilterAddMimeTypeMethodInfo
    ResolveRecentFilterMethod "addPattern" o = RecentFilterAddPatternMethodInfo
    ResolveRecentFilterMethod "addPixbufFormats" o = RecentFilterAddPixbufFormatsMethodInfo
    ResolveRecentFilterMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveRecentFilterMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveRecentFilterMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveRecentFilterMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveRecentFilterMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveRecentFilterMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveRecentFilterMethod "filter" o = RecentFilterFilterMethodInfo
    ResolveRecentFilterMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveRecentFilterMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveRecentFilterMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveRecentFilterMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveRecentFilterMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveRecentFilterMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveRecentFilterMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveRecentFilterMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveRecentFilterMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveRecentFilterMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveRecentFilterMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveRecentFilterMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveRecentFilterMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveRecentFilterMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveRecentFilterMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveRecentFilterMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveRecentFilterMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveRecentFilterMethod "getName" o = RecentFilterGetNameMethodInfo
    ResolveRecentFilterMethod "getNeeded" o = RecentFilterGetNeededMethodInfo
    ResolveRecentFilterMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveRecentFilterMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveRecentFilterMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveRecentFilterMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveRecentFilterMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveRecentFilterMethod "setName" o = RecentFilterSetNameMethodInfo
    ResolveRecentFilterMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveRecentFilterMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRecentFilterMethod t RecentFilter, O.OverloadedMethod info RecentFilter p) => OL.IsLabel t (RecentFilter -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRecentFilterMethod t RecentFilter, O.OverloadedMethod info RecentFilter p, R.HasField t RecentFilter p) => R.HasField t RecentFilter p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRecentFilterMethod t RecentFilter, O.OverloadedMethodInfo info RecentFilter) => OL.IsLabel t (O.MethodProxy info RecentFilter) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RecentFilter
type instance O.AttributeList RecentFilter = RecentFilterAttributeList
type RecentFilterAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList RecentFilter = RecentFilterSignalList
type RecentFilterSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method RecentFilter::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "RecentFilter" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_filter_new" gtk_recent_filter_new :: 
    IO (Ptr RecentFilter)

-- | Creates a new t'GI.Gtk.Objects.RecentFilter.RecentFilter' with no rules added to it.
-- Such filter does not accept any recently used resources, so is not
-- particularly useful until you add rules with
-- 'GI.Gtk.Objects.RecentFilter.recentFilterAddPattern', 'GI.Gtk.Objects.RecentFilter.recentFilterAddMimeType',
-- 'GI.Gtk.Objects.RecentFilter.recentFilterAddApplication', 'GI.Gtk.Objects.RecentFilter.recentFilterAddAge'.
-- To create a filter that accepts any recently used resource, use:
-- 
-- === /C code/
-- >
-- >GtkRecentFilter *filter = gtk_recent_filter_new ();
-- >gtk_recent_filter_add_pattern (filter, "*");
-- 
-- 
-- /Since: 2.10/
recentFilterNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m RecentFilter
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.RecentFilter.RecentFilter'
recentFilterNew  = liftIO $ do
    result <- gtk_recent_filter_new
    checkUnexpectedReturnNULL "recentFilterNew" result
    result' <- (newObject RecentFilter) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RecentFilter::add_age
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "days"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of days" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_filter_add_age" gtk_recent_filter_add_age :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    Int32 ->                                -- days : TBasicType TInt
    IO ()

-- | Adds a rule that allows resources based on their age - that is, the number
-- of days elapsed since they were last modified.
-- 
-- /Since: 2.10/
recentFilterAddAge ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> Int32
    -- ^ /@days@/: number of days
    -> m ()
recentFilterAddAge filter days = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_recent_filter_add_age filter' days
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentFilterAddAgeMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterAddAgeMethodInfo a signature where
    overloadedMethod = recentFilterAddAge

instance O.OverloadedMethodInfo RecentFilterAddAgeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterAddAge",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterAddAge"
        })


#endif

-- method RecentFilter::add_application
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "application"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an application name"
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

foreign import ccall "gtk_recent_filter_add_application" gtk_recent_filter_add_application :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    CString ->                              -- application : TBasicType TUTF8
    IO ()

-- | Adds a rule that allows resources based on the name of the application
-- that has registered them.
-- 
-- /Since: 2.10/
recentFilterAddApplication ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> T.Text
    -- ^ /@application@/: an application name
    -> m ()
recentFilterAddApplication filter application = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    application' <- textToCString application
    gtk_recent_filter_add_application filter' application'
    touchManagedPtr filter
    freeMem application'
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentFilterAddApplicationMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterAddApplicationMethodInfo a signature where
    overloadedMethod = recentFilterAddApplication

instance O.OverloadedMethodInfo RecentFilterAddApplicationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterAddApplication",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterAddApplication"
        })


#endif

-- method RecentFilter::add_custom
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "needed"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilterFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "bitfield of flags indicating the information that the custom\n         filter function needs."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilterFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "callback function; if the function returns %TRUE, then\n  the file will be displayed."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 3
--           , argDestroy = 4
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "data to pass to @func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data_destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "function to call to free @data when it is no longer needed."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
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

foreign import ccall "gtk_recent_filter_add_custom" gtk_recent_filter_add_custom :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    CUInt ->                                -- needed : TInterface (Name {namespace = "Gtk", name = "RecentFilterFlags"})
    FunPtr Gtk.Callbacks.C_RecentFilterFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "RecentFilterFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- data_destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Adds a rule to a filter that allows resources based on a custom callback
-- function. The bitfield /@needed@/ which is passed in provides information
-- about what sorts of information that the filter function needs;
-- this allows GTK+ to avoid retrieving expensive information when
-- it isn’t needed by the filter.
-- 
-- /Since: 2.10/
recentFilterAddCustom ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> [Gtk.Flags.RecentFilterFlags]
    -- ^ /@needed@/: bitfield of flags indicating the information that the custom
    --          filter function needs.
    -> Gtk.Callbacks.RecentFilterFunc
    -- ^ /@func@/: callback function; if the function returns 'P.True', then
    --   the file will be displayed.
    -> m ()
recentFilterAddCustom filter needed func = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    let needed' = gflagsToWord needed
    func' <- Gtk.Callbacks.mk_RecentFilterFunc (Gtk.Callbacks.wrap_RecentFilterFunc Nothing (Gtk.Callbacks.drop_closures_RecentFilterFunc func))
    let data_ = castFunPtrToPtr func'
    let dataDestroy = SP.safeFreeFunPtrPtr
    gtk_recent_filter_add_custom filter' needed' func' data_ dataDestroy
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentFilterAddCustomMethodInfo
instance (signature ~ ([Gtk.Flags.RecentFilterFlags] -> Gtk.Callbacks.RecentFilterFunc -> m ()), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterAddCustomMethodInfo a signature where
    overloadedMethod = recentFilterAddCustom

instance O.OverloadedMethodInfo RecentFilterAddCustomMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterAddCustom",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterAddCustom"
        })


#endif

-- method RecentFilter::add_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a group name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_filter_add_group" gtk_recent_filter_add_group :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    CString ->                              -- group : TBasicType TUTF8
    IO ()

-- | Adds a rule that allows resources based on the name of the group
-- to which they belong
-- 
-- /Since: 2.10/
recentFilterAddGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> T.Text
    -- ^ /@group@/: a group name
    -> m ()
recentFilterAddGroup filter group = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    group' <- textToCString group
    gtk_recent_filter_add_group filter' group'
    touchManagedPtr filter
    freeMem group'
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentFilterAddGroupMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterAddGroupMethodInfo a signature where
    overloadedMethod = recentFilterAddGroup

instance O.OverloadedMethodInfo RecentFilterAddGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterAddGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterAddGroup"
        })


#endif

-- method RecentFilter::add_mime_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mime_type"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a MIME type" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_filter_add_mime_type" gtk_recent_filter_add_mime_type :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    CString ->                              -- mime_type : TBasicType TUTF8
    IO ()

-- | Adds a rule that allows resources based on their registered MIME type.
-- 
-- /Since: 2.10/
recentFilterAddMimeType ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> T.Text
    -- ^ /@mimeType@/: a MIME type
    -> m ()
recentFilterAddMimeType filter mimeType = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    mimeType' <- textToCString mimeType
    gtk_recent_filter_add_mime_type filter' mimeType'
    touchManagedPtr filter
    freeMem mimeType'
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentFilterAddMimeTypeMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterAddMimeTypeMethodInfo a signature where
    overloadedMethod = recentFilterAddMimeType

instance O.OverloadedMethodInfo RecentFilterAddMimeTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterAddMimeType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterAddMimeType"
        })


#endif

-- method RecentFilter::add_pattern
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pattern"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a file pattern" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_filter_add_pattern" gtk_recent_filter_add_pattern :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    CString ->                              -- pattern : TBasicType TUTF8
    IO ()

-- | Adds a rule that allows resources based on a pattern matching their
-- display name.
-- 
-- /Since: 2.10/
recentFilterAddPattern ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> T.Text
    -- ^ /@pattern@/: a file pattern
    -> m ()
recentFilterAddPattern filter pattern = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    pattern' <- textToCString pattern
    gtk_recent_filter_add_pattern filter' pattern'
    touchManagedPtr filter
    freeMem pattern'
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentFilterAddPatternMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterAddPatternMethodInfo a signature where
    overloadedMethod = recentFilterAddPattern

instance O.OverloadedMethodInfo RecentFilterAddPatternMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterAddPattern",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterAddPattern"
        })


#endif

-- method RecentFilter::add_pixbuf_formats
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_filter_add_pixbuf_formats" gtk_recent_filter_add_pixbuf_formats :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    IO ()

-- | Adds a rule allowing image files in the formats supported
-- by GdkPixbuf.
-- 
-- /Since: 2.10/
recentFilterAddPixbufFormats ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> m ()
recentFilterAddPixbufFormats filter = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_recent_filter_add_pixbuf_formats filter'
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentFilterAddPixbufFormatsMethodInfo
instance (signature ~ (m ()), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterAddPixbufFormatsMethodInfo a signature where
    overloadedMethod = recentFilterAddPixbufFormats

instance O.OverloadedMethodInfo RecentFilterAddPixbufFormatsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterAddPixbufFormats",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterAddPixbufFormats"
        })


#endif

-- method RecentFilter::filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilterInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkRecentFilterInfo containing information\n  about a recently used resource"
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

foreign import ccall "gtk_recent_filter_filter" gtk_recent_filter_filter :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    Ptr Gtk.RecentFilterInfo.RecentFilterInfo -> -- filter_info : TInterface (Name {namespace = "Gtk", name = "RecentFilterInfo"})
    IO CInt

-- | Tests whether a file should be displayed according to /@filter@/.
-- The t'GI.Gtk.Structs.RecentFilterInfo.RecentFilterInfo' /@filterInfo@/ should include
-- the fields returned from 'GI.Gtk.Objects.RecentFilter.recentFilterGetNeeded', and
-- must set the t'GI.Gtk.Structs.RecentFilterInfo.RecentFilterInfo'.@/contains/@ field of /@filterInfo@/
-- to indicate which fields have been set.
-- 
-- This function will not typically be used by applications; it
-- is intended principally for use in the implementation of
-- t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'.
-- 
-- /Since: 2.10/
recentFilterFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> Gtk.RecentFilterInfo.RecentFilterInfo
    -- ^ /@filterInfo@/: a t'GI.Gtk.Structs.RecentFilterInfo.RecentFilterInfo' containing information
    --   about a recently used resource
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the file should be displayed
recentFilterFilter filter filterInfo = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    filterInfo' <- unsafeManagedPtrGetPtr filterInfo
    result <- gtk_recent_filter_filter filter' filterInfo'
    let result' = (/= 0) result
    touchManagedPtr filter
    touchManagedPtr filterInfo
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentFilterFilterMethodInfo
instance (signature ~ (Gtk.RecentFilterInfo.RecentFilterInfo -> m Bool), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterFilterMethodInfo a signature where
    overloadedMethod = recentFilterFilter

instance O.OverloadedMethodInfo RecentFilterFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterFilter"
        })


#endif

-- method RecentFilter::get_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_filter_get_name" gtk_recent_filter_get_name :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    IO CString

-- | Gets the human-readable name for the filter.
-- See 'GI.Gtk.Objects.RecentFilter.recentFilterSetName'.
-- 
-- /Since: 2.10/
recentFilterGetName ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the name of the filter, or 'P.Nothing'.  The returned string
    --   is owned by the filter object and should not be freed.
recentFilterGetName filter = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    result <- gtk_recent_filter_get_name filter'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr filter
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data RecentFilterGetNameMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterGetNameMethodInfo a signature where
    overloadedMethod = recentFilterGetName

instance O.OverloadedMethodInfo RecentFilterGetNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterGetName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterGetName"
        })


#endif

-- method RecentFilter::get_needed
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
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
--                  Name { namespace = "Gtk" , name = "RecentFilterFlags" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_filter_get_needed" gtk_recent_filter_get_needed :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    IO CUInt

-- | Gets the fields that need to be filled in for the t'GI.Gtk.Structs.RecentFilterInfo.RecentFilterInfo'
-- passed to 'GI.Gtk.Objects.RecentFilter.recentFilterFilter'
-- 
-- This function will not typically be used by applications; it
-- is intended principally for use in the implementation of
-- t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'.
-- 
-- /Since: 2.10/
recentFilterGetNeeded ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> m [Gtk.Flags.RecentFilterFlags]
    -- ^ __Returns:__ bitfield of flags indicating needed fields when
    --   calling 'GI.Gtk.Objects.RecentFilter.recentFilterFilter'
recentFilterGetNeeded filter = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    result <- gtk_recent_filter_get_needed filter'
    let result' = wordToGFlags result
    touchManagedPtr filter
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentFilterGetNeededMethodInfo
instance (signature ~ (m [Gtk.Flags.RecentFilterFlags]), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterGetNeededMethodInfo a signature where
    overloadedMethod = recentFilterGetNeeded

instance O.OverloadedMethodInfo RecentFilterGetNeededMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterGetNeeded",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterGetNeeded"
        })


#endif

-- method RecentFilter::set_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentFilter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "then human readable name of @filter"
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

foreign import ccall "gtk_recent_filter_set_name" gtk_recent_filter_set_name :: 
    Ptr RecentFilter ->                     -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Sets the human-readable name of the filter; this is the string
-- that will be displayed in the recently used resources selector
-- user interface if there is a selectable list of filters.
-- 
-- /Since: 2.10/
recentFilterSetName ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentFilter a) =>
    a
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> T.Text
    -- ^ /@name@/: then human readable name of /@filter@/
    -> m ()
recentFilterSetName filter name = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    name' <- textToCString name
    gtk_recent_filter_set_name filter' name'
    touchManagedPtr filter
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentFilterSetNameMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsRecentFilter a) => O.OverloadedMethod RecentFilterSetNameMethodInfo a signature where
    overloadedMethod = recentFilterSetName

instance O.OverloadedMethodInfo RecentFilterSetNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentFilter.recentFilterSetName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentFilter.html#v:recentFilterSetName"
        })


#endif


