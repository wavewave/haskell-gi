{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' is an interface that can be implemented by widgets
-- displaying the list of recently used files.  In GTK+, the main objects
-- that implement this interface are t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget',
-- t'GI.Gtk.Objects.RecentChooserDialog.RecentChooserDialog' and t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu'.
-- 
-- Recently used files are supported since GTK+ 2.10.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.RecentChooser
    ( 

-- * Exported types
    RecentChooser(..)                       ,
    IsRecentChooser                         ,
    toRecentChooser                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:addFilter"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [listFilters]("GI.Gtk.Interfaces.RecentChooser#g:method:listFilters"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [removeFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:removeFilter"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectAll]("GI.Gtk.Interfaces.RecentChooser#g:method:selectAll"), [selectUri]("GI.Gtk.Interfaces.RecentChooser#g:method:selectUri"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unselectAll]("GI.Gtk.Interfaces.RecentChooser#g:method:unselectAll"), [unselectUri]("GI.Gtk.Interfaces.RecentChooser#g:method:unselectUri"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getCurrentItem]("GI.Gtk.Interfaces.RecentChooser#g:method:getCurrentItem"), [getCurrentUri]("GI.Gtk.Interfaces.RecentChooser#g:method:getCurrentUri"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:getFilter"), [getItems]("GI.Gtk.Interfaces.RecentChooser#g:method:getItems"), [getLimit]("GI.Gtk.Interfaces.RecentChooser#g:method:getLimit"), [getLocalOnly]("GI.Gtk.Interfaces.RecentChooser#g:method:getLocalOnly"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSelectMultiple]("GI.Gtk.Interfaces.RecentChooser#g:method:getSelectMultiple"), [getShowIcons]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowIcons"), [getShowNotFound]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowNotFound"), [getShowPrivate]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowPrivate"), [getShowTips]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowTips"), [getSortType]("GI.Gtk.Interfaces.RecentChooser#g:method:getSortType"), [getUris]("GI.Gtk.Interfaces.RecentChooser#g:method:getUris").
-- 
-- ==== Setters
-- [setCurrentUri]("GI.Gtk.Interfaces.RecentChooser#g:method:setCurrentUri"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:setFilter"), [setLimit]("GI.Gtk.Interfaces.RecentChooser#g:method:setLimit"), [setLocalOnly]("GI.Gtk.Interfaces.RecentChooser#g:method:setLocalOnly"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSelectMultiple]("GI.Gtk.Interfaces.RecentChooser#g:method:setSelectMultiple"), [setShowIcons]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowIcons"), [setShowNotFound]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowNotFound"), [setShowPrivate]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowPrivate"), [setShowTips]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowTips"), [setSortFunc]("GI.Gtk.Interfaces.RecentChooser#g:method:setSortFunc"), [setSortType]("GI.Gtk.Interfaces.RecentChooser#g:method:setSortType").

#if defined(ENABLE_OVERLOADING)
    ResolveRecentChooserMethod              ,
#endif

-- ** addFilter #method:addFilter#

#if defined(ENABLE_OVERLOADING)
    RecentChooserAddFilterMethodInfo        ,
#endif
    recentChooserAddFilter                  ,


-- ** getCurrentItem #method:getCurrentItem#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetCurrentItemMethodInfo   ,
#endif
    recentChooserGetCurrentItem             ,


-- ** getCurrentUri #method:getCurrentUri#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetCurrentUriMethodInfo    ,
#endif
    recentChooserGetCurrentUri              ,


-- ** getFilter #method:getFilter#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetFilterMethodInfo        ,
#endif
    recentChooserGetFilter                  ,


-- ** getItems #method:getItems#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetItemsMethodInfo         ,
#endif
    recentChooserGetItems                   ,


-- ** getLimit #method:getLimit#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetLimitMethodInfo         ,
#endif
    recentChooserGetLimit                   ,


-- ** getLocalOnly #method:getLocalOnly#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetLocalOnlyMethodInfo     ,
#endif
    recentChooserGetLocalOnly               ,


-- ** getSelectMultiple #method:getSelectMultiple#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetSelectMultipleMethodInfo,
#endif
    recentChooserGetSelectMultiple          ,


-- ** getShowIcons #method:getShowIcons#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetShowIconsMethodInfo     ,
#endif
    recentChooserGetShowIcons               ,


-- ** getShowNotFound #method:getShowNotFound#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetShowNotFoundMethodInfo  ,
#endif
    recentChooserGetShowNotFound            ,


-- ** getShowPrivate #method:getShowPrivate#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetShowPrivateMethodInfo   ,
#endif
    recentChooserGetShowPrivate             ,


-- ** getShowTips #method:getShowTips#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetShowTipsMethodInfo      ,
#endif
    recentChooserGetShowTips                ,


-- ** getSortType #method:getSortType#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetSortTypeMethodInfo      ,
#endif
    recentChooserGetSortType                ,


-- ** getUris #method:getUris#

#if defined(ENABLE_OVERLOADING)
    RecentChooserGetUrisMethodInfo          ,
#endif
    recentChooserGetUris                    ,


-- ** listFilters #method:listFilters#

#if defined(ENABLE_OVERLOADING)
    RecentChooserListFiltersMethodInfo      ,
#endif
    recentChooserListFilters                ,


-- ** removeFilter #method:removeFilter#

#if defined(ENABLE_OVERLOADING)
    RecentChooserRemoveFilterMethodInfo     ,
#endif
    recentChooserRemoveFilter               ,


-- ** selectAll #method:selectAll#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSelectAllMethodInfo        ,
#endif
    recentChooserSelectAll                  ,


-- ** selectUri #method:selectUri#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSelectUriMethodInfo        ,
#endif
    recentChooserSelectUri                  ,


-- ** setCurrentUri #method:setCurrentUri#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetCurrentUriMethodInfo    ,
#endif
    recentChooserSetCurrentUri              ,


-- ** setFilter #method:setFilter#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetFilterMethodInfo        ,
#endif
    recentChooserSetFilter                  ,


-- ** setLimit #method:setLimit#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetLimitMethodInfo         ,
#endif
    recentChooserSetLimit                   ,


-- ** setLocalOnly #method:setLocalOnly#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetLocalOnlyMethodInfo     ,
#endif
    recentChooserSetLocalOnly               ,


-- ** setSelectMultiple #method:setSelectMultiple#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetSelectMultipleMethodInfo,
#endif
    recentChooserSetSelectMultiple          ,


-- ** setShowIcons #method:setShowIcons#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetShowIconsMethodInfo     ,
#endif
    recentChooserSetShowIcons               ,


-- ** setShowNotFound #method:setShowNotFound#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetShowNotFoundMethodInfo  ,
#endif
    recentChooserSetShowNotFound            ,


-- ** setShowPrivate #method:setShowPrivate#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetShowPrivateMethodInfo   ,
#endif
    recentChooserSetShowPrivate             ,


-- ** setShowTips #method:setShowTips#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetShowTipsMethodInfo      ,
#endif
    recentChooserSetShowTips                ,


-- ** setSortFunc #method:setSortFunc#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetSortFuncMethodInfo      ,
#endif
    recentChooserSetSortFunc                ,


-- ** setSortType #method:setSortType#

#if defined(ENABLE_OVERLOADING)
    RecentChooserSetSortTypeMethodInfo      ,
#endif
    recentChooserSetSortType                ,


-- ** unselectAll #method:unselectAll#

#if defined(ENABLE_OVERLOADING)
    RecentChooserUnselectAllMethodInfo      ,
#endif
    recentChooserUnselectAll                ,


-- ** unselectUri #method:unselectUri#

#if defined(ENABLE_OVERLOADING)
    RecentChooserUnselectUriMethodInfo      ,
#endif
    recentChooserUnselectUri                ,




 -- * Properties


-- ** filter #attr:filter#
-- | The t'GI.Gtk.Objects.RecentFilter.RecentFilter' object to be used when displaying
-- the recently used resources.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserFilterPropertyInfo         ,
#endif
    clearRecentChooserFilter                ,
    constructRecentChooserFilter            ,
    getRecentChooserFilter                  ,
#if defined(ENABLE_OVERLOADING)
    recentChooserFilter                     ,
#endif
    setRecentChooserFilter                  ,


-- ** limit #attr:limit#
-- | The maximum number of recently used resources to be displayed,
-- or -1 to display all items.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserLimitPropertyInfo          ,
#endif
    constructRecentChooserLimit             ,
    getRecentChooserLimit                   ,
#if defined(ENABLE_OVERLOADING)
    recentChooserLimit                      ,
#endif
    setRecentChooserLimit                   ,


-- ** localOnly #attr:localOnly#
-- | Whether this t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' should display only local (file:)
-- resources.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserLocalOnlyPropertyInfo      ,
#endif
    constructRecentChooserLocalOnly         ,
    getRecentChooserLocalOnly               ,
#if defined(ENABLE_OVERLOADING)
    recentChooserLocalOnly                  ,
#endif
    setRecentChooserLocalOnly               ,


-- ** recentManager #attr:recentManager#
-- | The t'GI.Gtk.Objects.RecentManager.RecentManager' instance used by the t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' to
-- display the list of recently used resources.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserRecentManagerPropertyInfo  ,
#endif
    constructRecentChooserRecentManager     ,
#if defined(ENABLE_OVERLOADING)
    recentChooserRecentManager              ,
#endif


-- ** selectMultiple #attr:selectMultiple#
-- | Allow the user to select multiple resources.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserSelectMultiplePropertyInfo ,
#endif
    constructRecentChooserSelectMultiple    ,
    getRecentChooserSelectMultiple          ,
#if defined(ENABLE_OVERLOADING)
    recentChooserSelectMultiple             ,
#endif
    setRecentChooserSelectMultiple          ,


-- ** showIcons #attr:showIcons#
-- | Whether this t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' should display an icon near the item.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserShowIconsPropertyInfo      ,
#endif
    constructRecentChooserShowIcons         ,
    getRecentChooserShowIcons               ,
#if defined(ENABLE_OVERLOADING)
    recentChooserShowIcons                  ,
#endif
    setRecentChooserShowIcons               ,


-- ** showNotFound #attr:showNotFound#
-- | Whether this t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' should display the recently used resources
-- even if not present anymore. Setting this to 'P.False' will perform a
-- potentially expensive check on every local resource (every remote
-- resource will always be displayed).
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserShowNotFoundPropertyInfo   ,
#endif
    constructRecentChooserShowNotFound      ,
    getRecentChooserShowNotFound            ,
#if defined(ENABLE_OVERLOADING)
    recentChooserShowNotFound               ,
#endif
    setRecentChooserShowNotFound            ,


-- ** showPrivate #attr:showPrivate#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    RecentChooserShowPrivatePropertyInfo    ,
#endif
    constructRecentChooserShowPrivate       ,
    getRecentChooserShowPrivate             ,
#if defined(ENABLE_OVERLOADING)
    recentChooserShowPrivate                ,
#endif
    setRecentChooserShowPrivate             ,


-- ** showTips #attr:showTips#
-- | Whether this t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' should display a tooltip containing the
-- full path of the recently used resources.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserShowTipsPropertyInfo       ,
#endif
    constructRecentChooserShowTips          ,
    getRecentChooserShowTips                ,
#if defined(ENABLE_OVERLOADING)
    recentChooserShowTips                   ,
#endif
    setRecentChooserShowTips                ,


-- ** sortType #attr:sortType#
-- | Sorting order to be used when displaying the recently used resources.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserSortTypePropertyInfo       ,
#endif
    constructRecentChooserSortType          ,
    getRecentChooserSortType                ,
#if defined(ENABLE_OVERLOADING)
    recentChooserSortType                   ,
#endif
    setRecentChooserSortType                ,




 -- * Signals


-- ** itemActivated #signal:itemActivated#

    RecentChooserItemActivatedCallback      ,
#if defined(ENABLE_OVERLOADING)
    RecentChooserItemActivatedSignalInfo    ,
#endif
    afterRecentChooserItemActivated         ,
    onRecentChooserItemActivated            ,


-- ** selectionChanged #signal:selectionChanged#

    RecentChooserSelectionChangedCallback   ,
#if defined(ENABLE_OVERLOADING)
    RecentChooserSelectionChangedSignalInfo ,
#endif
    afterRecentChooserSelectionChanged      ,
    onRecentChooserSelectionChanged         ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Objects.RecentFilter as Gtk.RecentFilter
import {-# SOURCE #-} qualified GI.Gtk.Objects.RecentManager as Gtk.RecentManager
import {-# SOURCE #-} qualified GI.Gtk.Structs.RecentInfo as Gtk.RecentInfo

-- interface RecentChooser 
-- | Memory-managed wrapper type.
newtype RecentChooser = RecentChooser (SP.ManagedPtr RecentChooser)
    deriving (Eq)

instance SP.ManagedPtrNewtype RecentChooser where
    toManagedPtr (RecentChooser p) = p

foreign import ccall "gtk_recent_chooser_get_type"
    c_gtk_recent_chooser_get_type :: IO B.Types.GType

instance B.Types.TypedObject RecentChooser where
    glibType = c_gtk_recent_chooser_get_type

instance B.Types.GObject RecentChooser

-- | Type class for types which can be safely cast to `RecentChooser`, for instance with `toRecentChooser`.
class (SP.GObject o, O.IsDescendantOf RecentChooser o) => IsRecentChooser o
instance (SP.GObject o, O.IsDescendantOf RecentChooser o) => IsRecentChooser o

instance O.HasParentTypes RecentChooser
type instance O.ParentTypes RecentChooser = '[GObject.Object.Object]

-- | Cast to `RecentChooser`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toRecentChooser :: (MIO.MonadIO m, IsRecentChooser o) => o -> m RecentChooser
toRecentChooser = MIO.liftIO . B.ManagedPtr.unsafeCastTo RecentChooser

-- | Convert 'RecentChooser' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe RecentChooser) where
    gvalueGType_ = c_gtk_recent_chooser_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr RecentChooser)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr RecentChooser)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject RecentChooser ptr
        else return P.Nothing
        
    

-- VVV Prop "filter"
   -- Type: TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@filter@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #filter
-- @
getRecentChooserFilter :: (MonadIO m, IsRecentChooser o) => o -> m Gtk.RecentFilter.RecentFilter
getRecentChooserFilter obj = MIO.liftIO $ checkUnexpectedNothing "getRecentChooserFilter" $ B.Properties.getObjectPropertyObject obj "filter" Gtk.RecentFilter.RecentFilter

-- | Set the value of the “@filter@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #filter 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserFilter :: (MonadIO m, IsRecentChooser o, Gtk.RecentFilter.IsRecentFilter a) => o -> a -> m ()
setRecentChooserFilter obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "filter" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@filter@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserFilter :: (IsRecentChooser o, MIO.MonadIO m, Gtk.RecentFilter.IsRecentFilter a) => a -> m (GValueConstruct o)
constructRecentChooserFilter val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "filter" (P.Just val)

-- | Set the value of the “@filter@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #filter
-- @
clearRecentChooserFilter :: (MonadIO m, IsRecentChooser o) => o -> m ()
clearRecentChooserFilter obj = liftIO $ B.Properties.setObjectPropertyObject obj "filter" (Nothing :: Maybe Gtk.RecentFilter.RecentFilter)

#if defined(ENABLE_OVERLOADING)
data RecentChooserFilterPropertyInfo
instance AttrInfo RecentChooserFilterPropertyInfo where
    type AttrAllowedOps RecentChooserFilterPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint RecentChooserFilterPropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserFilterPropertyInfo = Gtk.RecentFilter.IsRecentFilter
    type AttrTransferTypeConstraint RecentChooserFilterPropertyInfo = Gtk.RecentFilter.IsRecentFilter
    type AttrTransferType RecentChooserFilterPropertyInfo = Gtk.RecentFilter.RecentFilter
    type AttrGetType RecentChooserFilterPropertyInfo = Gtk.RecentFilter.RecentFilter
    type AttrLabel RecentChooserFilterPropertyInfo = "filter"
    type AttrOrigin RecentChooserFilterPropertyInfo = RecentChooser
    attrGet = getRecentChooserFilter
    attrSet = setRecentChooserFilter
    attrTransfer _ v = do
        unsafeCastTo Gtk.RecentFilter.RecentFilter v
    attrConstruct = constructRecentChooserFilter
    attrClear = clearRecentChooserFilter
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.filter"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:filter"
        })
#endif

-- VVV Prop "limit"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@limit@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #limit
-- @
getRecentChooserLimit :: (MonadIO m, IsRecentChooser o) => o -> m Int32
getRecentChooserLimit obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "limit"

-- | Set the value of the “@limit@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #limit 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserLimit :: (MonadIO m, IsRecentChooser o) => o -> Int32 -> m ()
setRecentChooserLimit obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "limit" val

-- | Construct a `GValueConstruct` with valid value for the “@limit@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserLimit :: (IsRecentChooser o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructRecentChooserLimit val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "limit" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserLimitPropertyInfo
instance AttrInfo RecentChooserLimitPropertyInfo where
    type AttrAllowedOps RecentChooserLimitPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserLimitPropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserLimitPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint RecentChooserLimitPropertyInfo = (~) Int32
    type AttrTransferType RecentChooserLimitPropertyInfo = Int32
    type AttrGetType RecentChooserLimitPropertyInfo = Int32
    type AttrLabel RecentChooserLimitPropertyInfo = "limit"
    type AttrOrigin RecentChooserLimitPropertyInfo = RecentChooser
    attrGet = getRecentChooserLimit
    attrSet = setRecentChooserLimit
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserLimit
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.limit"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:limit"
        })
#endif

-- VVV Prop "local-only"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@local-only@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #localOnly
-- @
getRecentChooserLocalOnly :: (MonadIO m, IsRecentChooser o) => o -> m Bool
getRecentChooserLocalOnly obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "local-only"

-- | Set the value of the “@local-only@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #localOnly 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserLocalOnly :: (MonadIO m, IsRecentChooser o) => o -> Bool -> m ()
setRecentChooserLocalOnly obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "local-only" val

-- | Construct a `GValueConstruct` with valid value for the “@local-only@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserLocalOnly :: (IsRecentChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructRecentChooserLocalOnly val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "local-only" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserLocalOnlyPropertyInfo
instance AttrInfo RecentChooserLocalOnlyPropertyInfo where
    type AttrAllowedOps RecentChooserLocalOnlyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserLocalOnlyPropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserLocalOnlyPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint RecentChooserLocalOnlyPropertyInfo = (~) Bool
    type AttrTransferType RecentChooserLocalOnlyPropertyInfo = Bool
    type AttrGetType RecentChooserLocalOnlyPropertyInfo = Bool
    type AttrLabel RecentChooserLocalOnlyPropertyInfo = "local-only"
    type AttrOrigin RecentChooserLocalOnlyPropertyInfo = RecentChooser
    attrGet = getRecentChooserLocalOnly
    attrSet = setRecentChooserLocalOnly
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserLocalOnly
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.localOnly"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:localOnly"
        })
#endif

-- VVV Prop "recent-manager"
   -- Type: TInterface (Name {namespace = "Gtk", name = "RecentManager"})
   -- Flags: [PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Construct a `GValueConstruct` with valid value for the “@recent-manager@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserRecentManager :: (IsRecentChooser o, MIO.MonadIO m, Gtk.RecentManager.IsRecentManager a) => a -> m (GValueConstruct o)
constructRecentChooserRecentManager val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "recent-manager" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data RecentChooserRecentManagerPropertyInfo
instance AttrInfo RecentChooserRecentManagerPropertyInfo where
    type AttrAllowedOps RecentChooserRecentManagerPropertyInfo = '[ 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint RecentChooserRecentManagerPropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserRecentManagerPropertyInfo = Gtk.RecentManager.IsRecentManager
    type AttrTransferTypeConstraint RecentChooserRecentManagerPropertyInfo = Gtk.RecentManager.IsRecentManager
    type AttrTransferType RecentChooserRecentManagerPropertyInfo = Gtk.RecentManager.RecentManager
    type AttrGetType RecentChooserRecentManagerPropertyInfo = ()
    type AttrLabel RecentChooserRecentManagerPropertyInfo = "recent-manager"
    type AttrOrigin RecentChooserRecentManagerPropertyInfo = RecentChooser
    attrGet = undefined
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.RecentManager.RecentManager v
    attrConstruct = constructRecentChooserRecentManager
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentManager"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:recentManager"
        })
#endif

-- VVV Prop "select-multiple"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@select-multiple@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #selectMultiple
-- @
getRecentChooserSelectMultiple :: (MonadIO m, IsRecentChooser o) => o -> m Bool
getRecentChooserSelectMultiple obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "select-multiple"

-- | Set the value of the “@select-multiple@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #selectMultiple 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserSelectMultiple :: (MonadIO m, IsRecentChooser o) => o -> Bool -> m ()
setRecentChooserSelectMultiple obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "select-multiple" val

-- | Construct a `GValueConstruct` with valid value for the “@select-multiple@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserSelectMultiple :: (IsRecentChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructRecentChooserSelectMultiple val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "select-multiple" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserSelectMultiplePropertyInfo
instance AttrInfo RecentChooserSelectMultiplePropertyInfo where
    type AttrAllowedOps RecentChooserSelectMultiplePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserSelectMultiplePropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserSelectMultiplePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint RecentChooserSelectMultiplePropertyInfo = (~) Bool
    type AttrTransferType RecentChooserSelectMultiplePropertyInfo = Bool
    type AttrGetType RecentChooserSelectMultiplePropertyInfo = Bool
    type AttrLabel RecentChooserSelectMultiplePropertyInfo = "select-multiple"
    type AttrOrigin RecentChooserSelectMultiplePropertyInfo = RecentChooser
    attrGet = getRecentChooserSelectMultiple
    attrSet = setRecentChooserSelectMultiple
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserSelectMultiple
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.selectMultiple"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:selectMultiple"
        })
#endif

-- VVV Prop "show-icons"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-icons@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #showIcons
-- @
getRecentChooserShowIcons :: (MonadIO m, IsRecentChooser o) => o -> m Bool
getRecentChooserShowIcons obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-icons"

-- | Set the value of the “@show-icons@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #showIcons 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserShowIcons :: (MonadIO m, IsRecentChooser o) => o -> Bool -> m ()
setRecentChooserShowIcons obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-icons" val

-- | Construct a `GValueConstruct` with valid value for the “@show-icons@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserShowIcons :: (IsRecentChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructRecentChooserShowIcons val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-icons" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserShowIconsPropertyInfo
instance AttrInfo RecentChooserShowIconsPropertyInfo where
    type AttrAllowedOps RecentChooserShowIconsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserShowIconsPropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserShowIconsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint RecentChooserShowIconsPropertyInfo = (~) Bool
    type AttrTransferType RecentChooserShowIconsPropertyInfo = Bool
    type AttrGetType RecentChooserShowIconsPropertyInfo = Bool
    type AttrLabel RecentChooserShowIconsPropertyInfo = "show-icons"
    type AttrOrigin RecentChooserShowIconsPropertyInfo = RecentChooser
    attrGet = getRecentChooserShowIcons
    attrSet = setRecentChooserShowIcons
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserShowIcons
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.showIcons"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:showIcons"
        })
#endif

-- VVV Prop "show-not-found"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-not-found@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #showNotFound
-- @
getRecentChooserShowNotFound :: (MonadIO m, IsRecentChooser o) => o -> m Bool
getRecentChooserShowNotFound obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-not-found"

-- | Set the value of the “@show-not-found@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #showNotFound 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserShowNotFound :: (MonadIO m, IsRecentChooser o) => o -> Bool -> m ()
setRecentChooserShowNotFound obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-not-found" val

-- | Construct a `GValueConstruct` with valid value for the “@show-not-found@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserShowNotFound :: (IsRecentChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructRecentChooserShowNotFound val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-not-found" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserShowNotFoundPropertyInfo
instance AttrInfo RecentChooserShowNotFoundPropertyInfo where
    type AttrAllowedOps RecentChooserShowNotFoundPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserShowNotFoundPropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserShowNotFoundPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint RecentChooserShowNotFoundPropertyInfo = (~) Bool
    type AttrTransferType RecentChooserShowNotFoundPropertyInfo = Bool
    type AttrGetType RecentChooserShowNotFoundPropertyInfo = Bool
    type AttrLabel RecentChooserShowNotFoundPropertyInfo = "show-not-found"
    type AttrOrigin RecentChooserShowNotFoundPropertyInfo = RecentChooser
    attrGet = getRecentChooserShowNotFound
    attrSet = setRecentChooserShowNotFound
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserShowNotFound
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.showNotFound"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:showNotFound"
        })
#endif

-- VVV Prop "show-private"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-private@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #showPrivate
-- @
getRecentChooserShowPrivate :: (MonadIO m, IsRecentChooser o) => o -> m Bool
getRecentChooserShowPrivate obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-private"

-- | Set the value of the “@show-private@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #showPrivate 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserShowPrivate :: (MonadIO m, IsRecentChooser o) => o -> Bool -> m ()
setRecentChooserShowPrivate obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-private" val

-- | Construct a `GValueConstruct` with valid value for the “@show-private@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserShowPrivate :: (IsRecentChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructRecentChooserShowPrivate val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-private" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserShowPrivatePropertyInfo
instance AttrInfo RecentChooserShowPrivatePropertyInfo where
    type AttrAllowedOps RecentChooserShowPrivatePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserShowPrivatePropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserShowPrivatePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint RecentChooserShowPrivatePropertyInfo = (~) Bool
    type AttrTransferType RecentChooserShowPrivatePropertyInfo = Bool
    type AttrGetType RecentChooserShowPrivatePropertyInfo = Bool
    type AttrLabel RecentChooserShowPrivatePropertyInfo = "show-private"
    type AttrOrigin RecentChooserShowPrivatePropertyInfo = RecentChooser
    attrGet = getRecentChooserShowPrivate
    attrSet = setRecentChooserShowPrivate
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserShowPrivate
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.showPrivate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:showPrivate"
        })
#endif

-- VVV Prop "show-tips"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-tips@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #showTips
-- @
getRecentChooserShowTips :: (MonadIO m, IsRecentChooser o) => o -> m Bool
getRecentChooserShowTips obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-tips"

-- | Set the value of the “@show-tips@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #showTips 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserShowTips :: (MonadIO m, IsRecentChooser o) => o -> Bool -> m ()
setRecentChooserShowTips obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-tips" val

-- | Construct a `GValueConstruct` with valid value for the “@show-tips@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserShowTips :: (IsRecentChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructRecentChooserShowTips val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-tips" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserShowTipsPropertyInfo
instance AttrInfo RecentChooserShowTipsPropertyInfo where
    type AttrAllowedOps RecentChooserShowTipsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserShowTipsPropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserShowTipsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint RecentChooserShowTipsPropertyInfo = (~) Bool
    type AttrTransferType RecentChooserShowTipsPropertyInfo = Bool
    type AttrGetType RecentChooserShowTipsPropertyInfo = Bool
    type AttrLabel RecentChooserShowTipsPropertyInfo = "show-tips"
    type AttrOrigin RecentChooserShowTipsPropertyInfo = RecentChooser
    attrGet = getRecentChooserShowTips
    attrSet = setRecentChooserShowTips
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserShowTips
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.showTips"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:showTips"
        })
#endif

-- VVV Prop "sort-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "RecentSortType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@sort-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooser #sortType
-- @
getRecentChooserSortType :: (MonadIO m, IsRecentChooser o) => o -> m Gtk.Enums.RecentSortType
getRecentChooserSortType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "sort-type"

-- | Set the value of the “@sort-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooser [ #sortType 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserSortType :: (MonadIO m, IsRecentChooser o) => o -> Gtk.Enums.RecentSortType -> m ()
setRecentChooserSortType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "sort-type" val

-- | Construct a `GValueConstruct` with valid value for the “@sort-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserSortType :: (IsRecentChooser o, MIO.MonadIO m) => Gtk.Enums.RecentSortType -> m (GValueConstruct o)
constructRecentChooserSortType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "sort-type" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserSortTypePropertyInfo
instance AttrInfo RecentChooserSortTypePropertyInfo where
    type AttrAllowedOps RecentChooserSortTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserSortTypePropertyInfo = IsRecentChooser
    type AttrSetTypeConstraint RecentChooserSortTypePropertyInfo = (~) Gtk.Enums.RecentSortType
    type AttrTransferTypeConstraint RecentChooserSortTypePropertyInfo = (~) Gtk.Enums.RecentSortType
    type AttrTransferType RecentChooserSortTypePropertyInfo = Gtk.Enums.RecentSortType
    type AttrGetType RecentChooserSortTypePropertyInfo = Gtk.Enums.RecentSortType
    type AttrLabel RecentChooserSortTypePropertyInfo = "sort-type"
    type AttrOrigin RecentChooserSortTypePropertyInfo = RecentChooser
    attrGet = getRecentChooserSortType
    attrSet = setRecentChooserSortType
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserSortType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.sortType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:attr:sortType"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RecentChooser
type instance O.AttributeList RecentChooser = RecentChooserAttributeList
type RecentChooserAttributeList = ('[ '("filter", RecentChooserFilterPropertyInfo), '("limit", RecentChooserLimitPropertyInfo), '("localOnly", RecentChooserLocalOnlyPropertyInfo), '("recentManager", RecentChooserRecentManagerPropertyInfo), '("selectMultiple", RecentChooserSelectMultiplePropertyInfo), '("showIcons", RecentChooserShowIconsPropertyInfo), '("showNotFound", RecentChooserShowNotFoundPropertyInfo), '("showPrivate", RecentChooserShowPrivatePropertyInfo), '("showTips", RecentChooserShowTipsPropertyInfo), '("sortType", RecentChooserSortTypePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
recentChooserFilter :: AttrLabelProxy "filter"
recentChooserFilter = AttrLabelProxy

recentChooserLimit :: AttrLabelProxy "limit"
recentChooserLimit = AttrLabelProxy

recentChooserLocalOnly :: AttrLabelProxy "localOnly"
recentChooserLocalOnly = AttrLabelProxy

recentChooserRecentManager :: AttrLabelProxy "recentManager"
recentChooserRecentManager = AttrLabelProxy

recentChooserSelectMultiple :: AttrLabelProxy "selectMultiple"
recentChooserSelectMultiple = AttrLabelProxy

recentChooserShowIcons :: AttrLabelProxy "showIcons"
recentChooserShowIcons = AttrLabelProxy

recentChooserShowNotFound :: AttrLabelProxy "showNotFound"
recentChooserShowNotFound = AttrLabelProxy

recentChooserShowPrivate :: AttrLabelProxy "showPrivate"
recentChooserShowPrivate = AttrLabelProxy

recentChooserShowTips :: AttrLabelProxy "showTips"
recentChooserShowTips = AttrLabelProxy

recentChooserSortType :: AttrLabelProxy "sortType"
recentChooserSortType = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveRecentChooserMethod (t :: Symbol) (o :: *) :: * where
    ResolveRecentChooserMethod "addFilter" o = RecentChooserAddFilterMethodInfo
    ResolveRecentChooserMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveRecentChooserMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveRecentChooserMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveRecentChooserMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveRecentChooserMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveRecentChooserMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveRecentChooserMethod "listFilters" o = RecentChooserListFiltersMethodInfo
    ResolveRecentChooserMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveRecentChooserMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveRecentChooserMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveRecentChooserMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveRecentChooserMethod "removeFilter" o = RecentChooserRemoveFilterMethodInfo
    ResolveRecentChooserMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveRecentChooserMethod "selectAll" o = RecentChooserSelectAllMethodInfo
    ResolveRecentChooserMethod "selectUri" o = RecentChooserSelectUriMethodInfo
    ResolveRecentChooserMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveRecentChooserMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveRecentChooserMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveRecentChooserMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveRecentChooserMethod "unselectAll" o = RecentChooserUnselectAllMethodInfo
    ResolveRecentChooserMethod "unselectUri" o = RecentChooserUnselectUriMethodInfo
    ResolveRecentChooserMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveRecentChooserMethod "getCurrentItem" o = RecentChooserGetCurrentItemMethodInfo
    ResolveRecentChooserMethod "getCurrentUri" o = RecentChooserGetCurrentUriMethodInfo
    ResolveRecentChooserMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveRecentChooserMethod "getFilter" o = RecentChooserGetFilterMethodInfo
    ResolveRecentChooserMethod "getItems" o = RecentChooserGetItemsMethodInfo
    ResolveRecentChooserMethod "getLimit" o = RecentChooserGetLimitMethodInfo
    ResolveRecentChooserMethod "getLocalOnly" o = RecentChooserGetLocalOnlyMethodInfo
    ResolveRecentChooserMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveRecentChooserMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveRecentChooserMethod "getSelectMultiple" o = RecentChooserGetSelectMultipleMethodInfo
    ResolveRecentChooserMethod "getShowIcons" o = RecentChooserGetShowIconsMethodInfo
    ResolveRecentChooserMethod "getShowNotFound" o = RecentChooserGetShowNotFoundMethodInfo
    ResolveRecentChooserMethod "getShowPrivate" o = RecentChooserGetShowPrivateMethodInfo
    ResolveRecentChooserMethod "getShowTips" o = RecentChooserGetShowTipsMethodInfo
    ResolveRecentChooserMethod "getSortType" o = RecentChooserGetSortTypeMethodInfo
    ResolveRecentChooserMethod "getUris" o = RecentChooserGetUrisMethodInfo
    ResolveRecentChooserMethod "setCurrentUri" o = RecentChooserSetCurrentUriMethodInfo
    ResolveRecentChooserMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveRecentChooserMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveRecentChooserMethod "setFilter" o = RecentChooserSetFilterMethodInfo
    ResolveRecentChooserMethod "setLimit" o = RecentChooserSetLimitMethodInfo
    ResolveRecentChooserMethod "setLocalOnly" o = RecentChooserSetLocalOnlyMethodInfo
    ResolveRecentChooserMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveRecentChooserMethod "setSelectMultiple" o = RecentChooserSetSelectMultipleMethodInfo
    ResolveRecentChooserMethod "setShowIcons" o = RecentChooserSetShowIconsMethodInfo
    ResolveRecentChooserMethod "setShowNotFound" o = RecentChooserSetShowNotFoundMethodInfo
    ResolveRecentChooserMethod "setShowPrivate" o = RecentChooserSetShowPrivateMethodInfo
    ResolveRecentChooserMethod "setShowTips" o = RecentChooserSetShowTipsMethodInfo
    ResolveRecentChooserMethod "setSortFunc" o = RecentChooserSetSortFuncMethodInfo
    ResolveRecentChooserMethod "setSortType" o = RecentChooserSetSortTypeMethodInfo
    ResolveRecentChooserMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRecentChooserMethod t RecentChooser, O.OverloadedMethod info RecentChooser p) => OL.IsLabel t (RecentChooser -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRecentChooserMethod t RecentChooser, O.OverloadedMethod info RecentChooser p, R.HasField t RecentChooser p) => R.HasField t RecentChooser p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRecentChooserMethod t RecentChooser, O.OverloadedMethodInfo info RecentChooser) => OL.IsLabel t (O.MethodProxy info RecentChooser) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method RecentChooser::add_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
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

foreign import ccall "gtk_recent_chooser_add_filter" gtk_recent_chooser_add_filter :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    Ptr Gtk.RecentFilter.RecentFilter ->    -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    IO ()

-- | Adds /@filter@/ to the list of t'GI.Gtk.Objects.RecentFilter.RecentFilter' objects held by /@chooser@/.
-- 
-- If no previous filter objects were defined, this function will call
-- 'GI.Gtk.Interfaces.RecentChooser.recentChooserSetFilter'.
-- 
-- /Since: 2.10/
recentChooserAddFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a, Gtk.RecentFilter.IsRecentFilter b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> b
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> m ()
recentChooserAddFilter chooser filter = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_recent_chooser_add_filter chooser' filter'
    touchManagedPtr chooser
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserAddFilterMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsRecentChooser a, Gtk.RecentFilter.IsRecentFilter b) => O.OverloadedMethod RecentChooserAddFilterMethodInfo a signature where
    overloadedMethod = recentChooserAddFilter

instance O.OverloadedMethodInfo RecentChooserAddFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserAddFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserAddFilter"
        })


#endif

-- method RecentChooser::get_current_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "RecentInfo" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_get_current_item" gtk_recent_chooser_get_current_item :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO (Ptr Gtk.RecentInfo.RecentInfo)

-- | Gets the t'GI.Gtk.Structs.RecentInfo.RecentInfo' currently selected by /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserGetCurrentItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Gtk.RecentInfo.RecentInfo
    -- ^ __Returns:__ a t'GI.Gtk.Structs.RecentInfo.RecentInfo'.  Use 'GI.Gtk.Structs.RecentInfo.recentInfoUnref' when
    --   when you have finished using it.
recentChooserGetCurrentItem chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_current_item chooser'
    checkUnexpectedReturnNULL "recentChooserGetCurrentItem" result
    result' <- (wrapBoxed Gtk.RecentInfo.RecentInfo) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetCurrentItemMethodInfo
instance (signature ~ (m Gtk.RecentInfo.RecentInfo), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetCurrentItemMethodInfo a signature where
    overloadedMethod = recentChooserGetCurrentItem

instance O.OverloadedMethodInfo RecentChooserGetCurrentItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetCurrentItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetCurrentItem"
        })


#endif

-- method RecentChooser::get_current_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_get_current_uri" gtk_recent_chooser_get_current_uri :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO CString

-- | Gets the URI currently selected by /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserGetCurrentUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m T.Text
    -- ^ __Returns:__ a newly allocated string holding a URI.
recentChooserGetCurrentUri chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_current_uri chooser'
    checkUnexpectedReturnNULL "recentChooserGetCurrentUri" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetCurrentUriMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetCurrentUriMethodInfo a signature where
    overloadedMethod = recentChooserGetCurrentUri

instance O.OverloadedMethodInfo RecentChooserGetCurrentUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetCurrentUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetCurrentUri"
        })


#endif

-- method RecentChooser::get_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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
--               (TInterface Name { namespace = "Gtk" , name = "RecentFilter" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_get_filter" gtk_recent_chooser_get_filter :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO (Ptr Gtk.RecentFilter.RecentFilter)

-- | Gets the t'GI.Gtk.Objects.RecentFilter.RecentFilter' object currently used by /@chooser@/ to affect
-- the display of the recently used resources.
-- 
-- /Since: 2.10/
recentChooserGetFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Gtk.RecentFilter.RecentFilter
    -- ^ __Returns:__ a t'GI.Gtk.Objects.RecentFilter.RecentFilter' object.
recentChooserGetFilter chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_filter chooser'
    checkUnexpectedReturnNULL "recentChooserGetFilter" result
    result' <- (newObject Gtk.RecentFilter.RecentFilter) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetFilterMethodInfo
instance (signature ~ (m Gtk.RecentFilter.RecentFilter), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetFilterMethodInfo a signature where
    overloadedMethod = recentChooserGetFilter

instance O.OverloadedMethodInfo RecentChooserGetFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetFilter"
        })


#endif

-- method RecentChooser::get_items
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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
--               (TGList
--                  (TInterface Name { namespace = "Gtk" , name = "RecentInfo" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_get_items" gtk_recent_chooser_get_items :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO (Ptr (GList (Ptr Gtk.RecentInfo.RecentInfo)))

-- | Gets the list of recently used resources in form of t'GI.Gtk.Structs.RecentInfo.RecentInfo' objects.
-- 
-- The return value of this function is affected by the “sort-type” and
-- “limit” properties of /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserGetItems ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m [Gtk.RecentInfo.RecentInfo]
    -- ^ __Returns:__ A newly allocated
    --   list of t'GI.Gtk.Structs.RecentInfo.RecentInfo' objects.  You should
    --   use 'GI.Gtk.Structs.RecentInfo.recentInfoUnref' on every item of the list, and then free
    --   the list itself using @/g_list_free()/@.
recentChooserGetItems chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_items chooser'
    result' <- unpackGList result
    result'' <- mapM (wrapBoxed Gtk.RecentInfo.RecentInfo) result'
    g_list_free result
    touchManagedPtr chooser
    return result''

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetItemsMethodInfo
instance (signature ~ (m [Gtk.RecentInfo.RecentInfo]), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetItemsMethodInfo a signature where
    overloadedMethod = recentChooserGetItems

instance O.OverloadedMethodInfo RecentChooserGetItemsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetItems",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetItems"
        })


#endif

-- method RecentChooser::get_limit
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_get_limit" gtk_recent_chooser_get_limit :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO Int32

-- | Gets the number of items returned by 'GI.Gtk.Interfaces.RecentChooser.recentChooserGetItems'
-- and 'GI.Gtk.Interfaces.RecentChooser.recentChooserGetUris'.
-- 
-- /Since: 2.10/
recentChooserGetLimit ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Int32
    -- ^ __Returns:__ A positive integer, or -1 meaning that all items are
    --   returned.
recentChooserGetLimit chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_limit chooser'
    touchManagedPtr chooser
    return result

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetLimitMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetLimitMethodInfo a signature where
    overloadedMethod = recentChooserGetLimit

instance O.OverloadedMethodInfo RecentChooserGetLimitMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetLimit",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetLimit"
        })


#endif

-- method RecentChooser::get_local_only
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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

foreign import ccall "gtk_recent_chooser_get_local_only" gtk_recent_chooser_get_local_only :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO CInt

-- | Gets whether only local resources should be shown in the recently used
-- resources selector.  See 'GI.Gtk.Interfaces.RecentChooser.recentChooserSetLocalOnly'
-- 
-- /Since: 2.10/
recentChooserGetLocalOnly ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if only local resources should be shown.
recentChooserGetLocalOnly chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_local_only chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetLocalOnlyMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetLocalOnlyMethodInfo a signature where
    overloadedMethod = recentChooserGetLocalOnly

instance O.OverloadedMethodInfo RecentChooserGetLocalOnlyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetLocalOnly",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetLocalOnly"
        })


#endif

-- method RecentChooser::get_select_multiple
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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

foreign import ccall "gtk_recent_chooser_get_select_multiple" gtk_recent_chooser_get_select_multiple :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO CInt

-- | Gets whether /@chooser@/ can select multiple items.
-- 
-- /Since: 2.10/
recentChooserGetSelectMultiple ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@chooser@/ can select more than one item.
recentChooserGetSelectMultiple chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_select_multiple chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetSelectMultipleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetSelectMultipleMethodInfo a signature where
    overloadedMethod = recentChooserGetSelectMultiple

instance O.OverloadedMethodInfo RecentChooserGetSelectMultipleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetSelectMultiple",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetSelectMultiple"
        })


#endif

-- method RecentChooser::get_show_icons
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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

foreign import ccall "gtk_recent_chooser_get_show_icons" gtk_recent_chooser_get_show_icons :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO CInt

-- | Retrieves whether /@chooser@/ should show an icon near the resource.
-- 
-- /Since: 2.10/
recentChooserGetShowIcons ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the icons should be displayed, 'P.False' otherwise.
recentChooserGetShowIcons chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_show_icons chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetShowIconsMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetShowIconsMethodInfo a signature where
    overloadedMethod = recentChooserGetShowIcons

instance O.OverloadedMethodInfo RecentChooserGetShowIconsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetShowIcons",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetShowIcons"
        })


#endif

-- method RecentChooser::get_show_not_found
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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

foreign import ccall "gtk_recent_chooser_get_show_not_found" gtk_recent_chooser_get_show_not_found :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO CInt

-- | Retrieves whether /@chooser@/ should show the recently used resources that
-- were not found.
-- 
-- /Since: 2.10/
recentChooserGetShowNotFound ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the resources not found should be displayed, and
    --   'P.False' otheriwse.
recentChooserGetShowNotFound chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_show_not_found chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetShowNotFoundMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetShowNotFoundMethodInfo a signature where
    overloadedMethod = recentChooserGetShowNotFound

instance O.OverloadedMethodInfo RecentChooserGetShowNotFoundMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetShowNotFound",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetShowNotFound"
        })


#endif

-- method RecentChooser::get_show_private
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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

foreign import ccall "gtk_recent_chooser_get_show_private" gtk_recent_chooser_get_show_private :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO CInt

-- | Returns whether /@chooser@/ should display recently used resources
-- registered as private.
-- 
-- /Since: 2.10/
recentChooserGetShowPrivate ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the recent chooser should show private items,
    --   'P.False' otherwise.
recentChooserGetShowPrivate chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_show_private chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetShowPrivateMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetShowPrivateMethodInfo a signature where
    overloadedMethod = recentChooserGetShowPrivate

instance O.OverloadedMethodInfo RecentChooserGetShowPrivateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetShowPrivate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetShowPrivate"
        })


#endif

-- method RecentChooser::get_show_tips
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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

foreign import ccall "gtk_recent_chooser_get_show_tips" gtk_recent_chooser_get_show_tips :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO CInt

-- | Gets whether /@chooser@/ should display tooltips containing the full path
-- of a recently user resource.
-- 
-- /Since: 2.10/
recentChooserGetShowTips ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the recent chooser should show tooltips,
    --   'P.False' otherwise.
recentChooserGetShowTips chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_show_tips chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetShowTipsMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetShowTipsMethodInfo a signature where
    overloadedMethod = recentChooserGetShowTips

instance O.OverloadedMethodInfo RecentChooserGetShowTipsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetShowTips",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetShowTips"
        })


#endif

-- method RecentChooser::get_sort_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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
--               (TInterface Name { namespace = "Gtk" , name = "RecentSortType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_get_sort_type" gtk_recent_chooser_get_sort_type :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO CUInt

-- | Gets the value set by 'GI.Gtk.Interfaces.RecentChooser.recentChooserSetSortType'.
-- 
-- /Since: 2.10/
recentChooserGetSortType ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m Gtk.Enums.RecentSortType
    -- ^ __Returns:__ the sorting order of the /@chooser@/.
recentChooserGetSortType chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_get_sort_type chooser'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetSortTypeMethodInfo
instance (signature ~ (m Gtk.Enums.RecentSortType), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetSortTypeMethodInfo a signature where
    overloadedMethod = recentChooserGetSortType

instance O.OverloadedMethodInfo RecentChooserGetSortTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetSortType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetSortType"
        })


#endif

-- method RecentChooser::get_uris
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "length"
--           , argType = TBasicType TUInt64
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for a the length of the\n    URI list, or %NULL"
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
-- returnType: Just (TCArray True (-1) 1 (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_get_uris" gtk_recent_chooser_get_uris :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    Ptr Word64 ->                           -- length : TBasicType TUInt64
    IO (Ptr CString)

-- | Gets the URI of the recently used resources.
-- 
-- The return value of this function is affected by the “sort-type” and “limit”
-- properties of /@chooser@/.
-- 
-- Since the returned array is 'P.Nothing' terminated, /@length@/ may be 'P.Nothing'.
-- 
-- /Since: 2.10/
recentChooserGetUris ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m (([T.Text], Word64))
    -- ^ __Returns:__ 
    --     A newly allocated, 'P.Nothing'-terminated array of strings. Use
    --     'GI.GLib.Functions.strfreev' to free it.
recentChooserGetUris chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    length_ <- allocMem :: IO (Ptr Word64)
    result <- gtk_recent_chooser_get_uris chooser' length_
    checkUnexpectedReturnNULL "recentChooserGetUris" result
    result' <- unpackZeroTerminatedUTF8CArray result
    mapZeroTerminatedCArray freeMem result
    freeMem result
    length_' <- peek length_
    touchManagedPtr chooser
    freeMem length_
    return (result', length_')

#if defined(ENABLE_OVERLOADING)
data RecentChooserGetUrisMethodInfo
instance (signature ~ (m (([T.Text], Word64))), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserGetUrisMethodInfo a signature where
    overloadedMethod = recentChooserGetUris

instance O.OverloadedMethodInfo RecentChooserGetUrisMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserGetUris",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserGetUris"
        })


#endif

-- method RecentChooser::list_filters
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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
--               (TGSList
--                  (TInterface Name { namespace = "Gtk" , name = "RecentFilter" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_list_filters" gtk_recent_chooser_list_filters :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO (Ptr (GSList (Ptr Gtk.RecentFilter.RecentFilter)))

-- | Gets the t'GI.Gtk.Objects.RecentFilter.RecentFilter' objects held by /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserListFilters ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m [Gtk.RecentFilter.RecentFilter]
    -- ^ __Returns:__ A singly linked list
    --   of t'GI.Gtk.Objects.RecentFilter.RecentFilter' objects.  You
    --   should just free the returned list using @/g_slist_free()/@.
recentChooserListFilters chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_recent_chooser_list_filters chooser'
    result' <- unpackGSList result
    result'' <- mapM (newObject Gtk.RecentFilter.RecentFilter) result'
    g_slist_free result
    touchManagedPtr chooser
    return result''

#if defined(ENABLE_OVERLOADING)
data RecentChooserListFiltersMethodInfo
instance (signature ~ (m [Gtk.RecentFilter.RecentFilter]), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserListFiltersMethodInfo a signature where
    overloadedMethod = recentChooserListFilters

instance O.OverloadedMethodInfo RecentChooserListFiltersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserListFilters",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserListFilters"
        })


#endif

-- method RecentChooser::remove_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
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

foreign import ccall "gtk_recent_chooser_remove_filter" gtk_recent_chooser_remove_filter :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    Ptr Gtk.RecentFilter.RecentFilter ->    -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    IO ()

-- | Removes /@filter@/ from the list of t'GI.Gtk.Objects.RecentFilter.RecentFilter' objects held by /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserRemoveFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a, Gtk.RecentFilter.IsRecentFilter b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> b
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> m ()
recentChooserRemoveFilter chooser filter = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_recent_chooser_remove_filter chooser' filter'
    touchManagedPtr chooser
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserRemoveFilterMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsRecentChooser a, Gtk.RecentFilter.IsRecentFilter b) => O.OverloadedMethod RecentChooserRemoveFilterMethodInfo a signature where
    overloadedMethod = recentChooserRemoveFilter

instance O.OverloadedMethodInfo RecentChooserRemoveFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserRemoveFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserRemoveFilter"
        })


#endif

-- method RecentChooser::select_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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

foreign import ccall "gtk_recent_chooser_select_all" gtk_recent_chooser_select_all :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO ()

-- | Selects all the items inside /@chooser@/, if the /@chooser@/ supports
-- multiple selection.
-- 
-- /Since: 2.10/
recentChooserSelectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m ()
recentChooserSelectAll chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    gtk_recent_chooser_select_all chooser'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSelectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSelectAllMethodInfo a signature where
    overloadedMethod = recentChooserSelectAll

instance O.OverloadedMethodInfo RecentChooserSelectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSelectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSelectAll"
        })


#endif

-- method RecentChooser::select_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a URI" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_chooser_select_uri" gtk_recent_chooser_select_uri :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Selects /@uri@/ inside /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserSelectUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> T.Text
    -- ^ /@uri@/: a URI
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
recentChooserSelectUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    onException (do
        _ <- propagateGError $ gtk_recent_chooser_select_uri chooser' uri'
        touchManagedPtr chooser
        freeMem uri'
        return ()
     ) (do
        freeMem uri'
     )

#if defined(ENABLE_OVERLOADING)
data RecentChooserSelectUriMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSelectUriMethodInfo a signature where
    overloadedMethod = recentChooserSelectUri

instance O.OverloadedMethodInfo RecentChooserSelectUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSelectUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSelectUri"
        })


#endif

-- method RecentChooser::set_current_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a URI" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_chooser_set_current_uri" gtk_recent_chooser_set_current_uri :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Sets /@uri@/ as the current URI for /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserSetCurrentUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> T.Text
    -- ^ /@uri@/: a URI
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
recentChooserSetCurrentUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    onException (do
        _ <- propagateGError $ gtk_recent_chooser_set_current_uri chooser' uri'
        touchManagedPtr chooser
        freeMem uri'
        return ()
     ) (do
        freeMem uri'
     )

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetCurrentUriMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetCurrentUriMethodInfo a signature where
    overloadedMethod = recentChooserSetCurrentUri

instance O.OverloadedMethodInfo RecentChooserSetCurrentUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetCurrentUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetCurrentUri"
        })


#endif

-- method RecentChooser::set_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentFilter" }
--           , direction = DirectionIn
--           , mayBeNull = True
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

foreign import ccall "gtk_recent_chooser_set_filter" gtk_recent_chooser_set_filter :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    Ptr Gtk.RecentFilter.RecentFilter ->    -- filter : TInterface (Name {namespace = "Gtk", name = "RecentFilter"})
    IO ()

-- | Sets /@filter@/ as the current t'GI.Gtk.Objects.RecentFilter.RecentFilter' object used by /@chooser@/
-- to affect the displayed recently used resources.
-- 
-- /Since: 2.10/
recentChooserSetFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a, Gtk.RecentFilter.IsRecentFilter b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Maybe (b)
    -- ^ /@filter@/: a t'GI.Gtk.Objects.RecentFilter.RecentFilter'
    -> m ()
recentChooserSetFilter chooser filter = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    maybeFilter <- case filter of
        Nothing -> return nullPtr
        Just jFilter -> do
            jFilter' <- unsafeManagedPtrCastPtr jFilter
            return jFilter'
    gtk_recent_chooser_set_filter chooser' maybeFilter
    touchManagedPtr chooser
    whenJust filter touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetFilterMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsRecentChooser a, Gtk.RecentFilter.IsRecentFilter b) => O.OverloadedMethod RecentChooserSetFilterMethodInfo a signature where
    overloadedMethod = recentChooserSetFilter

instance O.OverloadedMethodInfo RecentChooserSetFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetFilter"
        })


#endif

-- method RecentChooser::set_limit
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "limit"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a positive integer, or -1 for all items"
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

foreign import ccall "gtk_recent_chooser_set_limit" gtk_recent_chooser_set_limit :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    Int32 ->                                -- limit : TBasicType TInt
    IO ()

-- | Sets the number of items that should be returned by
-- 'GI.Gtk.Interfaces.RecentChooser.recentChooserGetItems' and 'GI.Gtk.Interfaces.RecentChooser.recentChooserGetUris'.
-- 
-- /Since: 2.10/
recentChooserSetLimit ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Int32
    -- ^ /@limit@/: a positive integer, or -1 for all items
    -> m ()
recentChooserSetLimit chooser limit = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    gtk_recent_chooser_set_limit chooser' limit
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetLimitMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetLimitMethodInfo a signature where
    overloadedMethod = recentChooserSetLimit

instance O.OverloadedMethodInfo RecentChooserSetLimitMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetLimit",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetLimit"
        })


#endif

-- method RecentChooser::set_local_only
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "local_only"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if only local files can be shown"
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

foreign import ccall "gtk_recent_chooser_set_local_only" gtk_recent_chooser_set_local_only :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CInt ->                                 -- local_only : TBasicType TBoolean
    IO ()

-- | Sets whether only local resources, that is resources using the file:\/\/ URI
-- scheme, should be shown in the recently used resources selector.  If
-- /@localOnly@/ is 'P.True' (the default) then the shown resources are guaranteed
-- to be accessible through the operating system native file system.
-- 
-- /Since: 2.10/
recentChooserSetLocalOnly ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Bool
    -- ^ /@localOnly@/: 'P.True' if only local files can be shown
    -> m ()
recentChooserSetLocalOnly chooser localOnly = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let localOnly' = (fromIntegral . fromEnum) localOnly
    gtk_recent_chooser_set_local_only chooser' localOnly'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetLocalOnlyMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetLocalOnlyMethodInfo a signature where
    overloadedMethod = recentChooserSetLocalOnly

instance O.OverloadedMethodInfo RecentChooserSetLocalOnlyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetLocalOnly",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetLocalOnly"
        })


#endif

-- method RecentChooser::set_select_multiple
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "select_multiple"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if @chooser can select more than one item"
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

foreign import ccall "gtk_recent_chooser_set_select_multiple" gtk_recent_chooser_set_select_multiple :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CInt ->                                 -- select_multiple : TBasicType TBoolean
    IO ()

-- | Sets whether /@chooser@/ can select multiple items.
-- 
-- /Since: 2.10/
recentChooserSetSelectMultiple ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Bool
    -- ^ /@selectMultiple@/: 'P.True' if /@chooser@/ can select more than one item
    -> m ()
recentChooserSetSelectMultiple chooser selectMultiple = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let selectMultiple' = (fromIntegral . fromEnum) selectMultiple
    gtk_recent_chooser_set_select_multiple chooser' selectMultiple'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetSelectMultipleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetSelectMultipleMethodInfo a signature where
    overloadedMethod = recentChooserSetSelectMultiple

instance O.OverloadedMethodInfo RecentChooserSetSelectMultipleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetSelectMultiple",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetSelectMultiple"
        })


#endif

-- method RecentChooser::set_show_icons
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_icons"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to show an icon near the resource"
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

foreign import ccall "gtk_recent_chooser_set_show_icons" gtk_recent_chooser_set_show_icons :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CInt ->                                 -- show_icons : TBasicType TBoolean
    IO ()

-- | Sets whether /@chooser@/ should show an icon near the resource when
-- displaying it.
-- 
-- /Since: 2.10/
recentChooserSetShowIcons ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Bool
    -- ^ /@showIcons@/: whether to show an icon near the resource
    -> m ()
recentChooserSetShowIcons chooser showIcons = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let showIcons' = (fromIntegral . fromEnum) showIcons
    gtk_recent_chooser_set_show_icons chooser' showIcons'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetShowIconsMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetShowIconsMethodInfo a signature where
    overloadedMethod = recentChooserSetShowIcons

instance O.OverloadedMethodInfo RecentChooserSetShowIconsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetShowIcons",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetShowIcons"
        })


#endif

-- method RecentChooser::set_show_not_found
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_not_found"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether to show the local items we didn\8217t find"
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

foreign import ccall "gtk_recent_chooser_set_show_not_found" gtk_recent_chooser_set_show_not_found :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CInt ->                                 -- show_not_found : TBasicType TBoolean
    IO ()

-- | Sets whether /@chooser@/ should display the recently used resources that
-- it didn’t find.  This only applies to local resources.
-- 
-- /Since: 2.10/
recentChooserSetShowNotFound ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Bool
    -- ^ /@showNotFound@/: whether to show the local items we didn’t find
    -> m ()
recentChooserSetShowNotFound chooser showNotFound = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let showNotFound' = (fromIntegral . fromEnum) showNotFound
    gtk_recent_chooser_set_show_not_found chooser' showNotFound'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetShowNotFoundMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetShowNotFoundMethodInfo a signature where
    overloadedMethod = recentChooserSetShowNotFound

instance O.OverloadedMethodInfo RecentChooserSetShowNotFoundMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetShowNotFound",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetShowNotFound"
        })


#endif

-- method RecentChooser::set_show_private
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_private"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to show private items, %FALSE otherwise"
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

foreign import ccall "gtk_recent_chooser_set_show_private" gtk_recent_chooser_set_show_private :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CInt ->                                 -- show_private : TBasicType TBoolean
    IO ()

-- | Whether to show recently used resources marked registered as private.
-- 
-- /Since: 2.10/
recentChooserSetShowPrivate ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Bool
    -- ^ /@showPrivate@/: 'P.True' to show private items, 'P.False' otherwise
    -> m ()
recentChooserSetShowPrivate chooser showPrivate = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let showPrivate' = (fromIntegral . fromEnum) showPrivate
    gtk_recent_chooser_set_show_private chooser' showPrivate'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetShowPrivateMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetShowPrivateMethodInfo a signature where
    overloadedMethod = recentChooserSetShowPrivate

instance O.OverloadedMethodInfo RecentChooserSetShowPrivateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetShowPrivate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetShowPrivate"
        })


#endif

-- method RecentChooser::set_show_tips
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_tips"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if tooltips should be shown"
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

foreign import ccall "gtk_recent_chooser_set_show_tips" gtk_recent_chooser_set_show_tips :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CInt ->                                 -- show_tips : TBasicType TBoolean
    IO ()

-- | Sets whether to show a tooltips containing the full path of each
-- recently used resource in a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' widget.
-- 
-- /Since: 2.10/
recentChooserSetShowTips ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Bool
    -- ^ /@showTips@/: 'P.True' if tooltips should be shown
    -> m ()
recentChooserSetShowTips chooser showTips = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let showTips' = (fromIntegral . fromEnum) showTips
    gtk_recent_chooser_set_show_tips chooser' showTips'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetShowTipsMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetShowTipsMethodInfo a signature where
    overloadedMethod = recentChooserSetShowTips

instance O.OverloadedMethodInfo RecentChooserSetShowTipsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetShowTips",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetShowTips"
        })


#endif

-- method RecentChooser::set_sort_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentSortFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the comparison function"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data to pass to @sort_func, or %NULL"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "destroy notifier for @sort_data, or %NULL"
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

foreign import ccall "gtk_recent_chooser_set_sort_func" gtk_recent_chooser_set_sort_func :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    FunPtr Gtk.Callbacks.C_RecentSortFunc -> -- sort_func : TInterface (Name {namespace = "Gtk", name = "RecentSortFunc"})
    Ptr () ->                               -- sort_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- data_destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the comparison function used when sorting to be /@sortFunc@/.  If
-- the /@chooser@/ has the sort type set to @/GTK_RECENT_SORT_CUSTOM/@ then
-- the chooser will sort using this function.
-- 
-- To the comparison function will be passed two t'GI.Gtk.Structs.RecentInfo.RecentInfo' structs and
-- /@sortData@/;  /@sortFunc@/ should return a positive integer if the first
-- item comes before the second, zero if the two items are equal and
-- a negative integer if the first item comes after the second.
-- 
-- /Since: 2.10/
recentChooserSetSortFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Gtk.Callbacks.RecentSortFunc
    -- ^ /@sortFunc@/: the comparison function
    -> m ()
recentChooserSetSortFunc chooser sortFunc = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    sortFunc' <- Gtk.Callbacks.mk_RecentSortFunc (Gtk.Callbacks.wrap_RecentSortFunc Nothing (Gtk.Callbacks.drop_closures_RecentSortFunc sortFunc))
    let sortData = castFunPtrToPtr sortFunc'
    let dataDestroy = SP.safeFreeFunPtrPtr
    gtk_recent_chooser_set_sort_func chooser' sortFunc' sortData dataDestroy
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetSortFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.RecentSortFunc -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetSortFuncMethodInfo a signature where
    overloadedMethod = recentChooserSetSortFunc

instance O.OverloadedMethodInfo RecentChooserSetSortFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetSortFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetSortFunc"
        })


#endif

-- method RecentChooser::set_sort_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentSortType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "sort order that the chooser should use"
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

foreign import ccall "gtk_recent_chooser_set_sort_type" gtk_recent_chooser_set_sort_type :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CUInt ->                                -- sort_type : TInterface (Name {namespace = "Gtk", name = "RecentSortType"})
    IO ()

-- | Changes the sorting order of the recently used resources list displayed by
-- /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserSetSortType ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> Gtk.Enums.RecentSortType
    -- ^ /@sortType@/: sort order that the chooser should use
    -> m ()
recentChooserSetSortType chooser sortType = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let sortType' = (fromIntegral . fromEnum) sortType
    gtk_recent_chooser_set_sort_type chooser' sortType'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserSetSortTypeMethodInfo
instance (signature ~ (Gtk.Enums.RecentSortType -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserSetSortTypeMethodInfo a signature where
    overloadedMethod = recentChooserSetSortType

instance O.OverloadedMethodInfo RecentChooserSetSortTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserSetSortType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserSetSortType"
        })


#endif

-- method RecentChooser::unselect_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
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

foreign import ccall "gtk_recent_chooser_unselect_all" gtk_recent_chooser_unselect_all :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    IO ()

-- | Unselects all the items inside /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserUnselectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> m ()
recentChooserUnselectAll chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    gtk_recent_chooser_unselect_all chooser'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserUnselectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserUnselectAllMethodInfo a signature where
    overloadedMethod = recentChooserUnselectAll

instance O.OverloadedMethodInfo RecentChooserUnselectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserUnselectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserUnselectAll"
        })


#endif

-- method RecentChooser::unselect_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooser"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a URI" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_chooser_unselect_uri" gtk_recent_chooser_unselect_uri :: 
    Ptr RecentChooser ->                    -- chooser : TInterface (Name {namespace = "Gtk", name = "RecentChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    IO ()

-- | Unselects /@uri@/ inside /@chooser@/.
-- 
-- /Since: 2.10/
recentChooserUnselectUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'
    -> T.Text
    -- ^ /@uri@/: a URI
    -> m ()
recentChooserUnselectUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    gtk_recent_chooser_unselect_uri chooser' uri'
    touchManagedPtr chooser
    freeMem uri'
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserUnselectUriMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsRecentChooser a) => O.OverloadedMethod RecentChooserUnselectUriMethodInfo a signature where
    overloadedMethod = recentChooserUnselectUri

instance O.OverloadedMethodInfo RecentChooserUnselectUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser.recentChooserUnselectUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#v:recentChooserUnselectUri"
        })


#endif

-- signal RecentChooser::item-activated
-- | This signal is emitted when the user \"activates\" a recent item
-- in the recent chooser.  This can happen by double-clicking on an item
-- in the recently used resources list, or by pressing
-- @Enter@.
-- 
-- /Since: 2.10/
type RecentChooserItemActivatedCallback =
    IO ()

type C_RecentChooserItemActivatedCallback =
    Ptr RecentChooser ->                    -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_RecentChooserItemActivatedCallback`.
foreign import ccall "wrapper"
    mk_RecentChooserItemActivatedCallback :: C_RecentChooserItemActivatedCallback -> IO (FunPtr C_RecentChooserItemActivatedCallback)

wrap_RecentChooserItemActivatedCallback :: 
    GObject a => (a -> RecentChooserItemActivatedCallback) ->
    C_RecentChooserItemActivatedCallback
wrap_RecentChooserItemActivatedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [itemActivated](#signal:itemActivated) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' recentChooser #itemActivated callback
-- @
-- 
-- 
onRecentChooserItemActivated :: (IsRecentChooser a, MonadIO m) => a -> ((?self :: a) => RecentChooserItemActivatedCallback) -> m SignalHandlerId
onRecentChooserItemActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_RecentChooserItemActivatedCallback wrapped
    wrapped'' <- mk_RecentChooserItemActivatedCallback wrapped'
    connectSignalFunPtr obj "item-activated" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [itemActivated](#signal:itemActivated) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' recentChooser #itemActivated callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterRecentChooserItemActivated :: (IsRecentChooser a, MonadIO m) => a -> ((?self :: a) => RecentChooserItemActivatedCallback) -> m SignalHandlerId
afterRecentChooserItemActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_RecentChooserItemActivatedCallback wrapped
    wrapped'' <- mk_RecentChooserItemActivatedCallback wrapped'
    connectSignalFunPtr obj "item-activated" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data RecentChooserItemActivatedSignalInfo
instance SignalInfo RecentChooserItemActivatedSignalInfo where
    type HaskellCallbackType RecentChooserItemActivatedSignalInfo = RecentChooserItemActivatedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_RecentChooserItemActivatedCallback cb
        cb'' <- mk_RecentChooserItemActivatedCallback cb'
        connectSignalFunPtr obj "item-activated" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser::item-activated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:signal:itemActivated"})

#endif

-- signal RecentChooser::selection-changed
-- | This signal is emitted when there is a change in the set of
-- selected recently used resources.  This can happen when a user
-- modifies the selection with the mouse or the keyboard, or when
-- explicitly calling functions to change the selection.
-- 
-- /Since: 2.10/
type RecentChooserSelectionChangedCallback =
    IO ()

type C_RecentChooserSelectionChangedCallback =
    Ptr RecentChooser ->                    -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_RecentChooserSelectionChangedCallback`.
foreign import ccall "wrapper"
    mk_RecentChooserSelectionChangedCallback :: C_RecentChooserSelectionChangedCallback -> IO (FunPtr C_RecentChooserSelectionChangedCallback)

wrap_RecentChooserSelectionChangedCallback :: 
    GObject a => (a -> RecentChooserSelectionChangedCallback) ->
    C_RecentChooserSelectionChangedCallback
wrap_RecentChooserSelectionChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [selectionChanged](#signal:selectionChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' recentChooser #selectionChanged callback
-- @
-- 
-- 
onRecentChooserSelectionChanged :: (IsRecentChooser a, MonadIO m) => a -> ((?self :: a) => RecentChooserSelectionChangedCallback) -> m SignalHandlerId
onRecentChooserSelectionChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_RecentChooserSelectionChangedCallback wrapped
    wrapped'' <- mk_RecentChooserSelectionChangedCallback wrapped'
    connectSignalFunPtr obj "selection-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectionChanged](#signal:selectionChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' recentChooser #selectionChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterRecentChooserSelectionChanged :: (IsRecentChooser a, MonadIO m) => a -> ((?self :: a) => RecentChooserSelectionChangedCallback) -> m SignalHandlerId
afterRecentChooserSelectionChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_RecentChooserSelectionChangedCallback wrapped
    wrapped'' <- mk_RecentChooserSelectionChangedCallback wrapped'
    connectSignalFunPtr obj "selection-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data RecentChooserSelectionChangedSignalInfo
instance SignalInfo RecentChooserSelectionChangedSignalInfo where
    type HaskellCallbackType RecentChooserSelectionChangedSignalInfo = RecentChooserSelectionChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_RecentChooserSelectionChangedCallback cb
        cb'' <- mk_RecentChooserSelectionChangedCallback cb'
        connectSignalFunPtr obj "selection-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.RecentChooser::selection-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-RecentChooser.html#g:signal:selectionChanged"})

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList RecentChooser = RecentChooserSignalList
type RecentChooserSignalList = ('[ '("itemActivated", RecentChooserItemActivatedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("selectionChanged", RecentChooserSelectionChangedSignalInfo)] :: [(Symbol, *)])

#endif


