{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.StyleContext.StyleContext' is an object that stores styling information affecting
-- a widget defined by t'GI.Gtk.Structs.WidgetPath.WidgetPath'.
-- 
-- In order to construct the final style information, t'GI.Gtk.Objects.StyleContext.StyleContext'
-- queries information from all attached @/GtkStyleProviders/@. Style providers
-- can be either attached explicitly to the context through
-- 'GI.Gtk.Objects.StyleContext.styleContextAddProvider', or to the screen through
-- 'GI.Gtk.Objects.StyleContext.styleContextAddProviderForScreen'. The resulting style is a
-- combination of all providers’ information in priority order.
-- 
-- For GTK+ widgets, any t'GI.Gtk.Objects.StyleContext.StyleContext' returned by
-- 'GI.Gtk.Objects.Widget.widgetGetStyleContext' will already have a t'GI.Gtk.Structs.WidgetPath.WidgetPath', a
-- t'GI.Gdk.Objects.Screen.Screen' and RTL\/LTR information set. The style context will also be
-- updated automatically if any of these settings change on the widget.
-- 
-- If you are using the theming layer standalone, you will need to set a
-- widget path and a screen yourself to the created style context through
-- 'GI.Gtk.Objects.StyleContext.styleContextSetPath' and possibly 'GI.Gtk.Objects.StyleContext.styleContextSetScreen'. See
-- the “Foreign drawing“ example in gtk3-demo.
-- 
-- # Style Classes # {@/gtkstylecontext/@-classes}
-- 
-- Widgets can add style classes to their context, which can be used to associate
-- different styles by class. The documentation for individual widgets lists
-- which style classes it uses itself, and which style classes may be added by
-- applications to affect their appearance.
-- 
-- GTK+ defines macros for a number of style classes.
-- 
-- = Style Regions
-- 
-- Widgets can also add regions with flags to their context. This feature is
-- deprecated and will be removed in a future GTK+ update. Please use style
-- classes instead.
-- 
-- GTK+ defines macros for a number of style regions.
-- 
-- = Custom styling in UI libraries and applications
-- 
-- If you are developing a library with custom @/GtkWidgets/@ that
-- render differently than standard components, you may need to add a
-- t'GI.Gtk.Interfaces.StyleProvider.StyleProvider' yourself with the 'GI.Gtk.Constants.STYLE_PROVIDER_PRIORITY_FALLBACK'
-- priority, either a t'GI.Gtk.Objects.CssProvider.CssProvider' or a custom object implementing the
-- t'GI.Gtk.Interfaces.StyleProvider.StyleProvider' interface. This way themes may still attempt
-- to style your UI elements in a different way if needed so.
-- 
-- If you are using custom styling on an applications, you probably want then
-- to make your style information prevail to the theme’s, so you must use
-- a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider' with the 'GI.Gtk.Constants.STYLE_PROVIDER_PRIORITY_APPLICATION'
-- priority, keep in mind that the user settings in
-- @XDG_CONFIG_HOME\/gtk-3.0\/gtk.css@ will
-- still take precedence over your changes, as it uses the
-- 'GI.Gtk.Constants.STYLE_PROVIDER_PRIORITY_USER' priority.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.StyleContext
    ( 

-- * Exported types
    StyleContext(..)                        ,
    IsStyleContext                          ,
    toStyleContext                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addClass]("GI.Gtk.Objects.StyleContext#g:method:addClass"), [addProvider]("GI.Gtk.Objects.StyleContext#g:method:addProvider"), [addRegion]("GI.Gtk.Objects.StyleContext#g:method:addRegion"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [cancelAnimations]("GI.Gtk.Objects.StyleContext#g:method:cancelAnimations"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasClass]("GI.Gtk.Objects.StyleContext#g:method:hasClass"), [hasRegion]("GI.Gtk.Objects.StyleContext#g:method:hasRegion"), [invalidate]("GI.Gtk.Objects.StyleContext#g:method:invalidate"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [listClasses]("GI.Gtk.Objects.StyleContext#g:method:listClasses"), [listRegions]("GI.Gtk.Objects.StyleContext#g:method:listRegions"), [lookupColor]("GI.Gtk.Objects.StyleContext#g:method:lookupColor"), [lookupIconSet]("GI.Gtk.Objects.StyleContext#g:method:lookupIconSet"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Gtk.Objects.StyleContext#g:method:notifyStateChange"), [popAnimatableRegion]("GI.Gtk.Objects.StyleContext#g:method:popAnimatableRegion"), [pushAnimatableRegion]("GI.Gtk.Objects.StyleContext#g:method:pushAnimatableRegion"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [removeClass]("GI.Gtk.Objects.StyleContext#g:method:removeClass"), [removeProvider]("GI.Gtk.Objects.StyleContext#g:method:removeProvider"), [removeRegion]("GI.Gtk.Objects.StyleContext#g:method:removeRegion"), [restore]("GI.Gtk.Objects.StyleContext#g:method:restore"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [save]("GI.Gtk.Objects.StyleContext#g:method:save"), [scrollAnimations]("GI.Gtk.Objects.StyleContext#g:method:scrollAnimations"), [stateIsRunning]("GI.Gtk.Objects.StyleContext#g:method:stateIsRunning"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toString]("GI.Gtk.Objects.StyleContext#g:method:toString"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBackgroundColor]("GI.Gtk.Objects.StyleContext#g:method:getBackgroundColor"), [getBorder]("GI.Gtk.Objects.StyleContext#g:method:getBorder"), [getBorderColor]("GI.Gtk.Objects.StyleContext#g:method:getBorderColor"), [getColor]("GI.Gtk.Objects.StyleContext#g:method:getColor"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDirection]("GI.Gtk.Objects.StyleContext#g:method:getDirection"), [getFont]("GI.Gtk.Objects.StyleContext#g:method:getFont"), [getFrameClock]("GI.Gtk.Objects.StyleContext#g:method:getFrameClock"), [getJunctionSides]("GI.Gtk.Objects.StyleContext#g:method:getJunctionSides"), [getMargin]("GI.Gtk.Objects.StyleContext#g:method:getMargin"), [getPadding]("GI.Gtk.Objects.StyleContext#g:method:getPadding"), [getParent]("GI.Gtk.Objects.StyleContext#g:method:getParent"), [getPath]("GI.Gtk.Objects.StyleContext#g:method:getPath"), [getProperty]("GI.Gtk.Objects.StyleContext#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getScale]("GI.Gtk.Objects.StyleContext#g:method:getScale"), [getScreen]("GI.Gtk.Objects.StyleContext#g:method:getScreen"), [getSection]("GI.Gtk.Objects.StyleContext#g:method:getSection"), [getState]("GI.Gtk.Objects.StyleContext#g:method:getState"), [getStyleProperty]("GI.Gtk.Objects.StyleContext#g:method:getStyleProperty").
-- 
-- ==== Setters
-- [setBackground]("GI.Gtk.Objects.StyleContext#g:method:setBackground"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDirection]("GI.Gtk.Objects.StyleContext#g:method:setDirection"), [setFrameClock]("GI.Gtk.Objects.StyleContext#g:method:setFrameClock"), [setJunctionSides]("GI.Gtk.Objects.StyleContext#g:method:setJunctionSides"), [setParent]("GI.Gtk.Objects.StyleContext#g:method:setParent"), [setPath]("GI.Gtk.Objects.StyleContext#g:method:setPath"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setScale]("GI.Gtk.Objects.StyleContext#g:method:setScale"), [setScreen]("GI.Gtk.Objects.StyleContext#g:method:setScreen"), [setState]("GI.Gtk.Objects.StyleContext#g:method:setState").

#if defined(ENABLE_OVERLOADING)
    ResolveStyleContextMethod               ,
#endif

-- ** addClass #method:addClass#

#if defined(ENABLE_OVERLOADING)
    StyleContextAddClassMethodInfo          ,
#endif
    styleContextAddClass                    ,


-- ** addProvider #method:addProvider#

#if defined(ENABLE_OVERLOADING)
    StyleContextAddProviderMethodInfo       ,
#endif
    styleContextAddProvider                 ,


-- ** addProviderForScreen #method:addProviderForScreen#

    styleContextAddProviderForScreen        ,


-- ** addRegion #method:addRegion#

#if defined(ENABLE_OVERLOADING)
    StyleContextAddRegionMethodInfo         ,
#endif
    styleContextAddRegion                   ,


-- ** cancelAnimations #method:cancelAnimations#

#if defined(ENABLE_OVERLOADING)
    StyleContextCancelAnimationsMethodInfo  ,
#endif
    styleContextCancelAnimations            ,


-- ** getBackgroundColor #method:getBackgroundColor#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetBackgroundColorMethodInfo,
#endif
    styleContextGetBackgroundColor          ,


-- ** getBorder #method:getBorder#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetBorderMethodInfo         ,
#endif
    styleContextGetBorder                   ,


-- ** getBorderColor #method:getBorderColor#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetBorderColorMethodInfo    ,
#endif
    styleContextGetBorderColor              ,


-- ** getColor #method:getColor#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetColorMethodInfo          ,
#endif
    styleContextGetColor                    ,


-- ** getDirection #method:getDirection#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetDirectionMethodInfo      ,
#endif
    styleContextGetDirection                ,


-- ** getFont #method:getFont#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetFontMethodInfo           ,
#endif
    styleContextGetFont                     ,


-- ** getFrameClock #method:getFrameClock#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetFrameClockMethodInfo     ,
#endif
    styleContextGetFrameClock               ,


-- ** getJunctionSides #method:getJunctionSides#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetJunctionSidesMethodInfo  ,
#endif
    styleContextGetJunctionSides            ,


-- ** getMargin #method:getMargin#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetMarginMethodInfo         ,
#endif
    styleContextGetMargin                   ,


-- ** getPadding #method:getPadding#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetPaddingMethodInfo        ,
#endif
    styleContextGetPadding                  ,


-- ** getParent #method:getParent#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetParentMethodInfo         ,
#endif
    styleContextGetParent                   ,


-- ** getPath #method:getPath#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetPathMethodInfo           ,
#endif
    styleContextGetPath                     ,


-- ** getProperty #method:getProperty#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetPropertyMethodInfo       ,
#endif
    styleContextGetProperty                 ,


-- ** getScale #method:getScale#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetScaleMethodInfo          ,
#endif
    styleContextGetScale                    ,


-- ** getScreen #method:getScreen#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetScreenMethodInfo         ,
#endif
    styleContextGetScreen                   ,


-- ** getSection #method:getSection#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetSectionMethodInfo        ,
#endif
    styleContextGetSection                  ,


-- ** getState #method:getState#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetStateMethodInfo          ,
#endif
    styleContextGetState                    ,


-- ** getStyleProperty #method:getStyleProperty#

#if defined(ENABLE_OVERLOADING)
    StyleContextGetStylePropertyMethodInfo  ,
#endif
    styleContextGetStyleProperty            ,


-- ** hasClass #method:hasClass#

#if defined(ENABLE_OVERLOADING)
    StyleContextHasClassMethodInfo          ,
#endif
    styleContextHasClass                    ,


-- ** hasRegion #method:hasRegion#

#if defined(ENABLE_OVERLOADING)
    StyleContextHasRegionMethodInfo         ,
#endif
    styleContextHasRegion                   ,


-- ** invalidate #method:invalidate#

#if defined(ENABLE_OVERLOADING)
    StyleContextInvalidateMethodInfo        ,
#endif
    styleContextInvalidate                  ,


-- ** listClasses #method:listClasses#

#if defined(ENABLE_OVERLOADING)
    StyleContextListClassesMethodInfo       ,
#endif
    styleContextListClasses                 ,


-- ** listRegions #method:listRegions#

#if defined(ENABLE_OVERLOADING)
    StyleContextListRegionsMethodInfo       ,
#endif
    styleContextListRegions                 ,


-- ** lookupColor #method:lookupColor#

#if defined(ENABLE_OVERLOADING)
    StyleContextLookupColorMethodInfo       ,
#endif
    styleContextLookupColor                 ,


-- ** lookupIconSet #method:lookupIconSet#

#if defined(ENABLE_OVERLOADING)
    StyleContextLookupIconSetMethodInfo     ,
#endif
    styleContextLookupIconSet               ,


-- ** new #method:new#

    styleContextNew                         ,


-- ** notifyStateChange #method:notifyStateChange#

#if defined(ENABLE_OVERLOADING)
    StyleContextNotifyStateChangeMethodInfo ,
#endif
    styleContextNotifyStateChange           ,


-- ** popAnimatableRegion #method:popAnimatableRegion#

#if defined(ENABLE_OVERLOADING)
    StyleContextPopAnimatableRegionMethodInfo,
#endif
    styleContextPopAnimatableRegion         ,


-- ** pushAnimatableRegion #method:pushAnimatableRegion#

#if defined(ENABLE_OVERLOADING)
    StyleContextPushAnimatableRegionMethodInfo,
#endif
    styleContextPushAnimatableRegion        ,


-- ** removeClass #method:removeClass#

#if defined(ENABLE_OVERLOADING)
    StyleContextRemoveClassMethodInfo       ,
#endif
    styleContextRemoveClass                 ,


-- ** removeProvider #method:removeProvider#

#if defined(ENABLE_OVERLOADING)
    StyleContextRemoveProviderMethodInfo    ,
#endif
    styleContextRemoveProvider              ,


-- ** removeProviderForScreen #method:removeProviderForScreen#

    styleContextRemoveProviderForScreen     ,


-- ** removeRegion #method:removeRegion#

#if defined(ENABLE_OVERLOADING)
    StyleContextRemoveRegionMethodInfo      ,
#endif
    styleContextRemoveRegion                ,


-- ** resetWidgets #method:resetWidgets#

    styleContextResetWidgets                ,


-- ** restore #method:restore#

#if defined(ENABLE_OVERLOADING)
    StyleContextRestoreMethodInfo           ,
#endif
    styleContextRestore                     ,


-- ** save #method:save#

#if defined(ENABLE_OVERLOADING)
    StyleContextSaveMethodInfo              ,
#endif
    styleContextSave                        ,


-- ** scrollAnimations #method:scrollAnimations#

#if defined(ENABLE_OVERLOADING)
    StyleContextScrollAnimationsMethodInfo  ,
#endif
    styleContextScrollAnimations            ,


-- ** setBackground #method:setBackground#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetBackgroundMethodInfo     ,
#endif
    styleContextSetBackground               ,


-- ** setDirection #method:setDirection#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetDirectionMethodInfo      ,
#endif
    styleContextSetDirection                ,


-- ** setFrameClock #method:setFrameClock#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetFrameClockMethodInfo     ,
#endif
    styleContextSetFrameClock               ,


-- ** setJunctionSides #method:setJunctionSides#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetJunctionSidesMethodInfo  ,
#endif
    styleContextSetJunctionSides            ,


-- ** setParent #method:setParent#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetParentMethodInfo         ,
#endif
    styleContextSetParent                   ,


-- ** setPath #method:setPath#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetPathMethodInfo           ,
#endif
    styleContextSetPath                     ,


-- ** setScale #method:setScale#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetScaleMethodInfo          ,
#endif
    styleContextSetScale                    ,


-- ** setScreen #method:setScreen#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetScreenMethodInfo         ,
#endif
    styleContextSetScreen                   ,


-- ** setState #method:setState#

#if defined(ENABLE_OVERLOADING)
    StyleContextSetStateMethodInfo          ,
#endif
    styleContextSetState                    ,


-- ** stateIsRunning #method:stateIsRunning#

#if defined(ENABLE_OVERLOADING)
    StyleContextStateIsRunningMethodInfo    ,
#endif
    styleContextStateIsRunning              ,


-- ** toString #method:toString#

#if defined(ENABLE_OVERLOADING)
    StyleContextToStringMethodInfo          ,
#endif
    styleContextToString                    ,




 -- * Properties


-- ** direction #attr:direction#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StyleContextDirectionPropertyInfo       ,
#endif
    constructStyleContextDirection          ,
    getStyleContextDirection                ,
    setStyleContextDirection                ,
#if defined(ENABLE_OVERLOADING)
    styleContextDirection                   ,
#endif


-- ** paintClock #attr:paintClock#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StyleContextPaintClockPropertyInfo      ,
#endif
    clearStyleContextPaintClock             ,
    constructStyleContextPaintClock         ,
    getStyleContextPaintClock               ,
    setStyleContextPaintClock               ,
#if defined(ENABLE_OVERLOADING)
    styleContextPaintClock                  ,
#endif


-- ** parent #attr:parent#
-- | Sets or gets the style context’s parent. See 'GI.Gtk.Objects.StyleContext.styleContextSetParent'
-- for details.
-- 
-- /Since: 3.4/

#if defined(ENABLE_OVERLOADING)
    StyleContextParentPropertyInfo          ,
#endif
    clearStyleContextParent                 ,
    constructStyleContextParent             ,
    getStyleContextParent                   ,
    setStyleContextParent                   ,
#if defined(ENABLE_OVERLOADING)
    styleContextParent                      ,
#endif


-- ** screen #attr:screen#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StyleContextScreenPropertyInfo          ,
#endif
    constructStyleContextScreen             ,
    getStyleContextScreen                   ,
    setStyleContextScreen                   ,
#if defined(ENABLE_OVERLOADING)
    styleContextScreen                      ,
#endif




 -- * Signals


-- ** changed #signal:changed#

    StyleContextChangedCallback             ,
#if defined(ENABLE_OVERLOADING)
    StyleContextChangedSignalInfo           ,
#endif
    afterStyleContextChanged                ,
    onStyleContextChanged                   ,




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
import qualified GI.Gdk.Objects.FrameClock as Gdk.FrameClock
import qualified GI.Gdk.Objects.Screen as Gdk.Screen
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.Gdk.Structs.RGBA as Gdk.RGBA
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.StyleProvider as Gtk.StyleProvider
import {-# SOURCE #-} qualified GI.Gtk.Structs.Border as Gtk.Border
import {-# SOURCE #-} qualified GI.Gtk.Structs.CssSection as Gtk.CssSection
import {-# SOURCE #-} qualified GI.Gtk.Structs.IconSet as Gtk.IconSet
import {-# SOURCE #-} qualified GI.Gtk.Structs.WidgetPath as Gtk.WidgetPath
import qualified GI.Pango.Structs.FontDescription as Pango.FontDescription

-- | Memory-managed wrapper type.
newtype StyleContext = StyleContext (SP.ManagedPtr StyleContext)
    deriving (Eq)

instance SP.ManagedPtrNewtype StyleContext where
    toManagedPtr (StyleContext p) = p

foreign import ccall "gtk_style_context_get_type"
    c_gtk_style_context_get_type :: IO B.Types.GType

instance B.Types.TypedObject StyleContext where
    glibType = c_gtk_style_context_get_type

instance B.Types.GObject StyleContext

-- | Type class for types which can be safely cast to `StyleContext`, for instance with `toStyleContext`.
class (SP.GObject o, O.IsDescendantOf StyleContext o) => IsStyleContext o
instance (SP.GObject o, O.IsDescendantOf StyleContext o) => IsStyleContext o

instance O.HasParentTypes StyleContext
type instance O.ParentTypes StyleContext = '[GObject.Object.Object]

-- | Cast to `StyleContext`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toStyleContext :: (MIO.MonadIO m, IsStyleContext o) => o -> m StyleContext
toStyleContext = MIO.liftIO . B.ManagedPtr.unsafeCastTo StyleContext

-- | Convert 'StyleContext' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe StyleContext) where
    gvalueGType_ = c_gtk_style_context_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr StyleContext)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr StyleContext)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject StyleContext ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveStyleContextMethod (t :: Symbol) (o :: *) :: * where
    ResolveStyleContextMethod "addClass" o = StyleContextAddClassMethodInfo
    ResolveStyleContextMethod "addProvider" o = StyleContextAddProviderMethodInfo
    ResolveStyleContextMethod "addRegion" o = StyleContextAddRegionMethodInfo
    ResolveStyleContextMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveStyleContextMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveStyleContextMethod "cancelAnimations" o = StyleContextCancelAnimationsMethodInfo
    ResolveStyleContextMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveStyleContextMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveStyleContextMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveStyleContextMethod "hasClass" o = StyleContextHasClassMethodInfo
    ResolveStyleContextMethod "hasRegion" o = StyleContextHasRegionMethodInfo
    ResolveStyleContextMethod "invalidate" o = StyleContextInvalidateMethodInfo
    ResolveStyleContextMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveStyleContextMethod "listClasses" o = StyleContextListClassesMethodInfo
    ResolveStyleContextMethod "listRegions" o = StyleContextListRegionsMethodInfo
    ResolveStyleContextMethod "lookupColor" o = StyleContextLookupColorMethodInfo
    ResolveStyleContextMethod "lookupIconSet" o = StyleContextLookupIconSetMethodInfo
    ResolveStyleContextMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveStyleContextMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveStyleContextMethod "notifyStateChange" o = StyleContextNotifyStateChangeMethodInfo
    ResolveStyleContextMethod "popAnimatableRegion" o = StyleContextPopAnimatableRegionMethodInfo
    ResolveStyleContextMethod "pushAnimatableRegion" o = StyleContextPushAnimatableRegionMethodInfo
    ResolveStyleContextMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveStyleContextMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveStyleContextMethod "removeClass" o = StyleContextRemoveClassMethodInfo
    ResolveStyleContextMethod "removeProvider" o = StyleContextRemoveProviderMethodInfo
    ResolveStyleContextMethod "removeRegion" o = StyleContextRemoveRegionMethodInfo
    ResolveStyleContextMethod "restore" o = StyleContextRestoreMethodInfo
    ResolveStyleContextMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveStyleContextMethod "save" o = StyleContextSaveMethodInfo
    ResolveStyleContextMethod "scrollAnimations" o = StyleContextScrollAnimationsMethodInfo
    ResolveStyleContextMethod "stateIsRunning" o = StyleContextStateIsRunningMethodInfo
    ResolveStyleContextMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveStyleContextMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveStyleContextMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveStyleContextMethod "toString" o = StyleContextToStringMethodInfo
    ResolveStyleContextMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveStyleContextMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveStyleContextMethod "getBackgroundColor" o = StyleContextGetBackgroundColorMethodInfo
    ResolveStyleContextMethod "getBorder" o = StyleContextGetBorderMethodInfo
    ResolveStyleContextMethod "getBorderColor" o = StyleContextGetBorderColorMethodInfo
    ResolveStyleContextMethod "getColor" o = StyleContextGetColorMethodInfo
    ResolveStyleContextMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveStyleContextMethod "getDirection" o = StyleContextGetDirectionMethodInfo
    ResolveStyleContextMethod "getFont" o = StyleContextGetFontMethodInfo
    ResolveStyleContextMethod "getFrameClock" o = StyleContextGetFrameClockMethodInfo
    ResolveStyleContextMethod "getJunctionSides" o = StyleContextGetJunctionSidesMethodInfo
    ResolveStyleContextMethod "getMargin" o = StyleContextGetMarginMethodInfo
    ResolveStyleContextMethod "getPadding" o = StyleContextGetPaddingMethodInfo
    ResolveStyleContextMethod "getParent" o = StyleContextGetParentMethodInfo
    ResolveStyleContextMethod "getPath" o = StyleContextGetPathMethodInfo
    ResolveStyleContextMethod "getProperty" o = StyleContextGetPropertyMethodInfo
    ResolveStyleContextMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveStyleContextMethod "getScale" o = StyleContextGetScaleMethodInfo
    ResolveStyleContextMethod "getScreen" o = StyleContextGetScreenMethodInfo
    ResolveStyleContextMethod "getSection" o = StyleContextGetSectionMethodInfo
    ResolveStyleContextMethod "getState" o = StyleContextGetStateMethodInfo
    ResolveStyleContextMethod "getStyleProperty" o = StyleContextGetStylePropertyMethodInfo
    ResolveStyleContextMethod "setBackground" o = StyleContextSetBackgroundMethodInfo
    ResolveStyleContextMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveStyleContextMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveStyleContextMethod "setDirection" o = StyleContextSetDirectionMethodInfo
    ResolveStyleContextMethod "setFrameClock" o = StyleContextSetFrameClockMethodInfo
    ResolveStyleContextMethod "setJunctionSides" o = StyleContextSetJunctionSidesMethodInfo
    ResolveStyleContextMethod "setParent" o = StyleContextSetParentMethodInfo
    ResolveStyleContextMethod "setPath" o = StyleContextSetPathMethodInfo
    ResolveStyleContextMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveStyleContextMethod "setScale" o = StyleContextSetScaleMethodInfo
    ResolveStyleContextMethod "setScreen" o = StyleContextSetScreenMethodInfo
    ResolveStyleContextMethod "setState" o = StyleContextSetStateMethodInfo
    ResolveStyleContextMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveStyleContextMethod t StyleContext, O.OverloadedMethod info StyleContext p) => OL.IsLabel t (StyleContext -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveStyleContextMethod t StyleContext, O.OverloadedMethod info StyleContext p, R.HasField t StyleContext p) => R.HasField t StyleContext p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveStyleContextMethod t StyleContext, O.OverloadedMethodInfo info StyleContext) => OL.IsLabel t (O.MethodProxy info StyleContext) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal StyleContext::changed
-- | The [changed](#g:signal:changed) signal is emitted when there is a change in the
-- t'GI.Gtk.Objects.StyleContext.StyleContext'.
-- 
-- For a t'GI.Gtk.Objects.StyleContext.StyleContext' returned by 'GI.Gtk.Objects.Widget.widgetGetStyleContext', the
-- [Widget::styleUpdated]("GI.Gtk.Objects.Widget#g:signal:styleUpdated") signal\/vfunc might be more convenient to use.
-- 
-- This signal is useful when using the theming layer standalone.
-- 
-- /Since: 3.0/
type StyleContextChangedCallback =
    IO ()

type C_StyleContextChangedCallback =
    Ptr StyleContext ->                     -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_StyleContextChangedCallback`.
foreign import ccall "wrapper"
    mk_StyleContextChangedCallback :: C_StyleContextChangedCallback -> IO (FunPtr C_StyleContextChangedCallback)

wrap_StyleContextChangedCallback :: 
    GObject a => (a -> StyleContextChangedCallback) ->
    C_StyleContextChangedCallback
wrap_StyleContextChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' styleContext #changed callback
-- @
-- 
-- 
onStyleContextChanged :: (IsStyleContext a, MonadIO m) => a -> ((?self :: a) => StyleContextChangedCallback) -> m SignalHandlerId
onStyleContextChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_StyleContextChangedCallback wrapped
    wrapped'' <- mk_StyleContextChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' styleContext #changed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterStyleContextChanged :: (IsStyleContext a, MonadIO m) => a -> ((?self :: a) => StyleContextChangedCallback) -> m SignalHandlerId
afterStyleContextChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_StyleContextChangedCallback wrapped
    wrapped'' <- mk_StyleContextChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data StyleContextChangedSignalInfo
instance SignalInfo StyleContextChangedSignalInfo where
    type HaskellCallbackType StyleContextChangedSignalInfo = StyleContextChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_StyleContextChangedCallback cb
        cb'' <- mk_StyleContextChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#g:signal:changed"})

#endif

-- VVV Prop "direction"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TextDirection"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@direction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' styleContext #direction
-- @
getStyleContextDirection :: (MonadIO m, IsStyleContext o) => o -> m Gtk.Enums.TextDirection
getStyleContextDirection obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "direction"

-- | Set the value of the “@direction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' styleContext [ #direction 'Data.GI.Base.Attributes.:=' value ]
-- @
setStyleContextDirection :: (MonadIO m, IsStyleContext o) => o -> Gtk.Enums.TextDirection -> m ()
setStyleContextDirection obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "direction" val

-- | Construct a `GValueConstruct` with valid value for the “@direction@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStyleContextDirection :: (IsStyleContext o, MIO.MonadIO m) => Gtk.Enums.TextDirection -> m (GValueConstruct o)
constructStyleContextDirection val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "direction" val

#if defined(ENABLE_OVERLOADING)
data StyleContextDirectionPropertyInfo
instance AttrInfo StyleContextDirectionPropertyInfo where
    type AttrAllowedOps StyleContextDirectionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StyleContextDirectionPropertyInfo = IsStyleContext
    type AttrSetTypeConstraint StyleContextDirectionPropertyInfo = (~) Gtk.Enums.TextDirection
    type AttrTransferTypeConstraint StyleContextDirectionPropertyInfo = (~) Gtk.Enums.TextDirection
    type AttrTransferType StyleContextDirectionPropertyInfo = Gtk.Enums.TextDirection
    type AttrGetType StyleContextDirectionPropertyInfo = Gtk.Enums.TextDirection
    type AttrLabel StyleContextDirectionPropertyInfo = "direction"
    type AttrOrigin StyleContextDirectionPropertyInfo = StyleContext
    attrGet = getStyleContextDirection
    attrSet = setStyleContextDirection
    attrTransfer _ v = do
        return v
    attrConstruct = constructStyleContextDirection
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.direction"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#g:attr:direction"
        })
#endif

-- VVV Prop "paint-clock"
   -- Type: TInterface (Name {namespace = "Gdk", name = "FrameClock"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@paint-clock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' styleContext #paintClock
-- @
getStyleContextPaintClock :: (MonadIO m, IsStyleContext o) => o -> m (Maybe Gdk.FrameClock.FrameClock)
getStyleContextPaintClock obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "paint-clock" Gdk.FrameClock.FrameClock

-- | Set the value of the “@paint-clock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' styleContext [ #paintClock 'Data.GI.Base.Attributes.:=' value ]
-- @
setStyleContextPaintClock :: (MonadIO m, IsStyleContext o, Gdk.FrameClock.IsFrameClock a) => o -> a -> m ()
setStyleContextPaintClock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "paint-clock" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@paint-clock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStyleContextPaintClock :: (IsStyleContext o, MIO.MonadIO m, Gdk.FrameClock.IsFrameClock a) => a -> m (GValueConstruct o)
constructStyleContextPaintClock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "paint-clock" (P.Just val)

-- | Set the value of the “@paint-clock@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #paintClock
-- @
clearStyleContextPaintClock :: (MonadIO m, IsStyleContext o) => o -> m ()
clearStyleContextPaintClock obj = liftIO $ B.Properties.setObjectPropertyObject obj "paint-clock" (Nothing :: Maybe Gdk.FrameClock.FrameClock)

#if defined(ENABLE_OVERLOADING)
data StyleContextPaintClockPropertyInfo
instance AttrInfo StyleContextPaintClockPropertyInfo where
    type AttrAllowedOps StyleContextPaintClockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint StyleContextPaintClockPropertyInfo = IsStyleContext
    type AttrSetTypeConstraint StyleContextPaintClockPropertyInfo = Gdk.FrameClock.IsFrameClock
    type AttrTransferTypeConstraint StyleContextPaintClockPropertyInfo = Gdk.FrameClock.IsFrameClock
    type AttrTransferType StyleContextPaintClockPropertyInfo = Gdk.FrameClock.FrameClock
    type AttrGetType StyleContextPaintClockPropertyInfo = (Maybe Gdk.FrameClock.FrameClock)
    type AttrLabel StyleContextPaintClockPropertyInfo = "paint-clock"
    type AttrOrigin StyleContextPaintClockPropertyInfo = StyleContext
    attrGet = getStyleContextPaintClock
    attrSet = setStyleContextPaintClock
    attrTransfer _ v = do
        unsafeCastTo Gdk.FrameClock.FrameClock v
    attrConstruct = constructStyleContextPaintClock
    attrClear = clearStyleContextPaintClock
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.paintClock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#g:attr:paintClock"
        })
#endif

-- VVV Prop "parent"
   -- Type: TInterface (Name {namespace = "Gtk", name = "StyleContext"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@parent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' styleContext #parent
-- @
getStyleContextParent :: (MonadIO m, IsStyleContext o) => o -> m (Maybe StyleContext)
getStyleContextParent obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "parent" StyleContext

-- | Set the value of the “@parent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' styleContext [ #parent 'Data.GI.Base.Attributes.:=' value ]
-- @
setStyleContextParent :: (MonadIO m, IsStyleContext o, IsStyleContext a) => o -> a -> m ()
setStyleContextParent obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "parent" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@parent@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStyleContextParent :: (IsStyleContext o, MIO.MonadIO m, IsStyleContext a) => a -> m (GValueConstruct o)
constructStyleContextParent val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "parent" (P.Just val)

-- | Set the value of the “@parent@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #parent
-- @
clearStyleContextParent :: (MonadIO m, IsStyleContext o) => o -> m ()
clearStyleContextParent obj = liftIO $ B.Properties.setObjectPropertyObject obj "parent" (Nothing :: Maybe StyleContext)

#if defined(ENABLE_OVERLOADING)
data StyleContextParentPropertyInfo
instance AttrInfo StyleContextParentPropertyInfo where
    type AttrAllowedOps StyleContextParentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint StyleContextParentPropertyInfo = IsStyleContext
    type AttrSetTypeConstraint StyleContextParentPropertyInfo = IsStyleContext
    type AttrTransferTypeConstraint StyleContextParentPropertyInfo = IsStyleContext
    type AttrTransferType StyleContextParentPropertyInfo = StyleContext
    type AttrGetType StyleContextParentPropertyInfo = (Maybe StyleContext)
    type AttrLabel StyleContextParentPropertyInfo = "parent"
    type AttrOrigin StyleContextParentPropertyInfo = StyleContext
    attrGet = getStyleContextParent
    attrSet = setStyleContextParent
    attrTransfer _ v = do
        unsafeCastTo StyleContext v
    attrConstruct = constructStyleContextParent
    attrClear = clearStyleContextParent
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.parent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#g:attr:parent"
        })
#endif

-- VVV Prop "screen"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Screen"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@screen@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' styleContext #screen
-- @
getStyleContextScreen :: (MonadIO m, IsStyleContext o) => o -> m Gdk.Screen.Screen
getStyleContextScreen obj = MIO.liftIO $ checkUnexpectedNothing "getStyleContextScreen" $ B.Properties.getObjectPropertyObject obj "screen" Gdk.Screen.Screen

-- | Set the value of the “@screen@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' styleContext [ #screen 'Data.GI.Base.Attributes.:=' value ]
-- @
setStyleContextScreen :: (MonadIO m, IsStyleContext o, Gdk.Screen.IsScreen a) => o -> a -> m ()
setStyleContextScreen obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "screen" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@screen@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStyleContextScreen :: (IsStyleContext o, MIO.MonadIO m, Gdk.Screen.IsScreen a) => a -> m (GValueConstruct o)
constructStyleContextScreen val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "screen" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data StyleContextScreenPropertyInfo
instance AttrInfo StyleContextScreenPropertyInfo where
    type AttrAllowedOps StyleContextScreenPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StyleContextScreenPropertyInfo = IsStyleContext
    type AttrSetTypeConstraint StyleContextScreenPropertyInfo = Gdk.Screen.IsScreen
    type AttrTransferTypeConstraint StyleContextScreenPropertyInfo = Gdk.Screen.IsScreen
    type AttrTransferType StyleContextScreenPropertyInfo = Gdk.Screen.Screen
    type AttrGetType StyleContextScreenPropertyInfo = Gdk.Screen.Screen
    type AttrLabel StyleContextScreenPropertyInfo = "screen"
    type AttrOrigin StyleContextScreenPropertyInfo = StyleContext
    attrGet = getStyleContextScreen
    attrSet = setStyleContextScreen
    attrTransfer _ v = do
        unsafeCastTo Gdk.Screen.Screen v
    attrConstruct = constructStyleContextScreen
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.screen"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#g:attr:screen"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList StyleContext
type instance O.AttributeList StyleContext = StyleContextAttributeList
type StyleContextAttributeList = ('[ '("direction", StyleContextDirectionPropertyInfo), '("paintClock", StyleContextPaintClockPropertyInfo), '("parent", StyleContextParentPropertyInfo), '("screen", StyleContextScreenPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
styleContextDirection :: AttrLabelProxy "direction"
styleContextDirection = AttrLabelProxy

styleContextPaintClock :: AttrLabelProxy "paintClock"
styleContextPaintClock = AttrLabelProxy

styleContextParent :: AttrLabelProxy "parent"
styleContextParent = AttrLabelProxy

styleContextScreen :: AttrLabelProxy "screen"
styleContextScreen = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList StyleContext = StyleContextSignalList
type StyleContextSignalList = ('[ '("changed", StyleContextChangedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method StyleContext::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "StyleContext" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_new" gtk_style_context_new :: 
    IO (Ptr StyleContext)

-- | Creates a standalone t'GI.Gtk.Objects.StyleContext.StyleContext', this style context
-- won’t be attached to any widget, so you may want
-- to call 'GI.Gtk.Objects.StyleContext.styleContextSetPath' yourself.
-- 
-- This function is only useful when using the theming layer
-- separated from GTK+, if you are using t'GI.Gtk.Objects.StyleContext.StyleContext' to
-- theme @/GtkWidgets/@, use 'GI.Gtk.Objects.Widget.widgetGetStyleContext'
-- in order to get a style context ready to theme the widget.
styleContextNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m StyleContext
    -- ^ __Returns:__ A newly created t'GI.Gtk.Objects.StyleContext.StyleContext'.
styleContextNew  = liftIO $ do
    result <- gtk_style_context_new
    checkUnexpectedReturnNULL "styleContextNew" result
    result' <- (wrapObject StyleContext) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method StyleContext::add_class
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "class_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class name to use in styling"
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

foreign import ccall "gtk_style_context_add_class" gtk_style_context_add_class :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- class_name : TBasicType TUTF8
    IO ()

-- | Adds a style class to /@context@/, so posterior calls to
-- @/gtk_style_context_get()/@ or any of the gtk_render_*()
-- functions will make use of this new class for styling.
-- 
-- In the CSS file format, a t'GI.Gtk.Objects.Entry.Entry' defining a “search”
-- class, would be matched by:
-- 
-- > <!-- language="CSS" -->
-- >entry.search { ... }
-- 
-- 
-- While any widget defining a “search” class would be
-- matched by:
-- > <!-- language="CSS" -->
-- >.search { ... }
-- 
-- 
-- /Since: 3.0/
styleContextAddClass ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@className@/: class name to use in styling
    -> m ()
styleContextAddClass context className = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    className' <- textToCString className
    gtk_style_context_add_class context' className'
    touchManagedPtr context
    freeMem className'
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextAddClassMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextAddClassMethodInfo a signature where
    overloadedMethod = styleContextAddClass

instance O.OverloadedMethodInfo StyleContextAddClassMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextAddClass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextAddClass"
        })


#endif

-- method StyleContext::add_provider
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleProvider"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "priority"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the priority of the style provider. The lower\n           it is, the earlier it will be used in the style\n           construction. Typically this will be in the range\n           between %GTK_STYLE_PROVIDER_PRIORITY_FALLBACK and\n           %GTK_STYLE_PROVIDER_PRIORITY_USER"
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

foreign import ccall "gtk_style_context_add_provider" gtk_style_context_add_provider :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gtk.StyleProvider.StyleProvider ->  -- provider : TInterface (Name {namespace = "Gtk", name = "StyleProvider"})
    Word32 ->                               -- priority : TBasicType TUInt
    IO ()

-- | Adds a style provider to /@context@/, to be used in style construction.
-- Note that a style provider added by this function only affects
-- the style of the widget to which /@context@/ belongs. If you want
-- to affect the style of all widgets, use
-- 'GI.Gtk.Objects.StyleContext.styleContextAddProviderForScreen'.
-- 
-- Note: If both priorities are the same, a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
-- added through this function takes precedence over another added
-- through 'GI.Gtk.Objects.StyleContext.styleContextAddProviderForScreen'.
-- 
-- /Since: 3.0/
styleContextAddProvider ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a, Gtk.StyleProvider.IsStyleProvider b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> b
    -- ^ /@provider@/: a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
    -> Word32
    -- ^ /@priority@/: the priority of the style provider. The lower
    --            it is, the earlier it will be used in the style
    --            construction. Typically this will be in the range
    --            between 'GI.Gtk.Constants.STYLE_PROVIDER_PRIORITY_FALLBACK' and
    --            'GI.Gtk.Constants.STYLE_PROVIDER_PRIORITY_USER'
    -> m ()
styleContextAddProvider context provider priority = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    provider' <- unsafeManagedPtrCastPtr provider
    gtk_style_context_add_provider context' provider' priority
    touchManagedPtr context
    touchManagedPtr provider
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextAddProviderMethodInfo
instance (signature ~ (b -> Word32 -> m ()), MonadIO m, IsStyleContext a, Gtk.StyleProvider.IsStyleProvider b) => O.OverloadedMethod StyleContextAddProviderMethodInfo a signature where
    overloadedMethod = styleContextAddProvider

instance O.OverloadedMethodInfo StyleContextAddProviderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextAddProvider",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextAddProvider"
        })


#endif

-- method StyleContext::add_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "region_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "region name to use in styling"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RegionFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "flags that apply to the region"
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

foreign import ccall "gtk_style_context_add_region" gtk_style_context_add_region :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- region_name : TBasicType TUTF8
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "RegionFlags"})
    IO ()

{-# DEPRECATED styleContextAddRegion ["(Since version 3.14)"] #-}
-- | Adds a region to /@context@/, so posterior calls to
-- @/gtk_style_context_get()/@ or any of the gtk_render_*()
-- functions will make use of this new region for styling.
-- 
-- In the CSS file format, a t'GI.Gtk.Objects.TreeView.TreeView' defining a “row”
-- region, would be matched by:
-- 
-- > <!-- language="CSS" -->
-- >treeview row { ... }
-- 
-- 
-- Pseudo-classes are used for matching /@flags@/, so the two
-- following rules:
-- > <!-- language="CSS" -->
-- >treeview row:nth-child(even) { ... }
-- >treeview row:nth-child(odd) { ... }
-- 
-- 
-- would apply to even and odd rows, respectively.
-- 
-- Region names must only contain lowercase letters
-- and “-”, starting always with a lowercase letter.
-- 
-- /Since: 3.0/
styleContextAddRegion ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@regionName@/: region name to use in styling
    -> [Gtk.Flags.RegionFlags]
    -- ^ /@flags@/: flags that apply to the region
    -> m ()
styleContextAddRegion context regionName flags = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    regionName' <- textToCString regionName
    let flags' = gflagsToWord flags
    gtk_style_context_add_region context' regionName' flags'
    touchManagedPtr context
    freeMem regionName'
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextAddRegionMethodInfo
instance (signature ~ (T.Text -> [Gtk.Flags.RegionFlags] -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextAddRegionMethodInfo a signature where
    overloadedMethod = styleContextAddRegion

instance O.OverloadedMethodInfo StyleContextAddRegionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextAddRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextAddRegion"
        })


#endif

-- method StyleContext::cancel_animations
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "region_id"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "animatable region to stop, or %NULL.\n    See gtk_style_context_push_animatable_region()"
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

foreign import ccall "gtk_style_context_cancel_animations" gtk_style_context_cancel_animations :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr () ->                               -- region_id : TBasicType TPtr
    IO ()

{-# DEPRECATED styleContextCancelAnimations ["(Since version 3.6)","This function does nothing."] #-}
-- | Stops all running animations for /@regionId@/ and all animatable
-- regions underneath.
-- 
-- A 'P.Nothing' /@regionId@/ will stop all ongoing animations in /@context@/,
-- when dealing with a t'GI.Gtk.Objects.StyleContext.StyleContext' obtained through
-- 'GI.Gtk.Objects.Widget.widgetGetStyleContext', this is normally done for you
-- in all circumstances you would expect all widget to be stopped,
-- so this should be only used in complex widgets with different
-- animatable regions.
-- 
-- /Since: 3.0/
styleContextCancelAnimations ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Ptr ()
    -- ^ /@regionId@/: animatable region to stop, or 'P.Nothing'.
    --     See 'GI.Gtk.Objects.StyleContext.styleContextPushAnimatableRegion'
    -> m ()
styleContextCancelAnimations context regionId = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_style_context_cancel_animations context' regionId
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextCancelAnimationsMethodInfo
instance (signature ~ (Ptr () -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextCancelAnimationsMethodInfo a signature where
    overloadedMethod = styleContextCancelAnimations

instance O.OverloadedMethodInfo StyleContextCancelAnimationsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextCancelAnimations",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextCancelAnimations"
        })


#endif

-- method StyleContext::get_background_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to retrieve the color for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the background color"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_background_color" gtk_style_context_get_background_color :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gdk.RGBA.RGBA ->                    -- color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

{-# DEPRECATED styleContextGetBackgroundColor ["(Since version 3.16)","Use 'GI.Gtk.Functions.renderBackground' instead."] #-}
-- | Gets the background color for a given state.
-- 
-- This function is far less useful than it seems, and it should not be used in
-- newly written code. CSS has no concept of \"background color\", as a background
-- can be an image, or a gradient, or any other pattern including solid colors.
-- 
-- The only reason why you would call 'GI.Gtk.Objects.StyleContext.styleContextGetBackgroundColor' is
-- to use the returned value to draw the background with it; the correct way to
-- achieve this result is to use 'GI.Gtk.Functions.renderBackground' instead, along with CSS
-- style classes to modify the color to be rendered.
-- 
-- /Since: 3.0/
styleContextGetBackgroundColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the color for
    -> m (Gdk.RGBA.RGBA)
styleContextGetBackgroundColor context state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let state' = gflagsToWord state
    color <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    gtk_style_context_get_background_color context' state' color
    color' <- (wrapBoxed Gdk.RGBA.RGBA) color
    touchManagedPtr context
    return color'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetBackgroundColorMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gdk.RGBA.RGBA)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetBackgroundColorMethodInfo a signature where
    overloadedMethod = styleContextGetBackgroundColor

instance O.OverloadedMethodInfo StyleContextGetBackgroundColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetBackgroundColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetBackgroundColor"
        })


#endif

-- method StyleContext::get_border
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to retrieve the border for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "border"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the border settings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_border" gtk_style_context_get_border :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gtk.Border.Border ->                -- border : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO ()

-- | Gets the border for a given state as a t'GI.Gtk.Structs.Border.Border'.
-- 
-- See 'GI.Gtk.Objects.StyleContext.styleContextGetProperty' and
-- 'GI.Gtk.Constants.STYLE_PROPERTY_BORDER_WIDTH' for details.
-- 
-- /Since: 3.0/
styleContextGetBorder ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the border for
    -> m (Gtk.Border.Border)
styleContextGetBorder context state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let state' = gflagsToWord state
    border <- SP.callocBoxedBytes 8 :: IO (Ptr Gtk.Border.Border)
    gtk_style_context_get_border context' state' border
    border' <- (wrapBoxed Gtk.Border.Border) border
    touchManagedPtr context
    return border'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetBorderMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gtk.Border.Border)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetBorderMethodInfo a signature where
    overloadedMethod = styleContextGetBorder

instance O.OverloadedMethodInfo StyleContextGetBorderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetBorder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetBorder"
        })


#endif

-- method StyleContext::get_border_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to retrieve the color for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the border color"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_border_color" gtk_style_context_get_border_color :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gdk.RGBA.RGBA ->                    -- color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

{-# DEPRECATED styleContextGetBorderColor ["(Since version 3.16)","Use 'GI.Gtk.Functions.renderFrame' instead."] #-}
-- | Gets the border color for a given state.
-- 
-- /Since: 3.0/
styleContextGetBorderColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the color for
    -> m (Gdk.RGBA.RGBA)
styleContextGetBorderColor context state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let state' = gflagsToWord state
    color <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    gtk_style_context_get_border_color context' state' color
    color' <- (wrapBoxed Gdk.RGBA.RGBA) color
    touchManagedPtr context
    return color'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetBorderColorMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gdk.RGBA.RGBA)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetBorderColorMethodInfo a signature where
    overloadedMethod = styleContextGetBorderColor

instance O.OverloadedMethodInfo StyleContextGetBorderColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetBorderColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetBorderColor"
        })


#endif

-- method StyleContext::get_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to retrieve the color for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the foreground color"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_color" gtk_style_context_get_color :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gdk.RGBA.RGBA ->                    -- color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

-- | Gets the foreground color for a given state.
-- 
-- See 'GI.Gtk.Objects.StyleContext.styleContextGetProperty' and
-- 'GI.Gtk.Constants.STYLE_PROPERTY_COLOR' for details.
-- 
-- /Since: 3.0/
styleContextGetColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the color for
    -> m (Gdk.RGBA.RGBA)
styleContextGetColor context state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let state' = gflagsToWord state
    color <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    gtk_style_context_get_color context' state' color
    color' <- (wrapBoxed Gdk.RGBA.RGBA) color
    touchManagedPtr context
    return color'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetColorMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gdk.RGBA.RGBA)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetColorMethodInfo a signature where
    overloadedMethod = styleContextGetColor

instance O.OverloadedMethodInfo StyleContextGetColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetColor"
        })


#endif

-- method StyleContext::get_direction
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TextDirection" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_direction" gtk_style_context_get_direction :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO CUInt

{-# DEPRECATED styleContextGetDirection ["(Since version 3.8)","Use 'GI.Gtk.Objects.StyleContext.styleContextGetState' and","  check for @/GTK_STATE_FLAG_DIR_LTR/@ and","  @/GTK_STATE_FLAG_DIR_RTL/@ instead."] #-}
-- | Returns the widget direction used for rendering.
-- 
-- /Since: 3.0/
styleContextGetDirection ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m Gtk.Enums.TextDirection
    -- ^ __Returns:__ the widget direction
styleContextGetDirection context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_get_direction context'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetDirectionMethodInfo
instance (signature ~ (m Gtk.Enums.TextDirection), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetDirectionMethodInfo a signature where
    overloadedMethod = styleContextGetDirection

instance O.OverloadedMethodInfo StyleContextGetDirectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetDirection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetDirection"
        })


#endif

-- method StyleContext::get_font
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to retrieve the font for"
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
--               (TInterface
--                  Name { namespace = "Pango" , name = "FontDescription" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_font" gtk_style_context_get_font :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    IO (Ptr Pango.FontDescription.FontDescription)

{-# DEPRECATED styleContextGetFont ["(Since version 3.8)","Use @/gtk_style_context_get()/@ for \\\"font\\\" or","    subproperties instead."] #-}
-- | Returns the font description for a given state. The returned
-- object is const and will remain valid until the
-- [StyleContext::changed]("GI.Gtk.Objects.StyleContext#g:signal:changed") signal happens.
-- 
-- /Since: 3.0/
styleContextGetFont ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the font for
    -> m Pango.FontDescription.FontDescription
    -- ^ __Returns:__ the t'GI.Pango.Structs.FontDescription.FontDescription' for the given
    --          state.  This object is owned by GTK+ and should not be
    --          freed.
styleContextGetFont context state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let state' = gflagsToWord state
    result <- gtk_style_context_get_font context' state'
    checkUnexpectedReturnNULL "styleContextGetFont" result
    result' <- (newBoxed Pango.FontDescription.FontDescription) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetFontMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m Pango.FontDescription.FontDescription), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetFontMethodInfo a signature where
    overloadedMethod = styleContextGetFont

instance O.OverloadedMethodInfo StyleContextGetFontMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetFont",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetFont"
        })


#endif

-- method StyleContext::get_frame_clock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "FrameClock" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_frame_clock" gtk_style_context_get_frame_clock :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO (Ptr Gdk.FrameClock.FrameClock)

-- | Returns the t'GI.Gdk.Objects.FrameClock.FrameClock' to which /@context@/ is attached.
-- 
-- /Since: 3.8/
styleContextGetFrameClock ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m (Maybe Gdk.FrameClock.FrameClock)
    -- ^ __Returns:__ a t'GI.Gdk.Objects.FrameClock.FrameClock', or 'P.Nothing'
    --  if /@context@/ does not have an attached frame clock.
styleContextGetFrameClock context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_get_frame_clock context'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gdk.FrameClock.FrameClock) result'
        return result''
    touchManagedPtr context
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StyleContextGetFrameClockMethodInfo
instance (signature ~ (m (Maybe Gdk.FrameClock.FrameClock)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetFrameClockMethodInfo a signature where
    overloadedMethod = styleContextGetFrameClock

instance O.OverloadedMethodInfo StyleContextGetFrameClockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetFrameClock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetFrameClock"
        })


#endif

-- method StyleContext::get_junction_sides
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "JunctionSides" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_junction_sides" gtk_style_context_get_junction_sides :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO CUInt

-- | Returns the sides where rendered elements connect visually with others.
-- 
-- /Since: 3.0/
styleContextGetJunctionSides ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m [Gtk.Flags.JunctionSides]
    -- ^ __Returns:__ the junction sides
styleContextGetJunctionSides context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_get_junction_sides context'
    let result' = wordToGFlags result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetJunctionSidesMethodInfo
instance (signature ~ (m [Gtk.Flags.JunctionSides]), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetJunctionSidesMethodInfo a signature where
    overloadedMethod = styleContextGetJunctionSides

instance O.OverloadedMethodInfo StyleContextGetJunctionSidesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetJunctionSides",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetJunctionSides"
        })


#endif

-- method StyleContext::get_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to retrieve the border for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "margin"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the margin settings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_margin" gtk_style_context_get_margin :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gtk.Border.Border ->                -- margin : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO ()

-- | Gets the margin for a given state as a t'GI.Gtk.Structs.Border.Border'.
-- See @/gtk_style_property_get()/@ and 'GI.Gtk.Constants.STYLE_PROPERTY_MARGIN'
-- for details.
-- 
-- /Since: 3.0/
styleContextGetMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the border for
    -> m (Gtk.Border.Border)
styleContextGetMargin context state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let state' = gflagsToWord state
    margin <- SP.callocBoxedBytes 8 :: IO (Ptr Gtk.Border.Border)
    gtk_style_context_get_margin context' state' margin
    margin' <- (wrapBoxed Gtk.Border.Border) margin
    touchManagedPtr context
    return margin'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetMarginMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gtk.Border.Border)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetMarginMethodInfo a signature where
    overloadedMethod = styleContextGetMargin

instance O.OverloadedMethodInfo StyleContextGetMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetMargin"
        })


#endif

-- method StyleContext::get_padding
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to retrieve the padding for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "padding"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the padding settings"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_padding" gtk_style_context_get_padding :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gtk.Border.Border ->                -- padding : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO ()

-- | Gets the padding for a given state as a t'GI.Gtk.Structs.Border.Border'.
-- See @/gtk_style_context_get()/@ and 'GI.Gtk.Constants.STYLE_PROPERTY_PADDING'
-- for details.
-- 
-- /Since: 3.0/
styleContextGetPadding ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the padding for
    -> m (Gtk.Border.Border)
styleContextGetPadding context state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let state' = gflagsToWord state
    padding <- SP.callocBoxedBytes 8 :: IO (Ptr Gtk.Border.Border)
    gtk_style_context_get_padding context' state' padding
    padding' <- (wrapBoxed Gtk.Border.Border) padding
    touchManagedPtr context
    return padding'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetPaddingMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gtk.Border.Border)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetPaddingMethodInfo a signature where
    overloadedMethod = styleContextGetPadding

instance O.OverloadedMethodInfo StyleContextGetPaddingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetPadding",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetPadding"
        })


#endif

-- method StyleContext::get_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "StyleContext" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_parent" gtk_style_context_get_parent :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO (Ptr StyleContext)

-- | Gets the parent context set via 'GI.Gtk.Objects.StyleContext.styleContextSetParent'.
-- See that function for details.
-- 
-- /Since: 3.4/
styleContextGetParent ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m (Maybe StyleContext)
    -- ^ __Returns:__ the parent context or 'P.Nothing'
styleContextGetParent context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_get_parent context'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject StyleContext) result'
        return result''
    touchManagedPtr context
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StyleContextGetParentMethodInfo
instance (signature ~ (m (Maybe StyleContext)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetParentMethodInfo a signature where
    overloadedMethod = styleContextGetParent

instance O.OverloadedMethodInfo StyleContextGetParentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetParent"
        })


#endif

-- method StyleContext::get_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WidgetPath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_path" gtk_style_context_get_path :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO (Ptr Gtk.WidgetPath.WidgetPath)

-- | Returns the widget path used for style matching.
-- 
-- /Since: 3.0/
styleContextGetPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m Gtk.WidgetPath.WidgetPath
    -- ^ __Returns:__ A t'GI.Gtk.Structs.WidgetPath.WidgetPath'
styleContextGetPath context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_get_path context'
    checkUnexpectedReturnNULL "styleContextGetPath" result
    result' <- (newBoxed Gtk.WidgetPath.WidgetPath) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetPathMethodInfo
instance (signature ~ (m Gtk.WidgetPath.WidgetPath), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetPathMethodInfo a signature where
    overloadedMethod = styleContextGetPath

instance O.OverloadedMethodInfo StyleContextGetPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetPath"
        })


#endif

-- method StyleContext::get_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "style property name"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to retrieve the property value for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the style property value"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_property" gtk_style_context_get_property :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- property : TBasicType TUTF8
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Gets a style property from /@context@/ for the given state.
-- 
-- Note that not all CSS properties that are supported by GTK+ can be
-- retrieved in this way, since they may not be representable as t'GI.GObject.Structs.Value.Value'.
-- GTK+ defines macros for a number of properties that can be used
-- with this function.
-- 
-- Note that passing a state other than the current state of /@context@/
-- is not recommended unless the style context has been saved with
-- 'GI.Gtk.Objects.StyleContext.styleContextSave'.
-- 
-- When /@value@/ is no longer needed, 'GI.GObject.Structs.Value.valueUnset' must be called
-- to free any allocated memory.
-- 
-- /Since: 3.0/
styleContextGetProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@property@/: style property name
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the property value for
    -> m (GValue)
styleContextGetProperty context property state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    property' <- textToCString property
    let state' = gflagsToWord state
    value <- SP.callocBytes 24 :: IO (Ptr GValue)
    gtk_style_context_get_property context' property' state' value
    value' <- B.GValue.wrapGValuePtr value
    touchManagedPtr context
    freeMem property'
    return value'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetPropertyMethodInfo
instance (signature ~ (T.Text -> [Gtk.Flags.StateFlags] -> m (GValue)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetPropertyMethodInfo a signature where
    overloadedMethod = styleContextGetProperty

instance O.OverloadedMethodInfo StyleContextGetPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetProperty"
        })


#endif

-- method StyleContext::get_scale
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_get_scale" gtk_style_context_get_scale :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO Int32

-- | Returns the scale used for assets.
-- 
-- /Since: 3.10/
styleContextGetScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m Int32
    -- ^ __Returns:__ the scale
styleContextGetScale context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_get_scale context'
    touchManagedPtr context
    return result

#if defined(ENABLE_OVERLOADING)
data StyleContextGetScaleMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetScaleMethodInfo a signature where
    overloadedMethod = styleContextGetScale

instance O.OverloadedMethodInfo StyleContextGetScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetScale"
        })


#endif

-- method StyleContext::get_screen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Screen" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_screen" gtk_style_context_get_screen :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO (Ptr Gdk.Screen.Screen)

-- | Returns the t'GI.Gdk.Objects.Screen.Screen' to which /@context@/ is attached.
styleContextGetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m Gdk.Screen.Screen
    -- ^ __Returns:__ a t'GI.Gdk.Objects.Screen.Screen'.
styleContextGetScreen context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_get_screen context'
    checkUnexpectedReturnNULL "styleContextGetScreen" result
    result' <- (newObject Gdk.Screen.Screen) result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetScreenMethodInfo
instance (signature ~ (m Gdk.Screen.Screen), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetScreenMethodInfo a signature where
    overloadedMethod = styleContextGetScreen

instance O.OverloadedMethodInfo StyleContextGetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetScreen"
        })


#endif

-- method StyleContext::get_section
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "style property name"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "CssSection" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_section" gtk_style_context_get_section :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- property : TBasicType TUTF8
    IO (Ptr Gtk.CssSection.CssSection)

-- | Queries the location in the CSS where /@property@/ was defined for the
-- current /@context@/. Note that the state to be queried is taken from
-- 'GI.Gtk.Objects.StyleContext.styleContextGetState'.
-- 
-- If the location is not available, 'P.Nothing' will be returned. The
-- location might not be available for various reasons, such as the
-- property being overridden, /@property@/ not naming a supported CSS
-- property or tracking of definitions being disabled for performance
-- reasons.
-- 
-- Shorthand CSS properties cannot be queried for a location and will
-- always return 'P.Nothing'.
styleContextGetSection ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@property@/: style property name
    -> m (Maybe Gtk.CssSection.CssSection)
    -- ^ __Returns:__ 'P.Nothing' or the section where a value
    -- for /@property@/ was defined
styleContextGetSection context property = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    property' <- textToCString property
    result <- gtk_style_context_get_section context' property'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed Gtk.CssSection.CssSection) result'
        return result''
    touchManagedPtr context
    freeMem property'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StyleContextGetSectionMethodInfo
instance (signature ~ (T.Text -> m (Maybe Gtk.CssSection.CssSection)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetSectionMethodInfo a signature where
    overloadedMethod = styleContextGetSection

instance O.OverloadedMethodInfo StyleContextGetSectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetSection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetSection"
        })


#endif

-- method StyleContext::get_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "StateFlags" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_get_state" gtk_style_context_get_state :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO CUInt

-- | Returns the state used for style matching.
-- 
-- This method should only be used to retrieve the t'GI.Gtk.Flags.StateFlags'
-- to pass to t'GI.Gtk.Objects.StyleContext.StyleContext' methods, like 'GI.Gtk.Objects.StyleContext.styleContextGetPadding'.
-- If you need to retrieve the current state of a t'GI.Gtk.Objects.Widget.Widget', use
-- 'GI.Gtk.Objects.Widget.widgetGetStateFlags'.
-- 
-- /Since: 3.0/
styleContextGetState ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m [Gtk.Flags.StateFlags]
    -- ^ __Returns:__ the state flags
styleContextGetState context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_get_state context'
    let result' = wordToGFlags result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleContextGetStateMethodInfo
instance (signature ~ (m [Gtk.Flags.StateFlags]), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetStateMethodInfo a signature where
    overloadedMethod = styleContextGetState

instance O.OverloadedMethodInfo StyleContextGetStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetState"
        })


#endif

-- method StyleContext::get_style_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the widget style property"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return location for the property value"
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

foreign import ccall "gtk_style_context_get_style_property" gtk_style_context_get_style_property :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- property_name : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Gets the value for a widget style property.
-- 
-- When /@value@/ is no longer needed, 'GI.GObject.Structs.Value.valueUnset' must be called
-- to free any allocated memory.
styleContextGetStyleProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@propertyName@/: the name of the widget style property
    -> GValue
    -- ^ /@value@/: Return location for the property value
    -> m ()
styleContextGetStyleProperty context propertyName value = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    propertyName' <- textToCString propertyName
    value' <- unsafeManagedPtrGetPtr value
    gtk_style_context_get_style_property context' propertyName' value'
    touchManagedPtr context
    touchManagedPtr value
    freeMem propertyName'
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextGetStylePropertyMethodInfo
instance (signature ~ (T.Text -> GValue -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextGetStylePropertyMethodInfo a signature where
    overloadedMethod = styleContextGetStyleProperty

instance O.OverloadedMethodInfo StyleContextGetStylePropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextGetStyleProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextGetStyleProperty"
        })


#endif

-- method StyleContext::has_class
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "class_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a class name" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_has_class" gtk_style_context_has_class :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- class_name : TBasicType TUTF8
    IO CInt

-- | Returns 'P.True' if /@context@/ currently has defined the
-- given class name.
-- 
-- /Since: 3.0/
styleContextHasClass ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@className@/: a class name
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@context@/ has /@className@/ defined
styleContextHasClass context className = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    className' <- textToCString className
    result <- gtk_style_context_has_class context' className'
    let result' = (/= 0) result
    touchManagedPtr context
    freeMem className'
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleContextHasClassMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextHasClassMethodInfo a signature where
    overloadedMethod = styleContextHasClass

instance O.OverloadedMethodInfo StyleContextHasClassMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextHasClass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextHasClass"
        })


#endif

-- method StyleContext::has_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "region_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a region name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags_return"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RegionFlags" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for region flags"
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

foreign import ccall "gtk_style_context_has_region" gtk_style_context_has_region :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- region_name : TBasicType TUTF8
    Ptr CUInt ->                            -- flags_return : TInterface (Name {namespace = "Gtk", name = "RegionFlags"})
    IO CInt

{-# DEPRECATED styleContextHasRegion ["(Since version 3.14)"] #-}
-- | Returns 'P.True' if /@context@/ has the region defined.
-- If /@flagsReturn@/ is not 'P.Nothing', it is set to the flags
-- affecting the region.
-- 
-- /Since: 3.0/
styleContextHasRegion ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@regionName@/: a region name
    -> m ((Bool, [Gtk.Flags.RegionFlags]))
    -- ^ __Returns:__ 'P.True' if region is defined
styleContextHasRegion context regionName = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    regionName' <- textToCString regionName
    flagsReturn <- allocMem :: IO (Ptr CUInt)
    result <- gtk_style_context_has_region context' regionName' flagsReturn
    let result' = (/= 0) result
    flagsReturn' <- peek flagsReturn
    let flagsReturn'' = wordToGFlags flagsReturn'
    touchManagedPtr context
    freeMem regionName'
    freeMem flagsReturn
    return (result', flagsReturn'')

#if defined(ENABLE_OVERLOADING)
data StyleContextHasRegionMethodInfo
instance (signature ~ (T.Text -> m ((Bool, [Gtk.Flags.RegionFlags]))), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextHasRegionMethodInfo a signature where
    overloadedMethod = styleContextHasRegion

instance O.OverloadedMethodInfo StyleContextHasRegionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextHasRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextHasRegion"
        })


#endif

-- method StyleContext::invalidate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext."
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

foreign import ccall "gtk_style_context_invalidate" gtk_style_context_invalidate :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO ()

{-# DEPRECATED styleContextInvalidate ["(Since version 3.12)","Style contexts are invalidated automatically."] #-}
-- | Invalidates /@context@/ style information, so it will be reconstructed
-- again. It is useful if you modify the /@context@/ and need the new
-- information immediately.
-- 
-- /Since: 3.0/
styleContextInvalidate ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'.
    -> m ()
styleContextInvalidate context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_style_context_invalidate context'
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextInvalidateMethodInfo
instance (signature ~ (m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextInvalidateMethodInfo a signature where
    overloadedMethod = styleContextInvalidate

instance O.OverloadedMethodInfo StyleContextInvalidateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextInvalidate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextInvalidate"
        })


#endif

-- method StyleContext::list_classes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TGList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_list_classes" gtk_style_context_list_classes :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO (Ptr (GList CString))

-- | Returns the list of classes currently defined in /@context@/.
-- 
-- /Since: 3.0/
styleContextListClasses ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m [T.Text]
    -- ^ __Returns:__ a t'GI.GLib.Structs.List.List' of
    --          strings with the currently defined classes. The contents
    --          of the list are owned by GTK+, but you must free the list
    --          itself with @/g_list_free()/@ when you are done with it.
styleContextListClasses context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_list_classes context'
    result' <- unpackGList result
    result'' <- mapM cstringToText result'
    g_list_free result
    touchManagedPtr context
    return result''

#if defined(ENABLE_OVERLOADING)
data StyleContextListClassesMethodInfo
instance (signature ~ (m [T.Text]), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextListClassesMethodInfo a signature where
    overloadedMethod = styleContextListClasses

instance O.OverloadedMethodInfo StyleContextListClassesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextListClasses",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextListClasses"
        })


#endif

-- method StyleContext::list_regions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TGList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_list_regions" gtk_style_context_list_regions :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO (Ptr (GList CString))

{-# DEPRECATED styleContextListRegions ["(Since version 3.14)"] #-}
-- | Returns the list of regions currently defined in /@context@/.
-- 
-- /Since: 3.0/
styleContextListRegions ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m [T.Text]
    -- ^ __Returns:__ a t'GI.GLib.Structs.List.List' of
    --          strings with the currently defined regions. The contents
    --          of the list are owned by GTK+, but you must free the list
    --          itself with @/g_list_free()/@ when you are done with it.
styleContextListRegions context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_style_context_list_regions context'
    result' <- unpackGList result
    result'' <- mapM cstringToText result'
    g_list_free result
    touchManagedPtr context
    return result''

#if defined(ENABLE_OVERLOADING)
data StyleContextListRegionsMethodInfo
instance (signature ~ (m [T.Text]), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextListRegionsMethodInfo a signature where
    overloadedMethod = styleContextListRegions

instance O.OverloadedMethodInfo StyleContextListRegionsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextListRegions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextListRegions"
        })


#endif

-- method StyleContext::lookup_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "color name to lookup"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return location for the looked up color"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_lookup_color" gtk_style_context_lookup_color :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- color_name : TBasicType TUTF8
    Ptr Gdk.RGBA.RGBA ->                    -- color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO CInt

-- | Looks up and resolves a color name in the /@context@/ color map.
styleContextLookupColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@colorName@/: color name to lookup
    -> m ((Bool, Gdk.RGBA.RGBA))
    -- ^ __Returns:__ 'P.True' if /@colorName@/ was found and resolved, 'P.False' otherwise
styleContextLookupColor context colorName = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    colorName' <- textToCString colorName
    color <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    result <- gtk_style_context_lookup_color context' colorName' color
    let result' = (/= 0) result
    color' <- (wrapBoxed Gdk.RGBA.RGBA) color
    touchManagedPtr context
    freeMem colorName'
    return (result', color')

#if defined(ENABLE_OVERLOADING)
data StyleContextLookupColorMethodInfo
instance (signature ~ (T.Text -> m ((Bool, Gdk.RGBA.RGBA))), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextLookupColorMethodInfo a signature where
    overloadedMethod = styleContextLookupColor

instance O.OverloadedMethodInfo StyleContextLookupColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextLookupColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextLookupColor"
        })


#endif

-- method StyleContext::lookup_icon_set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an icon name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconSet" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_context_lookup_icon_set" gtk_style_context_lookup_icon_set :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- stock_id : TBasicType TUTF8
    IO (Ptr Gtk.IconSet.IconSet)

{-# DEPRECATED styleContextLookupIconSet ["(Since version 3.10)","Use 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon' instead."] #-}
-- | Looks up /@stockId@/ in the icon factories associated to /@context@/ and
-- the default icon factory, returning an icon set if found, otherwise
-- 'P.Nothing'.
styleContextLookupIconSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@stockId@/: an icon name
    -> m (Maybe Gtk.IconSet.IconSet)
    -- ^ __Returns:__ The looked up @/GtkIconSet/@, or 'P.Nothing'
styleContextLookupIconSet context stockId = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    stockId' <- textToCString stockId
    result <- gtk_style_context_lookup_icon_set context' stockId'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed Gtk.IconSet.IconSet) result'
        return result''
    touchManagedPtr context
    freeMem stockId'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StyleContextLookupIconSetMethodInfo
instance (signature ~ (T.Text -> m (Maybe Gtk.IconSet.IconSet)), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextLookupIconSetMethodInfo a signature where
    overloadedMethod = styleContextLookupIconSet

instance O.OverloadedMethodInfo StyleContextLookupIconSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextLookupIconSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextLookupIconSet"
        })


#endif

-- method StyleContext::notify_state_change
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "region_id"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "animatable region to notify on, or %NULL.\n    See gtk_style_context_push_animatable_region()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to trigger transition for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_value"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if @state is the state we are changing to,\n    %FALSE if we are changing away from it"
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

foreign import ccall "gtk_style_context_notify_state_change" gtk_style_context_notify_state_change :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    Ptr () ->                               -- region_id : TBasicType TPtr
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateType"})
    CInt ->                                 -- state_value : TBasicType TBoolean
    IO ()

{-# DEPRECATED styleContextNotifyStateChange ["(Since version 3.6)","This function does nothing."] #-}
-- | Notifies a state change on /@context@/, so if the current style makes use
-- of transition animations, one will be started so all rendered elements
-- under /@regionId@/ are animated for state /@state@/ being set to value
-- /@stateValue@/.
-- 
-- The /@window@/ parameter is used in order to invalidate the rendered area
-- as the animation runs, so make sure it is the same window that is being
-- rendered on by the gtk_render_*() functions.
-- 
-- If /@regionId@/ is 'P.Nothing', all rendered elements using /@context@/ will be
-- affected by this state transition.
-- 
-- As a practical example, a t'GI.Gtk.Objects.Button.Button' notifying a state transition on
-- the prelight state:
-- > <!-- language="C" -->
-- >gtk_style_context_notify_state_change (context,
-- >                                       gtk_widget_get_window (widget),
-- >                                       NULL,
-- >                                       GTK_STATE_PRELIGHT,
-- >                                       button->in_button);
-- 
-- 
-- Can be handled in the CSS file like this:
-- > <!-- language="CSS" -->
-- >button {
-- >    background-color: #f00
-- >}
-- >
-- >button:hover {
-- >    background-color: #fff;
-- >    transition: 200ms linear
-- >}
-- 
-- 
-- This combination will animate the button background from red to white
-- if a pointer enters the button, and back to red if the pointer leaves
-- the button.
-- 
-- Note that /@state@/ is used when finding the transition parameters, which
-- is why the style places the transition under the :hover pseudo-class.
-- 
-- /Since: 3.0/
styleContextNotifyStateChange ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> b
    -- ^ /@window@/: a t'GI.Gdk.Objects.Window.Window'
    -> Ptr ()
    -- ^ /@regionId@/: animatable region to notify on, or 'P.Nothing'.
    --     See 'GI.Gtk.Objects.StyleContext.styleContextPushAnimatableRegion'
    -> Gtk.Enums.StateType
    -- ^ /@state@/: state to trigger transition for
    -> Bool
    -- ^ /@stateValue@/: 'P.True' if /@state@/ is the state we are changing to,
    --     'P.False' if we are changing away from it
    -> m ()
styleContextNotifyStateChange context window regionId state stateValue = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    window' <- unsafeManagedPtrCastPtr window
    let state' = (fromIntegral . fromEnum) state
    let stateValue' = (fromIntegral . fromEnum) stateValue
    gtk_style_context_notify_state_change context' window' regionId state' stateValue'
    touchManagedPtr context
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextNotifyStateChangeMethodInfo
instance (signature ~ (b -> Ptr () -> Gtk.Enums.StateType -> Bool -> m ()), MonadIO m, IsStyleContext a, Gdk.Window.IsWindow b) => O.OverloadedMethod StyleContextNotifyStateChangeMethodInfo a signature where
    overloadedMethod = styleContextNotifyStateChange

instance O.OverloadedMethodInfo StyleContextNotifyStateChangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextNotifyStateChange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextNotifyStateChange"
        })


#endif

-- method StyleContext::pop_animatable_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_pop_animatable_region" gtk_style_context_pop_animatable_region :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO ()

{-# DEPRECATED styleContextPopAnimatableRegion ["(Since version 3.6)","This function does nothing."] #-}
-- | Pops an animatable region from /@context@/.
-- See 'GI.Gtk.Objects.StyleContext.styleContextPushAnimatableRegion'.
-- 
-- /Since: 3.0/
styleContextPopAnimatableRegion ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m ()
styleContextPopAnimatableRegion context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_style_context_pop_animatable_region context'
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextPopAnimatableRegionMethodInfo
instance (signature ~ (m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextPopAnimatableRegionMethodInfo a signature where
    overloadedMethod = styleContextPopAnimatableRegion

instance O.OverloadedMethodInfo StyleContextPopAnimatableRegionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextPopAnimatableRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextPopAnimatableRegion"
        })


#endif

-- method StyleContext::push_animatable_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "region_id"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "unique identifier for the animatable region"
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

foreign import ccall "gtk_style_context_push_animatable_region" gtk_style_context_push_animatable_region :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr () ->                               -- region_id : TBasicType TPtr
    IO ()

{-# DEPRECATED styleContextPushAnimatableRegion ["(Since version 3.6)","This function does nothing."] #-}
-- | Pushes an animatable region, so all further gtk_render_*() calls between
-- this call and the following 'GI.Gtk.Objects.StyleContext.styleContextPopAnimatableRegion'
-- will potentially show transition animations for this region if
-- 'GI.Gtk.Objects.StyleContext.styleContextNotifyStateChange' is called for a given state,
-- and the current theme\/style defines transition animations for state
-- changes.
-- 
-- The /@regionId@/ used must be unique in /@context@/ so the themes
-- can uniquely identify rendered elements subject to a state transition.
-- 
-- /Since: 3.0/
styleContextPushAnimatableRegion ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Ptr ()
    -- ^ /@regionId@/: unique identifier for the animatable region
    -> m ()
styleContextPushAnimatableRegion context regionId = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_style_context_push_animatable_region context' regionId
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextPushAnimatableRegionMethodInfo
instance (signature ~ (Ptr () -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextPushAnimatableRegionMethodInfo a signature where
    overloadedMethod = styleContextPushAnimatableRegion

instance O.OverloadedMethodInfo StyleContextPushAnimatableRegionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextPushAnimatableRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextPushAnimatableRegion"
        })


#endif

-- method StyleContext::remove_class
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "class_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class name to remove"
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

foreign import ccall "gtk_style_context_remove_class" gtk_style_context_remove_class :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- class_name : TBasicType TUTF8
    IO ()

-- | Removes /@className@/ from /@context@/.
-- 
-- /Since: 3.0/
styleContextRemoveClass ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@className@/: class name to remove
    -> m ()
styleContextRemoveClass context className = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    className' <- textToCString className
    gtk_style_context_remove_class context' className'
    touchManagedPtr context
    freeMem className'
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextRemoveClassMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextRemoveClassMethodInfo a signature where
    overloadedMethod = styleContextRemoveClass

instance O.OverloadedMethodInfo StyleContextRemoveClassMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextRemoveClass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextRemoveClass"
        })


#endif

-- method StyleContext::remove_provider
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleProvider"
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

foreign import ccall "gtk_style_context_remove_provider" gtk_style_context_remove_provider :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gtk.StyleProvider.StyleProvider ->  -- provider : TInterface (Name {namespace = "Gtk", name = "StyleProvider"})
    IO ()

-- | Removes /@provider@/ from the style providers list in /@context@/.
-- 
-- /Since: 3.0/
styleContextRemoveProvider ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a, Gtk.StyleProvider.IsStyleProvider b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> b
    -- ^ /@provider@/: a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
    -> m ()
styleContextRemoveProvider context provider = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    provider' <- unsafeManagedPtrCastPtr provider
    gtk_style_context_remove_provider context' provider'
    touchManagedPtr context
    touchManagedPtr provider
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextRemoveProviderMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsStyleContext a, Gtk.StyleProvider.IsStyleProvider b) => O.OverloadedMethod StyleContextRemoveProviderMethodInfo a signature where
    overloadedMethod = styleContextRemoveProvider

instance O.OverloadedMethodInfo StyleContextRemoveProviderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextRemoveProvider",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextRemoveProvider"
        })


#endif

-- method StyleContext::remove_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "region_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "region name to unset"
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

foreign import ccall "gtk_style_context_remove_region" gtk_style_context_remove_region :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CString ->                              -- region_name : TBasicType TUTF8
    IO ()

{-# DEPRECATED styleContextRemoveRegion ["(Since version 3.14)"] #-}
-- | Removes a region from /@context@/.
-- 
-- /Since: 3.0/
styleContextRemoveRegion ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> T.Text
    -- ^ /@regionName@/: region name to unset
    -> m ()
styleContextRemoveRegion context regionName = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    regionName' <- textToCString regionName
    gtk_style_context_remove_region context' regionName'
    touchManagedPtr context
    freeMem regionName'
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextRemoveRegionMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextRemoveRegionMethodInfo a signature where
    overloadedMethod = styleContextRemoveRegion

instance O.OverloadedMethodInfo StyleContextRemoveRegionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextRemoveRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextRemoveRegion"
        })


#endif

-- method StyleContext::restore
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_restore" gtk_style_context_restore :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO ()

-- | Restores /@context@/ state to a previous stage.
-- See 'GI.Gtk.Objects.StyleContext.styleContextSave'.
-- 
-- /Since: 3.0/
styleContextRestore ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m ()
styleContextRestore context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_style_context_restore context'
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextRestoreMethodInfo
instance (signature ~ (m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextRestoreMethodInfo a signature where
    overloadedMethod = styleContextRestore

instance O.OverloadedMethodInfo StyleContextRestoreMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextRestore",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextRestore"
        })


#endif

-- method StyleContext::save
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_save" gtk_style_context_save :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO ()

-- | Saves the /@context@/ state, so temporary modifications done through
-- 'GI.Gtk.Objects.StyleContext.styleContextAddClass', 'GI.Gtk.Objects.StyleContext.styleContextRemoveClass',
-- 'GI.Gtk.Objects.StyleContext.styleContextSetState', etc. can quickly be reverted
-- in one go through 'GI.Gtk.Objects.StyleContext.styleContextRestore'.
-- 
-- The matching call to 'GI.Gtk.Objects.StyleContext.styleContextRestore' must be done
-- before GTK returns to the main loop.
-- 
-- /Since: 3.0/
styleContextSave ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m ()
styleContextSave context = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_style_context_save context'
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSaveMethodInfo
instance (signature ~ (m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextSaveMethodInfo a signature where
    overloadedMethod = styleContextSave

instance O.OverloadedMethodInfo StyleContextSaveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSave",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSave"
        })


#endif

-- method StyleContext::scroll_animations
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkWindow used previously in\n         gtk_style_context_notify_state_change()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dx"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Amount to scroll in the X axis"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dy"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Amount to scroll in the Y axis"
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

foreign import ccall "gtk_style_context_scroll_animations" gtk_style_context_scroll_animations :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    Int32 ->                                -- dx : TBasicType TInt
    Int32 ->                                -- dy : TBasicType TInt
    IO ()

{-# DEPRECATED styleContextScrollAnimations ["(Since version 3.6)","This function does nothing."] #-}
-- | This function is analogous to 'GI.Gdk.Objects.Window.windowScroll', and
-- should be called together with it so the invalidation
-- areas for any ongoing animation are scrolled together
-- with it.
-- 
-- /Since: 3.0/
styleContextScrollAnimations ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> b
    -- ^ /@window@/: a t'GI.Gdk.Objects.Window.Window' used previously in
    --          'GI.Gtk.Objects.StyleContext.styleContextNotifyStateChange'
    -> Int32
    -- ^ /@dx@/: Amount to scroll in the X axis
    -> Int32
    -- ^ /@dy@/: Amount to scroll in the Y axis
    -> m ()
styleContextScrollAnimations context window dx dy = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    window' <- unsafeManagedPtrCastPtr window
    gtk_style_context_scroll_animations context' window' dx dy
    touchManagedPtr context
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextScrollAnimationsMethodInfo
instance (signature ~ (b -> Int32 -> Int32 -> m ()), MonadIO m, IsStyleContext a, Gdk.Window.IsWindow b) => O.OverloadedMethod StyleContextScrollAnimationsMethodInfo a signature where
    overloadedMethod = styleContextScrollAnimations

instance O.OverloadedMethodInfo StyleContextScrollAnimationsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextScrollAnimations",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextScrollAnimations"
        })


#endif

-- method StyleContext::set_background
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_set_background" gtk_style_context_set_background :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    IO ()

{-# DEPRECATED styleContextSetBackground ["(Since version 3.18)","Use 'GI.Gtk.Functions.renderBackground' instead.","  Note that clients still using this function are now responsible","  for calling this function again whenever /@context@/ is invalidated."] #-}
-- | Sets the background of /@window@/ to the background pattern or
-- color specified in /@context@/ for its current state.
-- 
-- /Since: 3.0/
styleContextSetBackground ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> b
    -- ^ /@window@/: a t'GI.Gdk.Objects.Window.Window'
    -> m ()
styleContextSetBackground context window = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    window' <- unsafeManagedPtrCastPtr window
    gtk_style_context_set_background context' window'
    touchManagedPtr context
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetBackgroundMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsStyleContext a, Gdk.Window.IsWindow b) => O.OverloadedMethod StyleContextSetBackgroundMethodInfo a signature where
    overloadedMethod = styleContextSetBackground

instance O.OverloadedMethodInfo StyleContextSetBackgroundMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetBackground",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetBackground"
        })


#endif

-- method StyleContext::set_direction
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "direction"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextDirection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new direction." , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_set_direction" gtk_style_context_set_direction :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- direction : TInterface (Name {namespace = "Gtk", name = "TextDirection"})
    IO ()

{-# DEPRECATED styleContextSetDirection ["(Since version 3.8)","Use 'GI.Gtk.Objects.StyleContext.styleContextSetState' with","  @/GTK_STATE_FLAG_DIR_LTR/@ and @/GTK_STATE_FLAG_DIR_RTL/@","  instead."] #-}
-- | Sets the reading direction for rendering purposes.
-- 
-- If you are using a t'GI.Gtk.Objects.StyleContext.StyleContext' returned from
-- 'GI.Gtk.Objects.Widget.widgetGetStyleContext', you do not need to
-- call this yourself.
-- 
-- /Since: 3.0/
styleContextSetDirection ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Gtk.Enums.TextDirection
    -- ^ /@direction@/: the new direction.
    -> m ()
styleContextSetDirection context direction = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let direction' = (fromIntegral . fromEnum) direction
    gtk_style_context_set_direction context' direction'
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetDirectionMethodInfo
instance (signature ~ (Gtk.Enums.TextDirection -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextSetDirectionMethodInfo a signature where
    overloadedMethod = styleContextSetDirection

instance O.OverloadedMethodInfo StyleContextSetDirectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetDirection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetDirection"
        })


#endif

-- method StyleContext::set_frame_clock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkFrameClock" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "frame_clock"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "FrameClock" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkFrameClock" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_set_frame_clock" gtk_style_context_set_frame_clock :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gdk.FrameClock.FrameClock ->        -- frame_clock : TInterface (Name {namespace = "Gdk", name = "FrameClock"})
    IO ()

-- | Attaches /@context@/ to the given frame clock.
-- 
-- The frame clock is used for the timing of animations.
-- 
-- If you are using a t'GI.Gtk.Objects.StyleContext.StyleContext' returned from
-- 'GI.Gtk.Objects.Widget.widgetGetStyleContext', you do not need to
-- call this yourself.
-- 
-- /Since: 3.8/
styleContextSetFrameClock ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a, Gdk.FrameClock.IsFrameClock b) =>
    a
    -- ^ /@context@/: a t'GI.Gdk.Objects.FrameClock.FrameClock'
    -> b
    -- ^ /@frameClock@/: a t'GI.Gdk.Objects.FrameClock.FrameClock'
    -> m ()
styleContextSetFrameClock context frameClock = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    frameClock' <- unsafeManagedPtrCastPtr frameClock
    gtk_style_context_set_frame_clock context' frameClock'
    touchManagedPtr context
    touchManagedPtr frameClock
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetFrameClockMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsStyleContext a, Gdk.FrameClock.IsFrameClock b) => O.OverloadedMethod StyleContextSetFrameClockMethodInfo a signature where
    overloadedMethod = styleContextSetFrameClock

instance O.OverloadedMethodInfo StyleContextSetFrameClockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetFrameClock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetFrameClock"
        })


#endif

-- method StyleContext::set_junction_sides
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sides"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "JunctionSides" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "sides where rendered elements are visually connected to\n    other elements"
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

foreign import ccall "gtk_style_context_set_junction_sides" gtk_style_context_set_junction_sides :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- sides : TInterface (Name {namespace = "Gtk", name = "JunctionSides"})
    IO ()

-- | Sets the sides where rendered elements (mostly through
-- 'GI.Gtk.Functions.renderFrame') will visually connect with other visual elements.
-- 
-- This is merely a hint that may or may not be honored
-- by themes.
-- 
-- Container widgets are expected to set junction hints as appropriate
-- for their children, so it should not normally be necessary to call
-- this function manually.
-- 
-- /Since: 3.0/
styleContextSetJunctionSides ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.JunctionSides]
    -- ^ /@sides@/: sides where rendered elements are visually connected to
    --     other elements
    -> m ()
styleContextSetJunctionSides context sides = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let sides' = gflagsToWord sides
    gtk_style_context_set_junction_sides context' sides'
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetJunctionSidesMethodInfo
instance (signature ~ ([Gtk.Flags.JunctionSides] -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextSetJunctionSidesMethodInfo a signature where
    overloadedMethod = styleContextSetJunctionSides

instance O.OverloadedMethodInfo StyleContextSetJunctionSidesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetJunctionSides",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetJunctionSides"
        })


#endif

-- method StyleContext::set_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new parent or %NULL"
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

foreign import ccall "gtk_style_context_set_parent" gtk_style_context_set_parent :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr StyleContext ->                     -- parent : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO ()

-- | Sets the parent style context for /@context@/. The parent style
-- context is used to implement
-- <http://www.w3.org/TR/css3-cascade/#inheritance inheritance>
-- of properties.
-- 
-- If you are using a t'GI.Gtk.Objects.StyleContext.StyleContext' returned from
-- 'GI.Gtk.Objects.Widget.widgetGetStyleContext', the parent will be set for you.
-- 
-- /Since: 3.4/
styleContextSetParent ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a, IsStyleContext b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Maybe (b)
    -- ^ /@parent@/: the new parent or 'P.Nothing'
    -> m ()
styleContextSetParent context parent = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    gtk_style_context_set_parent context' maybeParent
    touchManagedPtr context
    whenJust parent touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetParentMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsStyleContext a, IsStyleContext b) => O.OverloadedMethod StyleContextSetParentMethodInfo a signature where
    overloadedMethod = styleContextSetParent

instance O.OverloadedMethodInfo StyleContextSetParentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetParent"
        })


#endif

-- method StyleContext::set_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetPath" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_set_path" gtk_style_context_set_path :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gtk.WidgetPath.WidgetPath ->        -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO ()

-- | Sets the t'GI.Gtk.Structs.WidgetPath.WidgetPath' used for style matching. As a
-- consequence, the style will be regenerated to match
-- the new given path.
-- 
-- If you are using a t'GI.Gtk.Objects.StyleContext.StyleContext' returned from
-- 'GI.Gtk.Objects.Widget.widgetGetStyleContext', you do not need to call
-- this yourself.
-- 
-- /Since: 3.0/
styleContextSetPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Gtk.WidgetPath.WidgetPath
    -- ^ /@path@/: a t'GI.Gtk.Structs.WidgetPath.WidgetPath'
    -> m ()
styleContextSetPath context path = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    path' <- unsafeManagedPtrGetPtr path
    gtk_style_context_set_path context' path'
    touchManagedPtr context
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetPathMethodInfo
instance (signature ~ (Gtk.WidgetPath.WidgetPath -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextSetPathMethodInfo a signature where
    overloadedMethod = styleContextSetPath

instance O.OverloadedMethodInfo StyleContextSetPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetPath"
        })


#endif

-- method StyleContext::set_scale
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scale"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "scale" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_set_scale" gtk_style_context_set_scale :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Int32 ->                                -- scale : TBasicType TInt
    IO ()

-- | Sets the scale to use when getting image assets for the style.
-- 
-- /Since: 3.10/
styleContextSetScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Int32
    -- ^ /@scale@/: scale
    -> m ()
styleContextSetScale context scale = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    gtk_style_context_set_scale context' scale
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetScaleMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextSetScaleMethodInfo a signature where
    overloadedMethod = styleContextSetScale

instance O.OverloadedMethodInfo StyleContextSetScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetScale"
        })


#endif

-- method StyleContext::set_screen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_set_screen" gtk_style_context_set_screen :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO ()

-- | Attaches /@context@/ to the given screen.
-- 
-- The screen is used to add style information from “global” style
-- providers, such as the screen’s t'GI.Gtk.Objects.Settings.Settings' instance.
-- 
-- If you are using a t'GI.Gtk.Objects.StyleContext.StyleContext' returned from
-- 'GI.Gtk.Objects.Widget.widgetGetStyleContext', you do not need to
-- call this yourself.
-- 
-- /Since: 3.0/
styleContextSetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a, Gdk.Screen.IsScreen b) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> b
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'
    -> m ()
styleContextSetScreen context screen = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    screen' <- unsafeManagedPtrCastPtr screen
    gtk_style_context_set_screen context' screen'
    touchManagedPtr context
    touchManagedPtr screen
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetScreenMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsStyleContext a, Gdk.Screen.IsScreen b) => O.OverloadedMethod StyleContextSetScreenMethodInfo a signature where
    overloadedMethod = styleContextSetScreen

instance O.OverloadedMethodInfo StyleContextSetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetScreen"
        })


#endif

-- method StyleContext::set_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to represent" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_set_state" gtk_style_context_set_state :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    IO ()

-- | Sets the state to be used for style matching.
-- 
-- /Since: 3.0/
styleContextSetState ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@flags@/: state to represent
    -> m ()
styleContextSetState context flags = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let flags' = gflagsToWord flags
    gtk_style_context_set_state context' flags'
    touchManagedPtr context
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleContextSetStateMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m ()), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextSetStateMethodInfo a signature where
    overloadedMethod = styleContextSetState

instance O.OverloadedMethodInfo StyleContextSetStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextSetState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextSetState"
        })


#endif

-- method StyleContext::state_is_running
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a widget state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "progress"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the transition progress"
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

foreign import ccall "gtk_style_context_state_is_running" gtk_style_context_state_is_running :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr CDouble ->                          -- progress : TBasicType TDouble
    IO CInt

{-# DEPRECATED styleContextStateIsRunning ["(Since version 3.6)","This function always returns 'P.False'"] #-}
-- | Returns 'P.True' if there is a transition animation running for the
-- current region (see 'GI.Gtk.Objects.StyleContext.styleContextPushAnimatableRegion').
-- 
-- If /@progress@/ is not 'P.Nothing', the animation progress will be returned
-- there, 0.0 means the state is closest to being unset, while 1.0 means
-- it’s closest to being set. This means transition animation will
-- run from 0 to 1 when /@state@/ is being set and from 1 to 0 when
-- it’s being unset.
-- 
-- /Since: 3.0/
styleContextStateIsRunning ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Gtk.Enums.StateType
    -- ^ /@state@/: a widget state
    -> m ((Bool, Double))
    -- ^ __Returns:__ 'P.True' if there is a running transition animation for /@state@/.
styleContextStateIsRunning context state = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let state' = (fromIntegral . fromEnum) state
    progress <- allocMem :: IO (Ptr CDouble)
    result <- gtk_style_context_state_is_running context' state' progress
    let result' = (/= 0) result
    progress' <- peek progress
    let progress'' = realToFrac progress'
    touchManagedPtr context
    freeMem progress
    return (result', progress'')

#if defined(ENABLE_OVERLOADING)
data StyleContextStateIsRunningMethodInfo
instance (signature ~ (Gtk.Enums.StateType -> m ((Bool, Double))), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextStateIsRunningMethodInfo a signature where
    overloadedMethod = styleContextStateIsRunning

instance O.OverloadedMethodInfo StyleContextStateIsRunningMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextStateIsRunning",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextStateIsRunning"
        })


#endif

-- method StyleContext::to_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "StyleContextPrintFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Flags that determine what to print"
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

foreign import ccall "gtk_style_context_to_string" gtk_style_context_to_string :: 
    Ptr StyleContext ->                     -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "StyleContextPrintFlags"})
    IO CString

-- | Converts the style context into a string representation.
-- 
-- The string representation always includes information about
-- the name, state, id, visibility and style classes of the CSS
-- node that is backing /@context@/. Depending on the flags, more
-- information may be included.
-- 
-- This function is intended for testing and debugging of the
-- CSS implementation in GTK+. There are no guarantees about
-- the format of the returned string, it may change.
-- 
-- /Since: 3.20/
styleContextToString ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleContext a) =>
    a
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> [Gtk.Flags.StyleContextPrintFlags]
    -- ^ /@flags@/: Flags that determine what to print
    -> m T.Text
    -- ^ __Returns:__ a newly allocated string representing /@context@/
styleContextToString context flags = liftIO $ do
    context' <- unsafeManagedPtrCastPtr context
    let flags' = gflagsToWord flags
    result <- gtk_style_context_to_string context' flags'
    checkUnexpectedReturnNULL "styleContextToString" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleContextToStringMethodInfo
instance (signature ~ ([Gtk.Flags.StyleContextPrintFlags] -> m T.Text), MonadIO m, IsStyleContext a) => O.OverloadedMethod StyleContextToStringMethodInfo a signature where
    overloadedMethod = styleContextToString

instance O.OverloadedMethodInfo StyleContextToStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.StyleContext.styleContextToString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-StyleContext.html#v:styleContextToString"
        })


#endif

-- method StyleContext::add_provider_for_screen
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleProvider"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "priority"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the priority of the style provider. The lower\n           it is, the earlier it will be used in the style\n           construction. Typically this will be in the range\n           between %GTK_STYLE_PROVIDER_PRIORITY_FALLBACK and\n           %GTK_STYLE_PROVIDER_PRIORITY_USER"
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

foreign import ccall "gtk_style_context_add_provider_for_screen" gtk_style_context_add_provider_for_screen :: 
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    Ptr Gtk.StyleProvider.StyleProvider ->  -- provider : TInterface (Name {namespace = "Gtk", name = "StyleProvider"})
    Word32 ->                               -- priority : TBasicType TUInt
    IO ()

-- | Adds a global style provider to /@screen@/, which will be used
-- in style construction for all @/GtkStyleContexts/@ under /@screen@/.
-- 
-- GTK+ uses this to make styling information from t'GI.Gtk.Objects.Settings.Settings'
-- available.
-- 
-- Note: If both priorities are the same, A t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
-- added through 'GI.Gtk.Objects.StyleContext.styleContextAddProvider' takes precedence
-- over another added through this function.
-- 
-- /Since: 3.0/
styleContextAddProviderForScreen ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Screen.IsScreen a, Gtk.StyleProvider.IsStyleProvider b) =>
    a
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'
    -> b
    -- ^ /@provider@/: a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
    -> Word32
    -- ^ /@priority@/: the priority of the style provider. The lower
    --            it is, the earlier it will be used in the style
    --            construction. Typically this will be in the range
    --            between 'GI.Gtk.Constants.STYLE_PROVIDER_PRIORITY_FALLBACK' and
    --            'GI.Gtk.Constants.STYLE_PROVIDER_PRIORITY_USER'
    -> m ()
styleContextAddProviderForScreen screen provider priority = liftIO $ do
    screen' <- unsafeManagedPtrCastPtr screen
    provider' <- unsafeManagedPtrCastPtr provider
    gtk_style_context_add_provider_for_screen screen' provider' priority
    touchManagedPtr screen
    touchManagedPtr provider
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method StyleContext::remove_provider_for_screen
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleProvider"
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

foreign import ccall "gtk_style_context_remove_provider_for_screen" gtk_style_context_remove_provider_for_screen :: 
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    Ptr Gtk.StyleProvider.StyleProvider ->  -- provider : TInterface (Name {namespace = "Gtk", name = "StyleProvider"})
    IO ()

-- | Removes /@provider@/ from the global style providers list in /@screen@/.
-- 
-- /Since: 3.0/
styleContextRemoveProviderForScreen ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Screen.IsScreen a, Gtk.StyleProvider.IsStyleProvider b) =>
    a
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'
    -> b
    -- ^ /@provider@/: a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
    -> m ()
styleContextRemoveProviderForScreen screen provider = liftIO $ do
    screen' <- unsafeManagedPtrCastPtr screen
    provider' <- unsafeManagedPtrCastPtr provider
    gtk_style_context_remove_provider_for_screen screen' provider'
    touchManagedPtr screen
    touchManagedPtr provider
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method StyleContext::reset_widgets
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_context_reset_widgets" gtk_style_context_reset_widgets :: 
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO ()

-- | This function recomputes the styles for all widgets under a particular
-- t'GI.Gdk.Objects.Screen.Screen'. This is useful when some global parameter has changed that
-- affects the appearance of all widgets, because when a widget gets a new
-- style, it will both redraw and recompute any cached information about
-- its appearance. As an example, it is used when the color scheme changes
-- in the related t'GI.Gtk.Objects.Settings.Settings' object.
-- 
-- /Since: 3.0/
styleContextResetWidgets ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Screen.IsScreen a) =>
    a
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'
    -> m ()
styleContextResetWidgets screen = liftIO $ do
    screen' <- unsafeManagedPtrCastPtr screen
    gtk_style_context_reset_widgets screen'
    touchManagedPtr screen
    return ()

#if defined(ENABLE_OVERLOADING)
#endif


