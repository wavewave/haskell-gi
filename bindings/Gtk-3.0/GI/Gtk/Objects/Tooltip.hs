{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Basic tooltips can be realized simply by using 'GI.Gtk.Objects.Widget.widgetSetTooltipText'
-- or 'GI.Gtk.Objects.Widget.widgetSetTooltipMarkup' without any explicit tooltip object.
-- 
-- When you need a tooltip with a little more fancy contents, like adding an
-- image, or you want the tooltip to have different contents per t'GI.Gtk.Objects.TreeView.TreeView'
-- row or cell, you will have to do a little more work:
-- 
-- * Set the [Widget:hasTooltip]("GI.Gtk.Objects.Widget#g:attr:hasTooltip") property to 'P.True', this will make GTK+
-- monitor the widget for motion and related events which are needed to
-- determine when and where to show a tooltip.
-- * Connect to the [Widget::queryTooltip]("GI.Gtk.Objects.Widget#g:signal:queryTooltip") signal.  This signal will be
-- emitted when a tooltip is supposed to be shown. One of the arguments passed
-- to the signal handler is a GtkTooltip object. This is the object that we
-- are about to display as a tooltip, and can be manipulated in your callback
-- using functions like 'GI.Gtk.Objects.Tooltip.tooltipSetIcon'. There are functions for setting
-- the tooltip’s markup, setting an image from a named icon, or even putting in
-- a custom widget.
-- 
-- 
--   Return 'P.True' from your query-tooltip handler. This causes the tooltip to be
--   show. If you return 'P.False', it will not be shown.
-- 
-- In the probably rare case where you want to have even more control over the
-- tooltip that is about to be shown, you can set your own t'GI.Gtk.Objects.Window.Window' which
-- will be used as tooltip window.  This works as follows:
-- 
-- * Set [Widget:hasTooltip]("GI.Gtk.Objects.Widget#g:attr:hasTooltip") and connect to [Widget::queryTooltip]("GI.Gtk.Objects.Widget#g:signal:queryTooltip") as before.
-- Use 'GI.Gtk.Objects.Widget.widgetSetTooltipWindow' to set a t'GI.Gtk.Objects.Window.Window' created by you as
-- tooltip window.
-- * In the [Widget::queryTooltip]("GI.Gtk.Objects.Widget#g:signal:queryTooltip") callback you can access your window using
-- 'GI.Gtk.Objects.Widget.widgetGetTooltipWindow' and manipulate as you wish. The semantics of
-- the return value are exactly as before, return 'P.True' to show the window,
-- 'P.False' to not show it.
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Tooltip
    ( 

-- * Exported types
    Tooltip(..)                             ,
    IsTooltip                               ,
    toTooltip                               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setCustom]("GI.Gtk.Objects.Tooltip#g:method:setCustom"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setIcon]("GI.Gtk.Objects.Tooltip#g:method:setIcon"), [setIconFromGicon]("GI.Gtk.Objects.Tooltip#g:method:setIconFromGicon"), [setIconFromIconName]("GI.Gtk.Objects.Tooltip#g:method:setIconFromIconName"), [setIconFromStock]("GI.Gtk.Objects.Tooltip#g:method:setIconFromStock"), [setMarkup]("GI.Gtk.Objects.Tooltip#g:method:setMarkup"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setText]("GI.Gtk.Objects.Tooltip#g:method:setText"), [setTipArea]("GI.Gtk.Objects.Tooltip#g:method:setTipArea").

#if defined(ENABLE_OVERLOADING)
    ResolveTooltipMethod                    ,
#endif

-- ** setCustom #method:setCustom#

#if defined(ENABLE_OVERLOADING)
    TooltipSetCustomMethodInfo              ,
#endif
    tooltipSetCustom                        ,


-- ** setIcon #method:setIcon#

#if defined(ENABLE_OVERLOADING)
    TooltipSetIconMethodInfo                ,
#endif
    tooltipSetIcon                          ,


-- ** setIconFromGicon #method:setIconFromGicon#

#if defined(ENABLE_OVERLOADING)
    TooltipSetIconFromGiconMethodInfo       ,
#endif
    tooltipSetIconFromGicon                 ,


-- ** setIconFromIconName #method:setIconFromIconName#

#if defined(ENABLE_OVERLOADING)
    TooltipSetIconFromIconNameMethodInfo    ,
#endif
    tooltipSetIconFromIconName              ,


-- ** setIconFromStock #method:setIconFromStock#

#if defined(ENABLE_OVERLOADING)
    TooltipSetIconFromStockMethodInfo       ,
#endif
    tooltipSetIconFromStock                 ,


-- ** setMarkup #method:setMarkup#

#if defined(ENABLE_OVERLOADING)
    TooltipSetMarkupMethodInfo              ,
#endif
    tooltipSetMarkup                        ,


-- ** setText #method:setText#

#if defined(ENABLE_OVERLOADING)
    TooltipSetTextMethodInfo                ,
#endif
    tooltipSetText                          ,


-- ** setTipArea #method:setTipArea#

#if defined(ENABLE_OVERLOADING)
    TooltipSetTipAreaMethodInfo             ,
#endif
    tooltipSetTipArea                       ,


-- ** triggerTooltipQuery #method:triggerTooltipQuery#

    tooltipTriggerTooltipQuery              ,




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
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gio.Interfaces.Icon as Gio.Icon
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Tooltip = Tooltip (SP.ManagedPtr Tooltip)
    deriving (Eq)

instance SP.ManagedPtrNewtype Tooltip where
    toManagedPtr (Tooltip p) = p

foreign import ccall "gtk_tooltip_get_type"
    c_gtk_tooltip_get_type :: IO B.Types.GType

instance B.Types.TypedObject Tooltip where
    glibType = c_gtk_tooltip_get_type

instance B.Types.GObject Tooltip

-- | Type class for types which can be safely cast to `Tooltip`, for instance with `toTooltip`.
class (SP.GObject o, O.IsDescendantOf Tooltip o) => IsTooltip o
instance (SP.GObject o, O.IsDescendantOf Tooltip o) => IsTooltip o

instance O.HasParentTypes Tooltip
type instance O.ParentTypes Tooltip = '[GObject.Object.Object]

-- | Cast to `Tooltip`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTooltip :: (MIO.MonadIO m, IsTooltip o) => o -> m Tooltip
toTooltip = MIO.liftIO . B.ManagedPtr.unsafeCastTo Tooltip

-- | Convert 'Tooltip' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Tooltip) where
    gvalueGType_ = c_gtk_tooltip_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Tooltip)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Tooltip)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Tooltip ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTooltipMethod (t :: Symbol) (o :: *) :: * where
    ResolveTooltipMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTooltipMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTooltipMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTooltipMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTooltipMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTooltipMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTooltipMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTooltipMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTooltipMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTooltipMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTooltipMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTooltipMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTooltipMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTooltipMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTooltipMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTooltipMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTooltipMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTooltipMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTooltipMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTooltipMethod "setCustom" o = TooltipSetCustomMethodInfo
    ResolveTooltipMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTooltipMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTooltipMethod "setIcon" o = TooltipSetIconMethodInfo
    ResolveTooltipMethod "setIconFromGicon" o = TooltipSetIconFromGiconMethodInfo
    ResolveTooltipMethod "setIconFromIconName" o = TooltipSetIconFromIconNameMethodInfo
    ResolveTooltipMethod "setIconFromStock" o = TooltipSetIconFromStockMethodInfo
    ResolveTooltipMethod "setMarkup" o = TooltipSetMarkupMethodInfo
    ResolveTooltipMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTooltipMethod "setText" o = TooltipSetTextMethodInfo
    ResolveTooltipMethod "setTipArea" o = TooltipSetTipAreaMethodInfo
    ResolveTooltipMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTooltipMethod t Tooltip, O.OverloadedMethod info Tooltip p) => OL.IsLabel t (Tooltip -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTooltipMethod t Tooltip, O.OverloadedMethod info Tooltip p, R.HasField t Tooltip p) => R.HasField t Tooltip p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTooltipMethod t Tooltip, O.OverloadedMethodInfo info Tooltip) => OL.IsLabel t (O.MethodProxy info Tooltip) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Tooltip
type instance O.AttributeList Tooltip = TooltipAttributeList
type TooltipAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Tooltip = TooltipSignalList
type TooltipSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method Tooltip::set_custom
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "custom_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkWidget, or %NULL to unset the old custom widget."
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

foreign import ccall "gtk_tooltip_set_custom" gtk_tooltip_set_custom :: 
    Ptr Tooltip ->                          -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    Ptr Gtk.Widget.Widget ->                -- custom_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Replaces the widget packed into the tooltip with
-- /@customWidget@/. /@customWidget@/ does not get destroyed when the tooltip goes
-- away.
-- By default a box with a t'GI.Gtk.Objects.Image.Image' and t'GI.Gtk.Objects.Label.Label' is embedded in
-- the tooltip, which can be configured using 'GI.Gtk.Objects.Tooltip.tooltipSetMarkup'
-- and 'GI.Gtk.Objects.Tooltip.tooltipSetIcon'.
-- 
-- /Since: 2.12/
tooltipSetCustom ::
    (B.CallStack.HasCallStack, MonadIO m, IsTooltip a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Maybe (b)
    -- ^ /@customWidget@/: a t'GI.Gtk.Objects.Widget.Widget', or 'P.Nothing' to unset the old custom widget.
    -> m ()
tooltipSetCustom tooltip customWidget = liftIO $ do
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    maybeCustomWidget <- case customWidget of
        Nothing -> return nullPtr
        Just jCustomWidget -> do
            jCustomWidget' <- unsafeManagedPtrCastPtr jCustomWidget
            return jCustomWidget'
    gtk_tooltip_set_custom tooltip' maybeCustomWidget
    touchManagedPtr tooltip
    whenJust customWidget touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TooltipSetCustomMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTooltip a, Gtk.Widget.IsWidget b) => O.OverloadedMethod TooltipSetCustomMethodInfo a signature where
    overloadedMethod = tooltipSetCustom

instance O.OverloadedMethodInfo TooltipSetCustomMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Tooltip.tooltipSetCustom",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Tooltip.html#v:tooltipSetCustom"
        })


#endif

-- method Tooltip::set_icon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkPixbuf, or %NULL"
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

foreign import ccall "gtk_tooltip_set_icon" gtk_tooltip_set_icon :: 
    Ptr Tooltip ->                          -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

-- | Sets the icon of the tooltip (which is in front of the text) to be
-- /@pixbuf@/.  If /@pixbuf@/ is 'P.Nothing', the image will be hidden.
-- 
-- /Since: 2.12/
tooltipSetIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsTooltip a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Maybe (b)
    -- ^ /@pixbuf@/: a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf', or 'P.Nothing'
    -> m ()
tooltipSetIcon tooltip pixbuf = liftIO $ do
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    maybePixbuf <- case pixbuf of
        Nothing -> return nullPtr
        Just jPixbuf -> do
            jPixbuf' <- unsafeManagedPtrCastPtr jPixbuf
            return jPixbuf'
    gtk_tooltip_set_icon tooltip' maybePixbuf
    touchManagedPtr tooltip
    whenJust pixbuf touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TooltipSetIconMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTooltip a, GdkPixbuf.Pixbuf.IsPixbuf b) => O.OverloadedMethod TooltipSetIconMethodInfo a signature where
    overloadedMethod = tooltipSetIcon

instance O.OverloadedMethodInfo TooltipSetIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Tooltip.tooltipSetIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Tooltip.html#v:tooltipSetIcon"
        })


#endif

-- method Tooltip::set_icon_from_gicon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gicon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GIcon representing the icon, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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

foreign import ccall "gtk_tooltip_set_icon_from_gicon" gtk_tooltip_set_icon_from_gicon :: 
    Ptr Tooltip ->                          -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    Ptr Gio.Icon.Icon ->                    -- gicon : TInterface (Name {namespace = "Gio", name = "Icon"})
    Int32 ->                                -- size : TBasicType TInt
    IO ()

-- | Sets the icon of the tooltip (which is in front of the text)
-- to be the icon indicated by /@gicon@/ with the size indicated
-- by /@size@/. If /@gicon@/ is 'P.Nothing', the image will be hidden.
-- 
-- /Since: 2.20/
tooltipSetIconFromGicon ::
    (B.CallStack.HasCallStack, MonadIO m, IsTooltip a, Gio.Icon.IsIcon b) =>
    a
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Maybe (b)
    -- ^ /@gicon@/: a t'GI.Gio.Interfaces.Icon.Icon' representing the icon, or 'P.Nothing'
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m ()
tooltipSetIconFromGicon tooltip gicon size = liftIO $ do
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    maybeGicon <- case gicon of
        Nothing -> return nullPtr
        Just jGicon -> do
            jGicon' <- unsafeManagedPtrCastPtr jGicon
            return jGicon'
    gtk_tooltip_set_icon_from_gicon tooltip' maybeGicon size
    touchManagedPtr tooltip
    whenJust gicon touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TooltipSetIconFromGiconMethodInfo
instance (signature ~ (Maybe (b) -> Int32 -> m ()), MonadIO m, IsTooltip a, Gio.Icon.IsIcon b) => O.OverloadedMethod TooltipSetIconFromGiconMethodInfo a signature where
    overloadedMethod = tooltipSetIconFromGicon

instance O.OverloadedMethodInfo TooltipSetIconFromGiconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Tooltip.tooltipSetIconFromGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Tooltip.html#v:tooltipSetIconFromGicon"
        })


#endif

-- method Tooltip::set_icon_from_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an icon name, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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

foreign import ccall "gtk_tooltip_set_icon_from_icon_name" gtk_tooltip_set_icon_from_icon_name :: 
    Ptr Tooltip ->                          -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    IO ()

-- | Sets the icon of the tooltip (which is in front of the text) to be
-- the icon indicated by /@iconName@/ with the size indicated
-- by /@size@/.  If /@iconName@/ is 'P.Nothing', the image will be hidden.
-- 
-- /Since: 2.14/
tooltipSetIconFromIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsTooltip a) =>
    a
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Maybe (T.Text)
    -- ^ /@iconName@/: an icon name, or 'P.Nothing'
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m ()
tooltipSetIconFromIconName tooltip iconName size = liftIO $ do
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    maybeIconName <- case iconName of
        Nothing -> return nullPtr
        Just jIconName -> do
            jIconName' <- textToCString jIconName
            return jIconName'
    gtk_tooltip_set_icon_from_icon_name tooltip' maybeIconName size
    touchManagedPtr tooltip
    freeMem maybeIconName
    return ()

#if defined(ENABLE_OVERLOADING)
data TooltipSetIconFromIconNameMethodInfo
instance (signature ~ (Maybe (T.Text) -> Int32 -> m ()), MonadIO m, IsTooltip a) => O.OverloadedMethod TooltipSetIconFromIconNameMethodInfo a signature where
    overloadedMethod = tooltipSetIconFromIconName

instance O.OverloadedMethodInfo TooltipSetIconFromIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Tooltip.tooltipSetIconFromIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Tooltip.html#v:tooltipSetIconFromIconName"
        })


#endif

-- method Tooltip::set_icon_from_stock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a stock id, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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

foreign import ccall "gtk_tooltip_set_icon_from_stock" gtk_tooltip_set_icon_from_stock :: 
    Ptr Tooltip ->                          -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    CString ->                              -- stock_id : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    IO ()

{-# DEPRECATED tooltipSetIconFromStock ["(Since version 3.10)","Use 'GI.Gtk.Objects.Tooltip.tooltipSetIconFromIconName' instead."] #-}
-- | Sets the icon of the tooltip (which is in front of the text) to be
-- the stock item indicated by /@stockId@/ with the size indicated
-- by /@size@/.  If /@stockId@/ is 'P.Nothing', the image will be hidden.
-- 
-- /Since: 2.12/
tooltipSetIconFromStock ::
    (B.CallStack.HasCallStack, MonadIO m, IsTooltip a) =>
    a
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Maybe (T.Text)
    -- ^ /@stockId@/: a stock id, or 'P.Nothing'
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m ()
tooltipSetIconFromStock tooltip stockId size = liftIO $ do
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    maybeStockId <- case stockId of
        Nothing -> return nullPtr
        Just jStockId -> do
            jStockId' <- textToCString jStockId
            return jStockId'
    gtk_tooltip_set_icon_from_stock tooltip' maybeStockId size
    touchManagedPtr tooltip
    freeMem maybeStockId
    return ()

#if defined(ENABLE_OVERLOADING)
data TooltipSetIconFromStockMethodInfo
instance (signature ~ (Maybe (T.Text) -> Int32 -> m ()), MonadIO m, IsTooltip a) => O.OverloadedMethod TooltipSetIconFromStockMethodInfo a signature where
    overloadedMethod = tooltipSetIconFromStock

instance O.OverloadedMethodInfo TooltipSetIconFromStockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Tooltip.tooltipSetIconFromStock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Tooltip.html#v:tooltipSetIconFromStock"
        })


#endif

-- method Tooltip::set_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "markup"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a markup string (see [Pango markup format][PangoMarkupFormat]) or %NULL"
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

foreign import ccall "gtk_tooltip_set_markup" gtk_tooltip_set_markup :: 
    Ptr Tooltip ->                          -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    CString ->                              -- markup : TBasicType TUTF8
    IO ()

-- | Sets the text of the tooltip to be /@markup@/, which is marked up
-- with the [Pango text markup language][PangoMarkupFormat].
-- If /@markup@/ is 'P.Nothing', the label will be hidden.
-- 
-- /Since: 2.12/
tooltipSetMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsTooltip a) =>
    a
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Maybe (T.Text)
    -- ^ /@markup@/: a markup string (see [Pango markup format][PangoMarkupFormat]) or 'P.Nothing'
    -> m ()
tooltipSetMarkup tooltip markup = liftIO $ do
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    maybeMarkup <- case markup of
        Nothing -> return nullPtr
        Just jMarkup -> do
            jMarkup' <- textToCString jMarkup
            return jMarkup'
    gtk_tooltip_set_markup tooltip' maybeMarkup
    touchManagedPtr tooltip
    freeMem maybeMarkup
    return ()

#if defined(ENABLE_OVERLOADING)
data TooltipSetMarkupMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsTooltip a) => O.OverloadedMethod TooltipSetMarkupMethodInfo a signature where
    overloadedMethod = tooltipSetMarkup

instance O.OverloadedMethodInfo TooltipSetMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Tooltip.tooltipSetMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Tooltip.html#v:tooltipSetMarkup"
        })


#endif

-- method Tooltip::set_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a text string or %NULL"
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

foreign import ccall "gtk_tooltip_set_text" gtk_tooltip_set_text :: 
    Ptr Tooltip ->                          -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Sets the text of the tooltip to be /@text@/. If /@text@/ is 'P.Nothing', the label
-- will be hidden. See also 'GI.Gtk.Objects.Tooltip.tooltipSetMarkup'.
-- 
-- /Since: 2.12/
tooltipSetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsTooltip a) =>
    a
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Maybe (T.Text)
    -- ^ /@text@/: a text string or 'P.Nothing'
    -> m ()
tooltipSetText tooltip text = liftIO $ do
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    maybeText <- case text of
        Nothing -> return nullPtr
        Just jText -> do
            jText' <- textToCString jText
            return jText'
    gtk_tooltip_set_text tooltip' maybeText
    touchManagedPtr tooltip
    freeMem maybeText
    return ()

#if defined(ENABLE_OVERLOADING)
data TooltipSetTextMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsTooltip a) => O.OverloadedMethod TooltipSetTextMethodInfo a signature where
    overloadedMethod = tooltipSetText

instance O.OverloadedMethodInfo TooltipSetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Tooltip.tooltipSetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Tooltip.html#v:tooltipSetText"
        })


#endif

-- method Tooltip::set_tip_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rect"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkRectangle" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tooltip_set_tip_area" gtk_tooltip_set_tip_area :: 
    Ptr Tooltip ->                          -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Sets the area of the widget, where the contents of this tooltip apply,
-- to be /@rect@/ (in widget coordinates).  This is especially useful for
-- properly setting tooltips on t'GI.Gtk.Objects.TreeView.TreeView' rows and cells, @/GtkIconViews/@,
-- etc.
-- 
-- For setting tooltips on t'GI.Gtk.Objects.TreeView.TreeView', please refer to the convenience
-- functions for this: 'GI.Gtk.Objects.TreeView.treeViewSetTooltipRow' and
-- 'GI.Gtk.Objects.TreeView.treeViewSetTooltipCell'.
-- 
-- /Since: 2.12/
tooltipSetTipArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsTooltip a) =>
    a
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Gdk.Rectangle.Rectangle
    -- ^ /@rect@/: a t'GI.Gdk.Structs.Rectangle.Rectangle'
    -> m ()
tooltipSetTipArea tooltip rect = liftIO $ do
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    rect' <- unsafeManagedPtrGetPtr rect
    gtk_tooltip_set_tip_area tooltip' rect'
    touchManagedPtr tooltip
    touchManagedPtr rect
    return ()

#if defined(ENABLE_OVERLOADING)
data TooltipSetTipAreaMethodInfo
instance (signature ~ (Gdk.Rectangle.Rectangle -> m ()), MonadIO m, IsTooltip a) => O.OverloadedMethod TooltipSetTipAreaMethodInfo a signature where
    overloadedMethod = tooltipSetTipArea

instance O.OverloadedMethodInfo TooltipSetTipAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Tooltip.tooltipSetTipArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Tooltip.html#v:tooltipSetTipArea"
        })


#endif

-- method Tooltip::trigger_tooltip_query
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "display"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Display" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkDisplay" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tooltip_trigger_tooltip_query" gtk_tooltip_trigger_tooltip_query :: 
    Ptr Gdk.Display.Display ->              -- display : TInterface (Name {namespace = "Gdk", name = "Display"})
    IO ()

-- | Triggers a new tooltip query on /@display@/, in order to update the current
-- visible tooltip, or to show\/hide the current tooltip.  This function is
-- useful to call when, for example, the state of the widget changed by a
-- key press.
-- 
-- /Since: 2.12/
tooltipTriggerTooltipQuery ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Display.IsDisplay a) =>
    a
    -- ^ /@display@/: a t'GI.Gdk.Objects.Display.Display'
    -> m ()
tooltipTriggerTooltipQuery display = liftIO $ do
    display' <- unsafeManagedPtrCastPtr display
    gtk_tooltip_trigger_tooltip_query display'
    touchManagedPtr display
    return ()

#if defined(ENABLE_OVERLOADING)
#endif


