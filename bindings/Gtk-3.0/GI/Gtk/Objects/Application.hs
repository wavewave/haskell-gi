{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.Application.Application' is a class that handles many important aspects
-- of a GTK+ application in a convenient fashion, without enforcing
-- a one-size-fits-all application model.
-- 
-- Currently, GtkApplication handles GTK+ initialization, application
-- uniqueness, session management, provides some basic scriptability and
-- desktop shell integration by exporting actions and menus and manages a
-- list of toplevel windows whose life-cycle is automatically tied to the
-- life-cycle of your application.
-- 
-- While GtkApplication works fine with plain @/GtkWindows/@, it is recommended
-- to use it together with t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow'.
-- 
-- When GDK threads are enabled, GtkApplication will acquire the GDK
-- lock when invoking actions that arrive from other processes.  The GDK
-- lock is not touched for local action invocations.  In order to have
-- actions invoked in a predictable context it is therefore recommended
-- that the GDK lock be held while invoking actions locally with
-- 'GI.Gio.Interfaces.ActionGroup.actionGroupActivateAction'.  The same applies to actions
-- associated with t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow' and to the “activate” and
-- “open” t'GI.Gio.Objects.Application.Application' methods.
-- 
-- ## Automatic resources ## {@/automatic/@-resources}
-- 
-- t'GI.Gtk.Objects.Application.Application' will automatically load menus from the t'GI.Gtk.Objects.Builder.Builder'
-- resource located at \"gtk\/menus.ui\", relative to the application\'s
-- resource base path (see 'GI.Gio.Objects.Application.applicationSetResourceBasePath').  The
-- menu with the ID \"app-menu\" is taken as the application\'s app menu
-- and the menu with the ID \"menubar\" is taken as the application\'s
-- menubar.  Additional menus (most interesting submenus) can be named
-- and accessed via 'GI.Gtk.Objects.Application.applicationGetMenuById' which allows for
-- dynamic population of a part of the menu structure.
-- 
-- If the resources \"gtk\/menus-appmenu.ui\" or \"gtk\/menus-traditional.ui\" are
-- present then these files will be used in preference, depending on the value
-- of 'GI.Gtk.Objects.Application.applicationPrefersAppMenu'. If the resource \"gtk\/menus-common.ui\"
-- is present it will be loaded as well. This is useful for storing items that
-- are referenced from both \"gtk\/menus-appmenu.ui\" and
-- \"gtk\/menus-traditional.ui\".
-- 
-- It is also possible to provide the menus manually using
-- 'GI.Gtk.Objects.Application.applicationSetAppMenu' and 'GI.Gtk.Objects.Application.applicationSetMenubar'.
-- 
-- t'GI.Gtk.Objects.Application.Application' will also automatically setup an icon search path for
-- the default icon theme by appending \"icons\" to the resource base
-- path.  This allows your application to easily store its icons as
-- resources.  See 'GI.Gtk.Objects.IconTheme.iconThemeAddResourcePath' for more
-- information.
-- 
-- If there is a resource located at \"gtk\/help-overlay.ui\" which
-- defines a t'GI.Gtk.Objects.ShortcutsWindow.ShortcutsWindow' with ID \"help_overlay\" then GtkApplication
-- associates an instance of this shortcuts window with each
-- t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow' and sets up keyboard accelerators (Control-F1
-- and Control-?) to open it. To create a menu item that displays the
-- shortcuts window, associate the item with the action win.show-help-overlay.
-- 
-- ## A simple application ## {@/gtkapplication/@}
-- 
-- <https://git.gnome.org/browse/gtk+/tree/examples/bp/bloatpad.c A simple example>
-- 
-- GtkApplication optionally registers with a session manager
-- of the users session (if you set the [Application:registerSession]("GI.Gtk.Objects.Application#g:attr:registerSession")
-- property) and offers various functionality related to the session
-- life-cycle.
-- 
-- An application can block various ways to end the session with
-- the 'GI.Gtk.Objects.Application.applicationInhibit' function. Typical use cases for
-- this kind of inhibiting are long-running, uninterruptible operations,
-- such as burning a CD or performing a disk backup. The session
-- manager may not honor the inhibitor, but it can be expected to
-- inform the user about the negative consequences of ending the
-- session while inhibitors are present.
-- 
-- ## See Also ## {@/seealso/@}
-- <https://wiki.gnome.org/HowDoI/GtkApplication HowDoI: Using GtkApplication>,
-- <https://developer.gnome.org/gtk3/stable/gtk-getting-started.html#id-1.2.3.3 Getting Started with GTK+: Basics>

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Application
    ( 

-- * Exported types
    Application(..)                         ,
    IsApplication                           ,
    toApplication                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [actionAdded]("GI.Gio.Interfaces.ActionGroup#g:method:actionAdded"), [actionEnabledChanged]("GI.Gio.Interfaces.ActionGroup#g:method:actionEnabledChanged"), [actionRemoved]("GI.Gio.Interfaces.ActionGroup#g:method:actionRemoved"), [actionStateChanged]("GI.Gio.Interfaces.ActionGroup#g:method:actionStateChanged"), [activate]("GI.Gio.Objects.Application#g:method:activate"), [activateAction]("GI.Gio.Interfaces.ActionGroup#g:method:activateAction"), [addAccelerator]("GI.Gtk.Objects.Application#g:method:addAccelerator"), [addAction]("GI.Gio.Interfaces.ActionMap#g:method:addAction"), [addActionEntries]("GI.Gio.Interfaces.ActionMap#g:method:addActionEntries"), [addMainOption]("GI.Gio.Objects.Application#g:method:addMainOption"), [addMainOptionEntries]("GI.Gio.Objects.Application#g:method:addMainOptionEntries"), [addOptionGroup]("GI.Gio.Objects.Application#g:method:addOptionGroup"), [addWindow]("GI.Gtk.Objects.Application#g:method:addWindow"), [bindBusyProperty]("GI.Gio.Objects.Application#g:method:bindBusyProperty"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [changeActionState]("GI.Gio.Interfaces.ActionGroup#g:method:changeActionState"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasAction]("GI.Gio.Interfaces.ActionGroup#g:method:hasAction"), [hold]("GI.Gio.Objects.Application#g:method:hold"), [inhibit]("GI.Gtk.Objects.Application#g:method:inhibit"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isInhibited]("GI.Gtk.Objects.Application#g:method:isInhibited"), [listActionDescriptions]("GI.Gtk.Objects.Application#g:method:listActionDescriptions"), [listActions]("GI.Gio.Interfaces.ActionGroup#g:method:listActions"), [lookupAction]("GI.Gio.Interfaces.ActionMap#g:method:lookupAction"), [markBusy]("GI.Gio.Objects.Application#g:method:markBusy"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [open]("GI.Gio.Objects.Application#g:method:open"), [prefersAppMenu]("GI.Gtk.Objects.Application#g:method:prefersAppMenu"), [queryAction]("GI.Gio.Interfaces.ActionGroup#g:method:queryAction"), [quit]("GI.Gio.Objects.Application#g:method:quit"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [register]("GI.Gio.Objects.Application#g:method:register"), [release]("GI.Gio.Objects.Application#g:method:release"), [removeAccelerator]("GI.Gtk.Objects.Application#g:method:removeAccelerator"), [removeAction]("GI.Gio.Interfaces.ActionMap#g:method:removeAction"), [removeWindow]("GI.Gtk.Objects.Application#g:method:removeWindow"), [run]("GI.Gio.Objects.Application#g:method:run"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendNotification]("GI.Gio.Objects.Application#g:method:sendNotification"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unbindBusyProperty]("GI.Gio.Objects.Application#g:method:unbindBusyProperty"), [uninhibit]("GI.Gtk.Objects.Application#g:method:uninhibit"), [unmarkBusy]("GI.Gio.Objects.Application#g:method:unmarkBusy"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure"), [withdrawNotification]("GI.Gio.Objects.Application#g:method:withdrawNotification").
-- 
-- ==== Getters
-- [getAccelsForAction]("GI.Gtk.Objects.Application#g:method:getAccelsForAction"), [getActionEnabled]("GI.Gio.Interfaces.ActionGroup#g:method:getActionEnabled"), [getActionParameterType]("GI.Gio.Interfaces.ActionGroup#g:method:getActionParameterType"), [getActionState]("GI.Gio.Interfaces.ActionGroup#g:method:getActionState"), [getActionStateHint]("GI.Gio.Interfaces.ActionGroup#g:method:getActionStateHint"), [getActionStateType]("GI.Gio.Interfaces.ActionGroup#g:method:getActionStateType"), [getActionsForAccel]("GI.Gtk.Objects.Application#g:method:getActionsForAccel"), [getActiveWindow]("GI.Gtk.Objects.Application#g:method:getActiveWindow"), [getAppMenu]("GI.Gtk.Objects.Application#g:method:getAppMenu"), [getApplicationId]("GI.Gio.Objects.Application#g:method:getApplicationId"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDbusConnection]("GI.Gio.Objects.Application#g:method:getDbusConnection"), [getDbusObjectPath]("GI.Gio.Objects.Application#g:method:getDbusObjectPath"), [getFlags]("GI.Gio.Objects.Application#g:method:getFlags"), [getInactivityTimeout]("GI.Gio.Objects.Application#g:method:getInactivityTimeout"), [getIsBusy]("GI.Gio.Objects.Application#g:method:getIsBusy"), [getIsRegistered]("GI.Gio.Objects.Application#g:method:getIsRegistered"), [getIsRemote]("GI.Gio.Objects.Application#g:method:getIsRemote"), [getMenuById]("GI.Gtk.Objects.Application#g:method:getMenuById"), [getMenubar]("GI.Gtk.Objects.Application#g:method:getMenubar"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getResourceBasePath]("GI.Gio.Objects.Application#g:method:getResourceBasePath"), [getWindowById]("GI.Gtk.Objects.Application#g:method:getWindowById"), [getWindows]("GI.Gtk.Objects.Application#g:method:getWindows").
-- 
-- ==== Setters
-- [setAccelsForAction]("GI.Gtk.Objects.Application#g:method:setAccelsForAction"), [setActionGroup]("GI.Gio.Objects.Application#g:method:setActionGroup"), [setAppMenu]("GI.Gtk.Objects.Application#g:method:setAppMenu"), [setApplicationId]("GI.Gio.Objects.Application#g:method:setApplicationId"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDefault]("GI.Gio.Objects.Application#g:method:setDefault"), [setFlags]("GI.Gio.Objects.Application#g:method:setFlags"), [setInactivityTimeout]("GI.Gio.Objects.Application#g:method:setInactivityTimeout"), [setMenubar]("GI.Gtk.Objects.Application#g:method:setMenubar"), [setOptionContextDescription]("GI.Gio.Objects.Application#g:method:setOptionContextDescription"), [setOptionContextParameterString]("GI.Gio.Objects.Application#g:method:setOptionContextParameterString"), [setOptionContextSummary]("GI.Gio.Objects.Application#g:method:setOptionContextSummary"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setResourceBasePath]("GI.Gio.Objects.Application#g:method:setResourceBasePath").

#if defined(ENABLE_OVERLOADING)
    ResolveApplicationMethod                ,
#endif

-- ** addAccelerator #method:addAccelerator#

#if defined(ENABLE_OVERLOADING)
    ApplicationAddAcceleratorMethodInfo     ,
#endif
    applicationAddAccelerator               ,


-- ** addWindow #method:addWindow#

#if defined(ENABLE_OVERLOADING)
    ApplicationAddWindowMethodInfo          ,
#endif
    applicationAddWindow                    ,


-- ** getAccelsForAction #method:getAccelsForAction#

#if defined(ENABLE_OVERLOADING)
    ApplicationGetAccelsForActionMethodInfo ,
#endif
    applicationGetAccelsForAction           ,


-- ** getActionsForAccel #method:getActionsForAccel#

#if defined(ENABLE_OVERLOADING)
    ApplicationGetActionsForAccelMethodInfo ,
#endif
    applicationGetActionsForAccel           ,


-- ** getActiveWindow #method:getActiveWindow#

#if defined(ENABLE_OVERLOADING)
    ApplicationGetActiveWindowMethodInfo    ,
#endif
    applicationGetActiveWindow              ,


-- ** getAppMenu #method:getAppMenu#

#if defined(ENABLE_OVERLOADING)
    ApplicationGetAppMenuMethodInfo         ,
#endif
    applicationGetAppMenu                   ,


-- ** getMenuById #method:getMenuById#

#if defined(ENABLE_OVERLOADING)
    ApplicationGetMenuByIdMethodInfo        ,
#endif
    applicationGetMenuById                  ,


-- ** getMenubar #method:getMenubar#

#if defined(ENABLE_OVERLOADING)
    ApplicationGetMenubarMethodInfo         ,
#endif
    applicationGetMenubar                   ,


-- ** getWindowById #method:getWindowById#

#if defined(ENABLE_OVERLOADING)
    ApplicationGetWindowByIdMethodInfo      ,
#endif
    applicationGetWindowById                ,


-- ** getWindows #method:getWindows#

#if defined(ENABLE_OVERLOADING)
    ApplicationGetWindowsMethodInfo         ,
#endif
    applicationGetWindows                   ,


-- ** inhibit #method:inhibit#

#if defined(ENABLE_OVERLOADING)
    ApplicationInhibitMethodInfo            ,
#endif
    applicationInhibit                      ,


-- ** isInhibited #method:isInhibited#

#if defined(ENABLE_OVERLOADING)
    ApplicationIsInhibitedMethodInfo        ,
#endif
    applicationIsInhibited                  ,


-- ** listActionDescriptions #method:listActionDescriptions#

#if defined(ENABLE_OVERLOADING)
    ApplicationListActionDescriptionsMethodInfo,
#endif
    applicationListActionDescriptions       ,


-- ** new #method:new#

    applicationNew                          ,


-- ** prefersAppMenu #method:prefersAppMenu#

#if defined(ENABLE_OVERLOADING)
    ApplicationPrefersAppMenuMethodInfo     ,
#endif
    applicationPrefersAppMenu               ,


-- ** removeAccelerator #method:removeAccelerator#

#if defined(ENABLE_OVERLOADING)
    ApplicationRemoveAcceleratorMethodInfo  ,
#endif
    applicationRemoveAccelerator            ,


-- ** removeWindow #method:removeWindow#

#if defined(ENABLE_OVERLOADING)
    ApplicationRemoveWindowMethodInfo       ,
#endif
    applicationRemoveWindow                 ,


-- ** setAccelsForAction #method:setAccelsForAction#

#if defined(ENABLE_OVERLOADING)
    ApplicationSetAccelsForActionMethodInfo ,
#endif
    applicationSetAccelsForAction           ,


-- ** setAppMenu #method:setAppMenu#

#if defined(ENABLE_OVERLOADING)
    ApplicationSetAppMenuMethodInfo         ,
#endif
    applicationSetAppMenu                   ,


-- ** setMenubar #method:setMenubar#

#if defined(ENABLE_OVERLOADING)
    ApplicationSetMenubarMethodInfo         ,
#endif
    applicationSetMenubar                   ,


-- ** uninhibit #method:uninhibit#

#if defined(ENABLE_OVERLOADING)
    ApplicationUninhibitMethodInfo          ,
#endif
    applicationUninhibit                    ,




 -- * Properties


-- ** activeWindow #attr:activeWindow#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ApplicationActiveWindowPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    applicationActiveWindow                 ,
#endif
    getApplicationActiveWindow              ,


-- ** appMenu #attr:appMenu#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ApplicationAppMenuPropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    applicationAppMenu                      ,
#endif
    clearApplicationAppMenu                 ,
    constructApplicationAppMenu             ,
    getApplicationAppMenu                   ,
    setApplicationAppMenu                   ,


-- ** menubar #attr:menubar#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ApplicationMenubarPropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    applicationMenubar                      ,
#endif
    clearApplicationMenubar                 ,
    constructApplicationMenubar             ,
    getApplicationMenubar                   ,
    setApplicationMenubar                   ,


-- ** registerSession #attr:registerSession#
-- | Set this property to 'P.True' to register with the session manager.
-- 
-- /Since: 3.4/

#if defined(ENABLE_OVERLOADING)
    ApplicationRegisterSessionPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    applicationRegisterSession              ,
#endif
    constructApplicationRegisterSession     ,
    getApplicationRegisterSession           ,
    setApplicationRegisterSession           ,


-- ** screensaverActive #attr:screensaverActive#
-- | This property is 'P.True' if GTK+ believes that the screensaver is
-- currently active. GTK+ only tracks session state (including this)
-- when t'GI.Gtk.Objects.Application.Application'::@/register-session/@ is set to 'P.True'.
-- 
-- Tracking the screensaver state is supported on Linux.
-- 
-- /Since: 3.24/

#if defined(ENABLE_OVERLOADING)
    ApplicationScreensaverActivePropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    applicationScreensaverActive            ,
#endif
    getApplicationScreensaverActive         ,




 -- * Signals


-- ** queryEnd #signal:queryEnd#

    ApplicationQueryEndCallback             ,
#if defined(ENABLE_OVERLOADING)
    ApplicationQueryEndSignalInfo           ,
#endif
    afterApplicationQueryEnd                ,
    onApplicationQueryEnd                   ,


-- ** windowAdded #signal:windowAdded#

    ApplicationWindowAddedCallback          ,
#if defined(ENABLE_OVERLOADING)
    ApplicationWindowAddedSignalInfo        ,
#endif
    afterApplicationWindowAdded             ,
    onApplicationWindowAdded                ,


-- ** windowRemoved #signal:windowRemoved#

    ApplicationWindowRemovedCallback        ,
#if defined(ENABLE_OVERLOADING)
    ApplicationWindowRemovedSignalInfo      ,
#endif
    afterApplicationWindowRemoved           ,
    onApplicationWindowRemoved              ,




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
import qualified GI.Gio.Flags as Gio.Flags
import qualified GI.Gio.Interfaces.ActionGroup as Gio.ActionGroup
import qualified GI.Gio.Interfaces.ActionMap as Gio.ActionMap
import qualified GI.Gio.Objects.Application as Gio.Application
import qualified GI.Gio.Objects.Menu as Gio.Menu
import qualified GI.Gio.Objects.MenuModel as Gio.MenuModel
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype Application = Application (SP.ManagedPtr Application)
    deriving (Eq)

instance SP.ManagedPtrNewtype Application where
    toManagedPtr (Application p) = p

foreign import ccall "gtk_application_get_type"
    c_gtk_application_get_type :: IO B.Types.GType

instance B.Types.TypedObject Application where
    glibType = c_gtk_application_get_type

instance B.Types.GObject Application

-- | Type class for types which can be safely cast to `Application`, for instance with `toApplication`.
class (SP.GObject o, O.IsDescendantOf Application o) => IsApplication o
instance (SP.GObject o, O.IsDescendantOf Application o) => IsApplication o

instance O.HasParentTypes Application
type instance O.ParentTypes Application = '[Gio.Application.Application, GObject.Object.Object, Gio.ActionGroup.ActionGroup, Gio.ActionMap.ActionMap]

-- | Cast to `Application`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toApplication :: (MIO.MonadIO m, IsApplication o) => o -> m Application
toApplication = MIO.liftIO . B.ManagedPtr.unsafeCastTo Application

-- | Convert 'Application' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Application) where
    gvalueGType_ = c_gtk_application_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Application)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Application)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Application ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveApplicationMethod (t :: Symbol) (o :: *) :: * where
    ResolveApplicationMethod "actionAdded" o = Gio.ActionGroup.ActionGroupActionAddedMethodInfo
    ResolveApplicationMethod "actionEnabledChanged" o = Gio.ActionGroup.ActionGroupActionEnabledChangedMethodInfo
    ResolveApplicationMethod "actionRemoved" o = Gio.ActionGroup.ActionGroupActionRemovedMethodInfo
    ResolveApplicationMethod "actionStateChanged" o = Gio.ActionGroup.ActionGroupActionStateChangedMethodInfo
    ResolveApplicationMethod "activate" o = Gio.Application.ApplicationActivateMethodInfo
    ResolveApplicationMethod "activateAction" o = Gio.ActionGroup.ActionGroupActivateActionMethodInfo
    ResolveApplicationMethod "addAccelerator" o = ApplicationAddAcceleratorMethodInfo
    ResolveApplicationMethod "addAction" o = Gio.ActionMap.ActionMapAddActionMethodInfo
    ResolveApplicationMethod "addActionEntries" o = Gio.ActionMap.ActionMapAddActionEntriesMethodInfo
    ResolveApplicationMethod "addMainOption" o = Gio.Application.ApplicationAddMainOptionMethodInfo
    ResolveApplicationMethod "addMainOptionEntries" o = Gio.Application.ApplicationAddMainOptionEntriesMethodInfo
    ResolveApplicationMethod "addOptionGroup" o = Gio.Application.ApplicationAddOptionGroupMethodInfo
    ResolveApplicationMethod "addWindow" o = ApplicationAddWindowMethodInfo
    ResolveApplicationMethod "bindBusyProperty" o = Gio.Application.ApplicationBindBusyPropertyMethodInfo
    ResolveApplicationMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveApplicationMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveApplicationMethod "changeActionState" o = Gio.ActionGroup.ActionGroupChangeActionStateMethodInfo
    ResolveApplicationMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveApplicationMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveApplicationMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveApplicationMethod "hasAction" o = Gio.ActionGroup.ActionGroupHasActionMethodInfo
    ResolveApplicationMethod "hold" o = Gio.Application.ApplicationHoldMethodInfo
    ResolveApplicationMethod "inhibit" o = ApplicationInhibitMethodInfo
    ResolveApplicationMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveApplicationMethod "isInhibited" o = ApplicationIsInhibitedMethodInfo
    ResolveApplicationMethod "listActionDescriptions" o = ApplicationListActionDescriptionsMethodInfo
    ResolveApplicationMethod "listActions" o = Gio.ActionGroup.ActionGroupListActionsMethodInfo
    ResolveApplicationMethod "lookupAction" o = Gio.ActionMap.ActionMapLookupActionMethodInfo
    ResolveApplicationMethod "markBusy" o = Gio.Application.ApplicationMarkBusyMethodInfo
    ResolveApplicationMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveApplicationMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveApplicationMethod "open" o = Gio.Application.ApplicationOpenMethodInfo
    ResolveApplicationMethod "prefersAppMenu" o = ApplicationPrefersAppMenuMethodInfo
    ResolveApplicationMethod "queryAction" o = Gio.ActionGroup.ActionGroupQueryActionMethodInfo
    ResolveApplicationMethod "quit" o = Gio.Application.ApplicationQuitMethodInfo
    ResolveApplicationMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveApplicationMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveApplicationMethod "register" o = Gio.Application.ApplicationRegisterMethodInfo
    ResolveApplicationMethod "release" o = Gio.Application.ApplicationReleaseMethodInfo
    ResolveApplicationMethod "removeAccelerator" o = ApplicationRemoveAcceleratorMethodInfo
    ResolveApplicationMethod "removeAction" o = Gio.ActionMap.ActionMapRemoveActionMethodInfo
    ResolveApplicationMethod "removeWindow" o = ApplicationRemoveWindowMethodInfo
    ResolveApplicationMethod "run" o = Gio.Application.ApplicationRunMethodInfo
    ResolveApplicationMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveApplicationMethod "sendNotification" o = Gio.Application.ApplicationSendNotificationMethodInfo
    ResolveApplicationMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveApplicationMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveApplicationMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveApplicationMethod "unbindBusyProperty" o = Gio.Application.ApplicationUnbindBusyPropertyMethodInfo
    ResolveApplicationMethod "uninhibit" o = ApplicationUninhibitMethodInfo
    ResolveApplicationMethod "unmarkBusy" o = Gio.Application.ApplicationUnmarkBusyMethodInfo
    ResolveApplicationMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveApplicationMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveApplicationMethod "withdrawNotification" o = Gio.Application.ApplicationWithdrawNotificationMethodInfo
    ResolveApplicationMethod "getAccelsForAction" o = ApplicationGetAccelsForActionMethodInfo
    ResolveApplicationMethod "getActionEnabled" o = Gio.ActionGroup.ActionGroupGetActionEnabledMethodInfo
    ResolveApplicationMethod "getActionParameterType" o = Gio.ActionGroup.ActionGroupGetActionParameterTypeMethodInfo
    ResolveApplicationMethod "getActionState" o = Gio.ActionGroup.ActionGroupGetActionStateMethodInfo
    ResolveApplicationMethod "getActionStateHint" o = Gio.ActionGroup.ActionGroupGetActionStateHintMethodInfo
    ResolveApplicationMethod "getActionStateType" o = Gio.ActionGroup.ActionGroupGetActionStateTypeMethodInfo
    ResolveApplicationMethod "getActionsForAccel" o = ApplicationGetActionsForAccelMethodInfo
    ResolveApplicationMethod "getActiveWindow" o = ApplicationGetActiveWindowMethodInfo
    ResolveApplicationMethod "getAppMenu" o = ApplicationGetAppMenuMethodInfo
    ResolveApplicationMethod "getApplicationId" o = Gio.Application.ApplicationGetApplicationIdMethodInfo
    ResolveApplicationMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveApplicationMethod "getDbusConnection" o = Gio.Application.ApplicationGetDbusConnectionMethodInfo
    ResolveApplicationMethod "getDbusObjectPath" o = Gio.Application.ApplicationGetDbusObjectPathMethodInfo
    ResolveApplicationMethod "getFlags" o = Gio.Application.ApplicationGetFlagsMethodInfo
    ResolveApplicationMethod "getInactivityTimeout" o = Gio.Application.ApplicationGetInactivityTimeoutMethodInfo
    ResolveApplicationMethod "getIsBusy" o = Gio.Application.ApplicationGetIsBusyMethodInfo
    ResolveApplicationMethod "getIsRegistered" o = Gio.Application.ApplicationGetIsRegisteredMethodInfo
    ResolveApplicationMethod "getIsRemote" o = Gio.Application.ApplicationGetIsRemoteMethodInfo
    ResolveApplicationMethod "getMenuById" o = ApplicationGetMenuByIdMethodInfo
    ResolveApplicationMethod "getMenubar" o = ApplicationGetMenubarMethodInfo
    ResolveApplicationMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveApplicationMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveApplicationMethod "getResourceBasePath" o = Gio.Application.ApplicationGetResourceBasePathMethodInfo
    ResolveApplicationMethod "getWindowById" o = ApplicationGetWindowByIdMethodInfo
    ResolveApplicationMethod "getWindows" o = ApplicationGetWindowsMethodInfo
    ResolveApplicationMethod "setAccelsForAction" o = ApplicationSetAccelsForActionMethodInfo
    ResolveApplicationMethod "setActionGroup" o = Gio.Application.ApplicationSetActionGroupMethodInfo
    ResolveApplicationMethod "setAppMenu" o = ApplicationSetAppMenuMethodInfo
    ResolveApplicationMethod "setApplicationId" o = Gio.Application.ApplicationSetApplicationIdMethodInfo
    ResolveApplicationMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveApplicationMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveApplicationMethod "setDefault" o = Gio.Application.ApplicationSetDefaultMethodInfo
    ResolveApplicationMethod "setFlags" o = Gio.Application.ApplicationSetFlagsMethodInfo
    ResolveApplicationMethod "setInactivityTimeout" o = Gio.Application.ApplicationSetInactivityTimeoutMethodInfo
    ResolveApplicationMethod "setMenubar" o = ApplicationSetMenubarMethodInfo
    ResolveApplicationMethod "setOptionContextDescription" o = Gio.Application.ApplicationSetOptionContextDescriptionMethodInfo
    ResolveApplicationMethod "setOptionContextParameterString" o = Gio.Application.ApplicationSetOptionContextParameterStringMethodInfo
    ResolveApplicationMethod "setOptionContextSummary" o = Gio.Application.ApplicationSetOptionContextSummaryMethodInfo
    ResolveApplicationMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveApplicationMethod "setResourceBasePath" o = Gio.Application.ApplicationSetResourceBasePathMethodInfo
    ResolveApplicationMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveApplicationMethod t Application, O.OverloadedMethod info Application p) => OL.IsLabel t (Application -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveApplicationMethod t Application, O.OverloadedMethod info Application p, R.HasField t Application p) => R.HasField t Application p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveApplicationMethod t Application, O.OverloadedMethodInfo info Application) => OL.IsLabel t (O.MethodProxy info Application) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Application::query-end
-- | Emitted when the session manager is about to end the session, only
-- if t'GI.Gtk.Objects.Application.Application'::@/register-session/@ is 'P.True'. Applications can
-- connect to this signal and call 'GI.Gtk.Objects.Application.applicationInhibit' with
-- 'GI.Gtk.Flags.ApplicationInhibitFlagsLogout' to delay the end of the session
-- until state has been saved.
-- 
-- /Since: 3.24.8/
type ApplicationQueryEndCallback =
    IO ()

type C_ApplicationQueryEndCallback =
    Ptr Application ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ApplicationQueryEndCallback`.
foreign import ccall "wrapper"
    mk_ApplicationQueryEndCallback :: C_ApplicationQueryEndCallback -> IO (FunPtr C_ApplicationQueryEndCallback)

wrap_ApplicationQueryEndCallback :: 
    GObject a => (a -> ApplicationQueryEndCallback) ->
    C_ApplicationQueryEndCallback
wrap_ApplicationQueryEndCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [queryEnd](#signal:queryEnd) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' application #queryEnd callback
-- @
-- 
-- 
onApplicationQueryEnd :: (IsApplication a, MonadIO m) => a -> ((?self :: a) => ApplicationQueryEndCallback) -> m SignalHandlerId
onApplicationQueryEnd obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ApplicationQueryEndCallback wrapped
    wrapped'' <- mk_ApplicationQueryEndCallback wrapped'
    connectSignalFunPtr obj "query-end" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [queryEnd](#signal:queryEnd) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' application #queryEnd callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterApplicationQueryEnd :: (IsApplication a, MonadIO m) => a -> ((?self :: a) => ApplicationQueryEndCallback) -> m SignalHandlerId
afterApplicationQueryEnd obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ApplicationQueryEndCallback wrapped
    wrapped'' <- mk_ApplicationQueryEndCallback wrapped'
    connectSignalFunPtr obj "query-end" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ApplicationQueryEndSignalInfo
instance SignalInfo ApplicationQueryEndSignalInfo where
    type HaskellCallbackType ApplicationQueryEndSignalInfo = ApplicationQueryEndCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ApplicationQueryEndCallback cb
        cb'' <- mk_ApplicationQueryEndCallback cb'
        connectSignalFunPtr obj "query-end" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application::query-end"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#g:signal:queryEnd"})

#endif

-- signal Application::window-added
-- | Emitted when a t'GI.Gtk.Objects.Window.Window' is added to /@application@/ through
-- 'GI.Gtk.Objects.Application.applicationAddWindow'.
-- 
-- /Since: 3.2/
type ApplicationWindowAddedCallback =
    Gtk.Window.Window
    -- ^ /@window@/: the newly-added t'GI.Gtk.Objects.Window.Window'
    -> IO ()

type C_ApplicationWindowAddedCallback =
    Ptr Application ->                      -- object
    Ptr Gtk.Window.Window ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ApplicationWindowAddedCallback`.
foreign import ccall "wrapper"
    mk_ApplicationWindowAddedCallback :: C_ApplicationWindowAddedCallback -> IO (FunPtr C_ApplicationWindowAddedCallback)

wrap_ApplicationWindowAddedCallback :: 
    GObject a => (a -> ApplicationWindowAddedCallback) ->
    C_ApplicationWindowAddedCallback
wrap_ApplicationWindowAddedCallback gi'cb gi'selfPtr window _ = do
    window' <- (newObject Gtk.Window.Window) window
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  window'


-- | Connect a signal handler for the [windowAdded](#signal:windowAdded) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' application #windowAdded callback
-- @
-- 
-- 
onApplicationWindowAdded :: (IsApplication a, MonadIO m) => a -> ((?self :: a) => ApplicationWindowAddedCallback) -> m SignalHandlerId
onApplicationWindowAdded obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ApplicationWindowAddedCallback wrapped
    wrapped'' <- mk_ApplicationWindowAddedCallback wrapped'
    connectSignalFunPtr obj "window-added" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [windowAdded](#signal:windowAdded) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' application #windowAdded callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterApplicationWindowAdded :: (IsApplication a, MonadIO m) => a -> ((?self :: a) => ApplicationWindowAddedCallback) -> m SignalHandlerId
afterApplicationWindowAdded obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ApplicationWindowAddedCallback wrapped
    wrapped'' <- mk_ApplicationWindowAddedCallback wrapped'
    connectSignalFunPtr obj "window-added" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ApplicationWindowAddedSignalInfo
instance SignalInfo ApplicationWindowAddedSignalInfo where
    type HaskellCallbackType ApplicationWindowAddedSignalInfo = ApplicationWindowAddedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ApplicationWindowAddedCallback cb
        cb'' <- mk_ApplicationWindowAddedCallback cb'
        connectSignalFunPtr obj "window-added" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application::window-added"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#g:signal:windowAdded"})

#endif

-- signal Application::window-removed
-- | Emitted when a t'GI.Gtk.Objects.Window.Window' is removed from /@application@/,
-- either as a side-effect of being destroyed or explicitly
-- through 'GI.Gtk.Objects.Application.applicationRemoveWindow'.
-- 
-- /Since: 3.2/
type ApplicationWindowRemovedCallback =
    Gtk.Window.Window
    -- ^ /@window@/: the t'GI.Gtk.Objects.Window.Window' that is being removed
    -> IO ()

type C_ApplicationWindowRemovedCallback =
    Ptr Application ->                      -- object
    Ptr Gtk.Window.Window ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ApplicationWindowRemovedCallback`.
foreign import ccall "wrapper"
    mk_ApplicationWindowRemovedCallback :: C_ApplicationWindowRemovedCallback -> IO (FunPtr C_ApplicationWindowRemovedCallback)

wrap_ApplicationWindowRemovedCallback :: 
    GObject a => (a -> ApplicationWindowRemovedCallback) ->
    C_ApplicationWindowRemovedCallback
wrap_ApplicationWindowRemovedCallback gi'cb gi'selfPtr window _ = do
    window' <- (newObject Gtk.Window.Window) window
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  window'


-- | Connect a signal handler for the [windowRemoved](#signal:windowRemoved) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' application #windowRemoved callback
-- @
-- 
-- 
onApplicationWindowRemoved :: (IsApplication a, MonadIO m) => a -> ((?self :: a) => ApplicationWindowRemovedCallback) -> m SignalHandlerId
onApplicationWindowRemoved obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ApplicationWindowRemovedCallback wrapped
    wrapped'' <- mk_ApplicationWindowRemovedCallback wrapped'
    connectSignalFunPtr obj "window-removed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [windowRemoved](#signal:windowRemoved) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' application #windowRemoved callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterApplicationWindowRemoved :: (IsApplication a, MonadIO m) => a -> ((?self :: a) => ApplicationWindowRemovedCallback) -> m SignalHandlerId
afterApplicationWindowRemoved obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ApplicationWindowRemovedCallback wrapped
    wrapped'' <- mk_ApplicationWindowRemovedCallback wrapped'
    connectSignalFunPtr obj "window-removed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ApplicationWindowRemovedSignalInfo
instance SignalInfo ApplicationWindowRemovedSignalInfo where
    type HaskellCallbackType ApplicationWindowRemovedSignalInfo = ApplicationWindowRemovedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ApplicationWindowRemovedCallback cb
        cb'' <- mk_ApplicationWindowRemovedCallback cb'
        connectSignalFunPtr obj "window-removed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application::window-removed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#g:signal:windowRemoved"})

#endif

-- VVV Prop "active-window"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Window"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just True,Nothing)

-- | Get the value of the “@active-window@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' application #activeWindow
-- @
getApplicationActiveWindow :: (MonadIO m, IsApplication o) => o -> m (Maybe Gtk.Window.Window)
getApplicationActiveWindow obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "active-window" Gtk.Window.Window

#if defined(ENABLE_OVERLOADING)
data ApplicationActiveWindowPropertyInfo
instance AttrInfo ApplicationActiveWindowPropertyInfo where
    type AttrAllowedOps ApplicationActiveWindowPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ApplicationActiveWindowPropertyInfo = IsApplication
    type AttrSetTypeConstraint ApplicationActiveWindowPropertyInfo = (~) ()
    type AttrTransferTypeConstraint ApplicationActiveWindowPropertyInfo = (~) ()
    type AttrTransferType ApplicationActiveWindowPropertyInfo = ()
    type AttrGetType ApplicationActiveWindowPropertyInfo = (Maybe Gtk.Window.Window)
    type AttrLabel ApplicationActiveWindowPropertyInfo = "active-window"
    type AttrOrigin ApplicationActiveWindowPropertyInfo = Application
    attrGet = getApplicationActiveWindow
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.activeWindow"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#g:attr:activeWindow"
        })
#endif

-- VVV Prop "app-menu"
   -- Type: TInterface (Name {namespace = "Gio", name = "MenuModel"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@app-menu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' application #appMenu
-- @
getApplicationAppMenu :: (MonadIO m, IsApplication o) => o -> m (Maybe Gio.MenuModel.MenuModel)
getApplicationAppMenu obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "app-menu" Gio.MenuModel.MenuModel

-- | Set the value of the “@app-menu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' application [ #appMenu 'Data.GI.Base.Attributes.:=' value ]
-- @
setApplicationAppMenu :: (MonadIO m, IsApplication o, Gio.MenuModel.IsMenuModel a) => o -> a -> m ()
setApplicationAppMenu obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "app-menu" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@app-menu@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructApplicationAppMenu :: (IsApplication o, MIO.MonadIO m, Gio.MenuModel.IsMenuModel a) => a -> m (GValueConstruct o)
constructApplicationAppMenu val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "app-menu" (P.Just val)

-- | Set the value of the “@app-menu@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #appMenu
-- @
clearApplicationAppMenu :: (MonadIO m, IsApplication o) => o -> m ()
clearApplicationAppMenu obj = liftIO $ B.Properties.setObjectPropertyObject obj "app-menu" (Nothing :: Maybe Gio.MenuModel.MenuModel)

#if defined(ENABLE_OVERLOADING)
data ApplicationAppMenuPropertyInfo
instance AttrInfo ApplicationAppMenuPropertyInfo where
    type AttrAllowedOps ApplicationAppMenuPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ApplicationAppMenuPropertyInfo = IsApplication
    type AttrSetTypeConstraint ApplicationAppMenuPropertyInfo = Gio.MenuModel.IsMenuModel
    type AttrTransferTypeConstraint ApplicationAppMenuPropertyInfo = Gio.MenuModel.IsMenuModel
    type AttrTransferType ApplicationAppMenuPropertyInfo = Gio.MenuModel.MenuModel
    type AttrGetType ApplicationAppMenuPropertyInfo = (Maybe Gio.MenuModel.MenuModel)
    type AttrLabel ApplicationAppMenuPropertyInfo = "app-menu"
    type AttrOrigin ApplicationAppMenuPropertyInfo = Application
    attrGet = getApplicationAppMenu
    attrSet = setApplicationAppMenu
    attrTransfer _ v = do
        unsafeCastTo Gio.MenuModel.MenuModel v
    attrConstruct = constructApplicationAppMenu
    attrClear = clearApplicationAppMenu
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.appMenu"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#g:attr:appMenu"
        })
#endif

-- VVV Prop "menubar"
   -- Type: TInterface (Name {namespace = "Gio", name = "MenuModel"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@menubar@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' application #menubar
-- @
getApplicationMenubar :: (MonadIO m, IsApplication o) => o -> m Gio.MenuModel.MenuModel
getApplicationMenubar obj = MIO.liftIO $ checkUnexpectedNothing "getApplicationMenubar" $ B.Properties.getObjectPropertyObject obj "menubar" Gio.MenuModel.MenuModel

-- | Set the value of the “@menubar@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' application [ #menubar 'Data.GI.Base.Attributes.:=' value ]
-- @
setApplicationMenubar :: (MonadIO m, IsApplication o, Gio.MenuModel.IsMenuModel a) => o -> a -> m ()
setApplicationMenubar obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "menubar" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@menubar@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructApplicationMenubar :: (IsApplication o, MIO.MonadIO m, Gio.MenuModel.IsMenuModel a) => a -> m (GValueConstruct o)
constructApplicationMenubar val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "menubar" (P.Just val)

-- | Set the value of the “@menubar@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #menubar
-- @
clearApplicationMenubar :: (MonadIO m, IsApplication o) => o -> m ()
clearApplicationMenubar obj = liftIO $ B.Properties.setObjectPropertyObject obj "menubar" (Nothing :: Maybe Gio.MenuModel.MenuModel)

#if defined(ENABLE_OVERLOADING)
data ApplicationMenubarPropertyInfo
instance AttrInfo ApplicationMenubarPropertyInfo where
    type AttrAllowedOps ApplicationMenubarPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ApplicationMenubarPropertyInfo = IsApplication
    type AttrSetTypeConstraint ApplicationMenubarPropertyInfo = Gio.MenuModel.IsMenuModel
    type AttrTransferTypeConstraint ApplicationMenubarPropertyInfo = Gio.MenuModel.IsMenuModel
    type AttrTransferType ApplicationMenubarPropertyInfo = Gio.MenuModel.MenuModel
    type AttrGetType ApplicationMenubarPropertyInfo = Gio.MenuModel.MenuModel
    type AttrLabel ApplicationMenubarPropertyInfo = "menubar"
    type AttrOrigin ApplicationMenubarPropertyInfo = Application
    attrGet = getApplicationMenubar
    attrSet = setApplicationMenubar
    attrTransfer _ v = do
        unsafeCastTo Gio.MenuModel.MenuModel v
    attrConstruct = constructApplicationMenubar
    attrClear = clearApplicationMenubar
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.menubar"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#g:attr:menubar"
        })
#endif

-- VVV Prop "register-session"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@register-session@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' application #registerSession
-- @
getApplicationRegisterSession :: (MonadIO m, IsApplication o) => o -> m Bool
getApplicationRegisterSession obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "register-session"

-- | Set the value of the “@register-session@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' application [ #registerSession 'Data.GI.Base.Attributes.:=' value ]
-- @
setApplicationRegisterSession :: (MonadIO m, IsApplication o) => o -> Bool -> m ()
setApplicationRegisterSession obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "register-session" val

-- | Construct a `GValueConstruct` with valid value for the “@register-session@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructApplicationRegisterSession :: (IsApplication o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructApplicationRegisterSession val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "register-session" val

#if defined(ENABLE_OVERLOADING)
data ApplicationRegisterSessionPropertyInfo
instance AttrInfo ApplicationRegisterSessionPropertyInfo where
    type AttrAllowedOps ApplicationRegisterSessionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ApplicationRegisterSessionPropertyInfo = IsApplication
    type AttrSetTypeConstraint ApplicationRegisterSessionPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ApplicationRegisterSessionPropertyInfo = (~) Bool
    type AttrTransferType ApplicationRegisterSessionPropertyInfo = Bool
    type AttrGetType ApplicationRegisterSessionPropertyInfo = Bool
    type AttrLabel ApplicationRegisterSessionPropertyInfo = "register-session"
    type AttrOrigin ApplicationRegisterSessionPropertyInfo = Application
    attrGet = getApplicationRegisterSession
    attrSet = setApplicationRegisterSession
    attrTransfer _ v = do
        return v
    attrConstruct = constructApplicationRegisterSession
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.registerSession"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#g:attr:registerSession"
        })
#endif

-- VVV Prop "screensaver-active"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@screensaver-active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' application #screensaverActive
-- @
getApplicationScreensaverActive :: (MonadIO m, IsApplication o) => o -> m Bool
getApplicationScreensaverActive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "screensaver-active"

#if defined(ENABLE_OVERLOADING)
data ApplicationScreensaverActivePropertyInfo
instance AttrInfo ApplicationScreensaverActivePropertyInfo where
    type AttrAllowedOps ApplicationScreensaverActivePropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint ApplicationScreensaverActivePropertyInfo = IsApplication
    type AttrSetTypeConstraint ApplicationScreensaverActivePropertyInfo = (~) ()
    type AttrTransferTypeConstraint ApplicationScreensaverActivePropertyInfo = (~) ()
    type AttrTransferType ApplicationScreensaverActivePropertyInfo = ()
    type AttrGetType ApplicationScreensaverActivePropertyInfo = Bool
    type AttrLabel ApplicationScreensaverActivePropertyInfo = "screensaver-active"
    type AttrOrigin ApplicationScreensaverActivePropertyInfo = Application
    attrGet = getApplicationScreensaverActive
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.screensaverActive"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#g:attr:screensaverActive"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Application
type instance O.AttributeList Application = ApplicationAttributeList
type ApplicationAttributeList = ('[ '("actionGroup", Gio.Application.ApplicationActionGroupPropertyInfo), '("activeWindow", ApplicationActiveWindowPropertyInfo), '("appMenu", ApplicationAppMenuPropertyInfo), '("applicationId", Gio.Application.ApplicationApplicationIdPropertyInfo), '("flags", Gio.Application.ApplicationFlagsPropertyInfo), '("inactivityTimeout", Gio.Application.ApplicationInactivityTimeoutPropertyInfo), '("isBusy", Gio.Application.ApplicationIsBusyPropertyInfo), '("isRegistered", Gio.Application.ApplicationIsRegisteredPropertyInfo), '("isRemote", Gio.Application.ApplicationIsRemotePropertyInfo), '("menubar", ApplicationMenubarPropertyInfo), '("registerSession", ApplicationRegisterSessionPropertyInfo), '("resourceBasePath", Gio.Application.ApplicationResourceBasePathPropertyInfo), '("screensaverActive", ApplicationScreensaverActivePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
applicationActiveWindow :: AttrLabelProxy "activeWindow"
applicationActiveWindow = AttrLabelProxy

applicationAppMenu :: AttrLabelProxy "appMenu"
applicationAppMenu = AttrLabelProxy

applicationMenubar :: AttrLabelProxy "menubar"
applicationMenubar = AttrLabelProxy

applicationRegisterSession :: AttrLabelProxy "registerSession"
applicationRegisterSession = AttrLabelProxy

applicationScreensaverActive :: AttrLabelProxy "screensaverActive"
applicationScreensaverActive = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Application = ApplicationSignalList
type ApplicationSignalList = ('[ '("actionAdded", Gio.ActionGroup.ActionGroupActionAddedSignalInfo), '("actionEnabledChanged", Gio.ActionGroup.ActionGroupActionEnabledChangedSignalInfo), '("actionRemoved", Gio.ActionGroup.ActionGroupActionRemovedSignalInfo), '("actionStateChanged", Gio.ActionGroup.ActionGroupActionStateChangedSignalInfo), '("activate", Gio.Application.ApplicationActivateSignalInfo), '("commandLine", Gio.Application.ApplicationCommandLineSignalInfo), '("handleLocalOptions", Gio.Application.ApplicationHandleLocalOptionsSignalInfo), '("nameLost", Gio.Application.ApplicationNameLostSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("open", Gio.Application.ApplicationOpenSignalInfo), '("queryEnd", ApplicationQueryEndSignalInfo), '("shutdown", Gio.Application.ApplicationShutdownSignalInfo), '("startup", Gio.Application.ApplicationStartupSignalInfo), '("windowAdded", ApplicationWindowAddedSignalInfo), '("windowRemoved", ApplicationWindowRemovedSignalInfo)] :: [(Symbol, *)])

#endif

-- method Application::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "application_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The application ID."
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
--               TInterface Name { namespace = "Gio" , name = "ApplicationFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the application flags"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Application" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_new" gtk_application_new :: 
    CString ->                              -- application_id : TBasicType TUTF8
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gio", name = "ApplicationFlags"})
    IO (Ptr Application)

-- | Creates a new t'GI.Gtk.Objects.Application.Application' instance.
-- 
-- When using t'GI.Gtk.Objects.Application.Application', it is not necessary to call 'GI.Gtk.Functions.init'
-- manually. It is called as soon as the application gets registered as
-- the primary instance.
-- 
-- Concretely, 'GI.Gtk.Functions.init' is called in the default handler for the
-- [Application::startup]("GI.Gio.Objects.Application#g:signal:startup") signal. Therefore, t'GI.Gtk.Objects.Application.Application' subclasses should
-- chain up in their [Application::startup]("GI.Gio.Objects.Application#g:signal:startup") handler before using any GTK+ API.
-- 
-- Note that commandline arguments are not passed to 'GI.Gtk.Functions.init'.
-- All GTK+ functionality that is available via commandline arguments
-- can also be achieved by setting suitable environment variables
-- such as @G_DEBUG@, so this should not be a big
-- problem. If you absolutely must support GTK+ commandline arguments,
-- you can explicitly call 'GI.Gtk.Functions.init' before creating the application
-- instance.
-- 
-- If non-'P.Nothing', the application ID must be valid.  See
-- 'GI.Gio.Objects.Application.applicationIdIsValid'.
-- 
-- If no application ID is given then some features (most notably application
-- uniqueness) will be disabled. A null application ID is only allowed with
-- GTK+ 3.6 or later.
-- 
-- /Since: 3.0/
applicationNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@applicationId@/: The application ID.
    -> [Gio.Flags.ApplicationFlags]
    -- ^ /@flags@/: the application flags
    -> m (Maybe Application)
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Application.Application' instance
applicationNew applicationId flags = liftIO $ do
    maybeApplicationId <- case applicationId of
        Nothing -> return nullPtr
        Just jApplicationId -> do
            jApplicationId' <- textToCString jApplicationId
            return jApplicationId'
    let flags' = gflagsToWord flags
    result <- gtk_application_new maybeApplicationId flags'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Application) result'
        return result''
    freeMem maybeApplicationId
    return maybeResult

#if defined(ENABLE_OVERLOADING)
#endif

-- method Application::add_accelerator
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "accelerator string" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the action to activate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parameter"
--           , argType = TVariant
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "parameter to pass when activating the action,\n  or %NULL if the action does not accept an activation parameter"
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

foreign import ccall "gtk_application_add_accelerator" gtk_application_add_accelerator :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    CString ->                              -- accelerator : TBasicType TUTF8
    CString ->                              -- action_name : TBasicType TUTF8
    Ptr GVariant ->                         -- parameter : TVariant
    IO ()

{-# DEPRECATED applicationAddAccelerator ["(Since version 3.14)","Use 'GI.Gtk.Objects.Application.applicationSetAccelsForAction' instead"] #-}
-- | Installs an accelerator that will cause the named action
-- to be activated when the key combination specificed by /@accelerator@/
-- is pressed.
-- 
-- /@accelerator@/ must be a string that can be parsed by 'GI.Gtk.Functions.acceleratorParse',
-- e.g. \"\<Primary>q\" or “\<Control>\<Alt>p”.
-- 
-- /@actionName@/ must be the name of an action as it would be used
-- in the app menu, i.e. actions that have been added to the application
-- are referred to with an “app.” prefix, and window-specific actions
-- with a “win.” prefix.
-- 
-- GtkApplication also extracts accelerators out of “accel” attributes
-- in the @/GMenuModels/@ passed to 'GI.Gtk.Objects.Application.applicationSetAppMenu' and
-- 'GI.Gtk.Objects.Application.applicationSetMenubar', which is usually more convenient
-- than calling this function for each accelerator.
-- 
-- /Since: 3.4/
applicationAddAccelerator ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> T.Text
    -- ^ /@accelerator@/: accelerator string
    -> T.Text
    -- ^ /@actionName@/: the name of the action to activate
    -> Maybe (GVariant)
    -- ^ /@parameter@/: parameter to pass when activating the action,
    --   or 'P.Nothing' if the action does not accept an activation parameter
    -> m ()
applicationAddAccelerator application accelerator actionName parameter = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    accelerator' <- textToCString accelerator
    actionName' <- textToCString actionName
    maybeParameter <- case parameter of
        Nothing -> return nullPtr
        Just jParameter -> do
            jParameter' <- unsafeManagedPtrGetPtr jParameter
            return jParameter'
    gtk_application_add_accelerator application' accelerator' actionName' maybeParameter
    touchManagedPtr application
    whenJust parameter touchManagedPtr
    freeMem accelerator'
    freeMem actionName'
    return ()

#if defined(ENABLE_OVERLOADING)
data ApplicationAddAcceleratorMethodInfo
instance (signature ~ (T.Text -> T.Text -> Maybe (GVariant) -> m ()), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationAddAcceleratorMethodInfo a signature where
    overloadedMethod = applicationAddAccelerator

instance O.OverloadedMethodInfo ApplicationAddAcceleratorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationAddAccelerator",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationAddAccelerator"
        })


#endif

-- method Application::add_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_application_add_window" gtk_application_add_window :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    Ptr Gtk.Window.Window ->                -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Adds a window to /@application@/.
-- 
-- This call can only happen after the /@application@/ has started;
-- typically, you should add new application windows in response
-- to the emission of the [Application::activate]("GI.Gio.Objects.Application#g:signal:activate") signal.
-- 
-- This call is equivalent to setting the [Window:application]("GI.Gtk.Objects.Window#g:attr:application")
-- property of /@window@/ to /@application@/.
-- 
-- Normally, the connection between the application and the window
-- will remain until the window is destroyed, but you can explicitly
-- remove it with 'GI.Gtk.Objects.Application.applicationRemoveWindow'.
-- 
-- GTK+ will keep the /@application@/ running as long as it has
-- any windows.
-- 
-- /Since: 3.0/
applicationAddWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a, Gtk.Window.IsWindow b) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> b
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
applicationAddWindow application window = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    window' <- unsafeManagedPtrCastPtr window
    gtk_application_add_window application' window'
    touchManagedPtr application
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data ApplicationAddWindowMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsApplication a, Gtk.Window.IsWindow b) => O.OverloadedMethod ApplicationAddWindowMethodInfo a signature where
    overloadedMethod = applicationAddWindow

instance O.OverloadedMethodInfo ApplicationAddWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationAddWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationAddWindow"
        })


#endif

-- method Application::get_accels_for_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detailed_action_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a detailed action name, specifying an action\n    and target to obtain accelerators for"
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
-- returnType: Just (TCArray True (-1) (-1) (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_get_accels_for_action" gtk_application_get_accels_for_action :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    CString ->                              -- detailed_action_name : TBasicType TUTF8
    IO (Ptr CString)

-- | Gets the accelerators that are currently associated with
-- the given action.
-- 
-- /Since: 3.12/
applicationGetAccelsForAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> T.Text
    -- ^ /@detailedActionName@/: a detailed action name, specifying an action
    --     and target to obtain accelerators for
    -> m [T.Text]
    -- ^ __Returns:__ accelerators for /@detailedActionName@/, as
    --     a 'P.Nothing'-terminated array. Free with 'GI.GLib.Functions.strfreev' when no longer needed
applicationGetAccelsForAction application detailedActionName = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    detailedActionName' <- textToCString detailedActionName
    result <- gtk_application_get_accels_for_action application' detailedActionName'
    checkUnexpectedReturnNULL "applicationGetAccelsForAction" result
    result' <- unpackZeroTerminatedUTF8CArray result
    mapZeroTerminatedCArray freeMem result
    freeMem result
    touchManagedPtr application
    freeMem detailedActionName'
    return result'

#if defined(ENABLE_OVERLOADING)
data ApplicationGetAccelsForActionMethodInfo
instance (signature ~ (T.Text -> m [T.Text]), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationGetAccelsForActionMethodInfo a signature where
    overloadedMethod = applicationGetAccelsForAction

instance O.OverloadedMethodInfo ApplicationGetAccelsForActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationGetAccelsForAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationGetAccelsForAction"
        })


#endif

-- method Application::get_actions_for_accel
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "an accelerator that can be parsed by gtk_accelerator_parse()"
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
-- returnType: Just (TCArray True (-1) (-1) (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_get_actions_for_accel" gtk_application_get_actions_for_accel :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    CString ->                              -- accel : TBasicType TUTF8
    IO (Ptr CString)

-- | Returns the list of actions (possibly empty) that /@accel@/ maps to.
-- Each item in the list is a detailed action name in the usual form.
-- 
-- This might be useful to discover if an accel already exists in
-- order to prevent installation of a conflicting accelerator (from
-- an accelerator editor or a plugin system, for example). Note that
-- having more than one action per accelerator may not be a bad thing
-- and might make sense in cases where the actions never appear in the
-- same context.
-- 
-- In case there are no actions for a given accelerator, an empty array
-- is returned.  'P.Nothing' is never returned.
-- 
-- It is a programmer error to pass an invalid accelerator string.
-- If you are unsure, check it with 'GI.Gtk.Functions.acceleratorParse' first.
-- 
-- /Since: 3.14/
applicationGetActionsForAccel ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> T.Text
    -- ^ /@accel@/: an accelerator that can be parsed by 'GI.Gtk.Functions.acceleratorParse'
    -> m [T.Text]
    -- ^ __Returns:__ a 'P.Nothing'-terminated array of actions for /@accel@/
applicationGetActionsForAccel application accel = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    accel' <- textToCString accel
    result <- gtk_application_get_actions_for_accel application' accel'
    checkUnexpectedReturnNULL "applicationGetActionsForAccel" result
    result' <- unpackZeroTerminatedUTF8CArray result
    mapZeroTerminatedCArray freeMem result
    freeMem result
    touchManagedPtr application
    freeMem accel'
    return result'

#if defined(ENABLE_OVERLOADING)
data ApplicationGetActionsForAccelMethodInfo
instance (signature ~ (T.Text -> m [T.Text]), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationGetActionsForAccelMethodInfo a signature where
    overloadedMethod = applicationGetActionsForAccel

instance O.OverloadedMethodInfo ApplicationGetActionsForAccelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationGetActionsForAccel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationGetActionsForAccel"
        })


#endif

-- method Application::get_active_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
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

foreign import ccall "gtk_application_get_active_window" gtk_application_get_active_window :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    IO (Ptr Gtk.Window.Window)

-- | Gets the “active” window for the application.
-- 
-- The active window is the one that was most recently focused (within
-- the application).  This window may not have the focus at the moment
-- if another application has it — this is just the most
-- recently-focused window within this application.
-- 
-- /Since: 3.6/
applicationGetActiveWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> m (Maybe Gtk.Window.Window)
    -- ^ __Returns:__ the active window, or 'P.Nothing' if
    --   there isn\'t one.
applicationGetActiveWindow application = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    result <- gtk_application_get_active_window application'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Window.Window) result'
        return result''
    touchManagedPtr application
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ApplicationGetActiveWindowMethodInfo
instance (signature ~ (m (Maybe Gtk.Window.Window)), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationGetActiveWindowMethodInfo a signature where
    overloadedMethod = applicationGetActiveWindow

instance O.OverloadedMethodInfo ApplicationGetActiveWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationGetActiveWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationGetActiveWindow"
        })


#endif

-- method Application::get_app_menu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "MenuModel" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_get_app_menu" gtk_application_get_app_menu :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    IO (Ptr Gio.MenuModel.MenuModel)

-- | Returns the menu model that has been set with
-- 'GI.Gtk.Objects.Application.applicationSetAppMenu'.
-- 
-- /Since: 3.4/
applicationGetAppMenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> m (Maybe Gio.MenuModel.MenuModel)
    -- ^ __Returns:__ the application menu of /@application@/
    --   or 'P.Nothing' if no application menu has been set.
applicationGetAppMenu application = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    result <- gtk_application_get_app_menu application'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gio.MenuModel.MenuModel) result'
        return result''
    touchManagedPtr application
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ApplicationGetAppMenuMethodInfo
instance (signature ~ (m (Maybe Gio.MenuModel.MenuModel)), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationGetAppMenuMethodInfo a signature where
    overloadedMethod = applicationGetAppMenu

instance O.OverloadedMethodInfo ApplicationGetAppMenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationGetAppMenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationGetAppMenu"
        })


#endif

-- method Application::get_menu_by_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the id of the menu to look up"
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
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "Menu" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_get_menu_by_id" gtk_application_get_menu_by_id :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    CString ->                              -- id : TBasicType TUTF8
    IO (Ptr Gio.Menu.Menu)

-- | Gets a menu from automatically loaded resources.
-- See [Automatic resources][automatic-resources]
-- for more information.
-- 
-- /Since: 3.14/
applicationGetMenuById ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> T.Text
    -- ^ /@id@/: the id of the menu to look up
    -> m Gio.Menu.Menu
    -- ^ __Returns:__ Gets the menu with the
    --     given id from the automatically loaded resources
applicationGetMenuById application id = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    id' <- textToCString id
    result <- gtk_application_get_menu_by_id application' id'
    checkUnexpectedReturnNULL "applicationGetMenuById" result
    result' <- (newObject Gio.Menu.Menu) result
    touchManagedPtr application
    freeMem id'
    return result'

#if defined(ENABLE_OVERLOADING)
data ApplicationGetMenuByIdMethodInfo
instance (signature ~ (T.Text -> m Gio.Menu.Menu), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationGetMenuByIdMethodInfo a signature where
    overloadedMethod = applicationGetMenuById

instance O.OverloadedMethodInfo ApplicationGetMenuByIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationGetMenuById",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationGetMenuById"
        })


#endif

-- method Application::get_menubar
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "MenuModel" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_get_menubar" gtk_application_get_menubar :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    IO (Ptr Gio.MenuModel.MenuModel)

-- | Returns the menu model that has been set with
-- 'GI.Gtk.Objects.Application.applicationSetMenubar'.
-- 
-- /Since: 3.4/
applicationGetMenubar ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> m Gio.MenuModel.MenuModel
    -- ^ __Returns:__ the menubar for windows of /@application@/
applicationGetMenubar application = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    result <- gtk_application_get_menubar application'
    checkUnexpectedReturnNULL "applicationGetMenubar" result
    result' <- (newObject Gio.MenuModel.MenuModel) result
    touchManagedPtr application
    return result'

#if defined(ENABLE_OVERLOADING)
data ApplicationGetMenubarMethodInfo
instance (signature ~ (m Gio.MenuModel.MenuModel), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationGetMenubarMethodInfo a signature where
    overloadedMethod = applicationGetMenubar

instance O.OverloadedMethodInfo ApplicationGetMenubarMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationGetMenubar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationGetMenubar"
        })


#endif

-- method Application::get_window_by_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "id"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an identifier number"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_get_window_by_id" gtk_application_get_window_by_id :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    Word32 ->                               -- id : TBasicType TUInt
    IO (Ptr Gtk.Window.Window)

-- | Returns the t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow' with the given ID.
-- 
-- The ID of a t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow' can be retrieved with
-- 'GI.Gtk.Objects.ApplicationWindow.applicationWindowGetId'.
-- 
-- /Since: 3.6/
applicationGetWindowById ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> Word32
    -- ^ /@id@/: an identifier number
    -> m (Maybe Gtk.Window.Window)
    -- ^ __Returns:__ the window with ID /@id@/, or
    --   'P.Nothing' if there is no window with this ID
applicationGetWindowById application id = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    result <- gtk_application_get_window_by_id application' id
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Window.Window) result'
        return result''
    touchManagedPtr application
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ApplicationGetWindowByIdMethodInfo
instance (signature ~ (Word32 -> m (Maybe Gtk.Window.Window)), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationGetWindowByIdMethodInfo a signature where
    overloadedMethod = applicationGetWindowById

instance O.OverloadedMethodInfo ApplicationGetWindowByIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationGetWindowById",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationGetWindowById"
        })


#endif

-- method Application::get_windows
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Window" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_get_windows" gtk_application_get_windows :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    IO (Ptr (GList (Ptr Gtk.Window.Window)))

-- | Gets a list of the @/GtkWindows/@ associated with /@application@/.
-- 
-- The list is sorted by most recently focused window, such that the first
-- element is the currently focused window. (Useful for choosing a parent
-- for a transient window.)
-- 
-- The list that is returned should not be modified in any way. It will
-- only remain valid until the next focus change or window creation or
-- deletion.
-- 
-- /Since: 3.0/
applicationGetWindows ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> m [Gtk.Window.Window]
    -- ^ __Returns:__ a t'GI.GLib.Structs.List.List' of t'GI.Gtk.Objects.Window.Window'
applicationGetWindows application = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    result <- gtk_application_get_windows application'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.Window.Window) result'
    touchManagedPtr application
    return result''

#if defined(ENABLE_OVERLOADING)
data ApplicationGetWindowsMethodInfo
instance (signature ~ (m [Gtk.Window.Window]), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationGetWindowsMethodInfo a signature where
    overloadedMethod = applicationGetWindows

instance O.OverloadedMethodInfo ApplicationGetWindowsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationGetWindows",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationGetWindows"
        })


#endif

-- method Application::inhibit
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkApplication"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow, or %NULL"
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
--               TInterface
--                 Name { namespace = "Gtk" , name = "ApplicationInhibitFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "what types of actions should be inhibited"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "reason"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a short, human-readable string that explains\n    why these operations are inhibited"
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
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_inhibit" gtk_application_inhibit :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    Ptr Gtk.Window.Window ->                -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "ApplicationInhibitFlags"})
    CString ->                              -- reason : TBasicType TUTF8
    IO Word32

-- | Inform the session manager that certain types of actions should be
-- inhibited. This is not guaranteed to work on all platforms and for
-- all types of actions.
-- 
-- Applications should invoke this method when they begin an operation
-- that should not be interrupted, such as creating a CD or DVD. The
-- types of actions that may be blocked are specified by the /@flags@/
-- parameter. When the application completes the operation it should
-- call 'GI.Gtk.Objects.Application.applicationUninhibit' to remove the inhibitor. Note that
-- an application can have multiple inhibitors, and all of them must
-- be individually removed. Inhibitors are also cleared when the
-- application exits.
-- 
-- Applications should not expect that they will always be able to block
-- the action. In most cases, users will be given the option to force
-- the action to take place.
-- 
-- Reasons should be short and to the point.
-- 
-- If /@window@/ is given, the session manager may point the user to
-- this window to find out more about why the action is inhibited.
-- 
-- /Since: 3.4/
applicationInhibit ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a, Gtk.Window.IsWindow b) =>
    a
    -- ^ /@application@/: the t'GI.Gtk.Objects.Application.Application'
    -> Maybe (b)
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window', or 'P.Nothing'
    -> [Gtk.Flags.ApplicationInhibitFlags]
    -- ^ /@flags@/: what types of actions should be inhibited
    -> Maybe (T.Text)
    -- ^ /@reason@/: a short, human-readable string that explains
    --     why these operations are inhibited
    -> m Word32
    -- ^ __Returns:__ A non-zero cookie that is used to uniquely identify this
    --     request. It should be used as an argument to 'GI.Gtk.Objects.Application.applicationUninhibit'
    --     in order to remove the request. If the platform does not support
    --     inhibiting or the request failed for some reason, 0 is returned.
applicationInhibit application window flags reason = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    maybeWindow <- case window of
        Nothing -> return nullPtr
        Just jWindow -> do
            jWindow' <- unsafeManagedPtrCastPtr jWindow
            return jWindow'
    let flags' = gflagsToWord flags
    maybeReason <- case reason of
        Nothing -> return nullPtr
        Just jReason -> do
            jReason' <- textToCString jReason
            return jReason'
    result <- gtk_application_inhibit application' maybeWindow flags' maybeReason
    touchManagedPtr application
    whenJust window touchManagedPtr
    freeMem maybeReason
    return result

#if defined(ENABLE_OVERLOADING)
data ApplicationInhibitMethodInfo
instance (signature ~ (Maybe (b) -> [Gtk.Flags.ApplicationInhibitFlags] -> Maybe (T.Text) -> m Word32), MonadIO m, IsApplication a, Gtk.Window.IsWindow b) => O.OverloadedMethod ApplicationInhibitMethodInfo a signature where
    overloadedMethod = applicationInhibit

instance O.OverloadedMethodInfo ApplicationInhibitMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationInhibit",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationInhibit"
        })


#endif

-- method Application::is_inhibited
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkApplication"
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
--               TInterface
--                 Name { namespace = "Gtk" , name = "ApplicationInhibitFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "what types of actions should be queried"
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

foreign import ccall "gtk_application_is_inhibited" gtk_application_is_inhibited :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "ApplicationInhibitFlags"})
    IO CInt

-- | Determines if any of the actions specified in /@flags@/ are
-- currently inhibited (possibly by another application).
-- 
-- Note that this information may not be available (for example
-- when the application is running in a sandbox).
-- 
-- /Since: 3.4/
applicationIsInhibited ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: the t'GI.Gtk.Objects.Application.Application'
    -> [Gtk.Flags.ApplicationInhibitFlags]
    -- ^ /@flags@/: what types of actions should be queried
    -> m Bool
    -- ^ __Returns:__ 'P.True' if any of the actions specified in /@flags@/ are inhibited
applicationIsInhibited application flags = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    let flags' = gflagsToWord flags
    result <- gtk_application_is_inhibited application' flags'
    let result' = (/= 0) result
    touchManagedPtr application
    return result'

#if defined(ENABLE_OVERLOADING)
data ApplicationIsInhibitedMethodInfo
instance (signature ~ ([Gtk.Flags.ApplicationInhibitFlags] -> m Bool), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationIsInhibitedMethodInfo a signature where
    overloadedMethod = applicationIsInhibited

instance O.OverloadedMethodInfo ApplicationIsInhibitedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationIsInhibited",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationIsInhibited"
        })


#endif

-- method Application::list_action_descriptions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TCArray True (-1) (-1) (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_application_list_action_descriptions" gtk_application_list_action_descriptions :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    IO (Ptr CString)

-- | Lists the detailed action names which have associated accelerators.
-- See 'GI.Gtk.Objects.Application.applicationSetAccelsForAction'.
-- 
-- /Since: 3.12/
applicationListActionDescriptions ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> m [T.Text]
    -- ^ __Returns:__ a 'P.Nothing'-terminated array of strings,
    --     free with 'GI.GLib.Functions.strfreev' when done
applicationListActionDescriptions application = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    result <- gtk_application_list_action_descriptions application'
    checkUnexpectedReturnNULL "applicationListActionDescriptions" result
    result' <- unpackZeroTerminatedUTF8CArray result
    mapZeroTerminatedCArray freeMem result
    freeMem result
    touchManagedPtr application
    return result'

#if defined(ENABLE_OVERLOADING)
data ApplicationListActionDescriptionsMethodInfo
instance (signature ~ (m [T.Text]), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationListActionDescriptionsMethodInfo a signature where
    overloadedMethod = applicationListActionDescriptions

instance O.OverloadedMethodInfo ApplicationListActionDescriptionsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationListActionDescriptions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationListActionDescriptions"
        })


#endif

-- method Application::prefers_app_menu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
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

foreign import ccall "gtk_application_prefers_app_menu" gtk_application_prefers_app_menu :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    IO CInt

-- | Determines if the desktop environment in which the application is
-- running would prefer an application menu be shown.
-- 
-- If this function returns 'P.True' then the application should call
-- 'GI.Gtk.Objects.Application.applicationSetAppMenu' with the contents of an application
-- menu, which will be shown by the desktop environment.  If it returns
-- 'P.False' then you should consider using an alternate approach, such as
-- a menubar.
-- 
-- The value returned by this function is purely advisory and you are
-- free to ignore it.  If you call 'GI.Gtk.Objects.Application.applicationSetAppMenu' even
-- if the desktop environment doesn\'t support app menus, then a fallback
-- will be provided.
-- 
-- Applications are similarly free not to set an app menu even if the
-- desktop environment wants to show one.  In that case, a fallback will
-- also be created by the desktop environment (GNOME, for example, uses
-- a menu with only a \"Quit\" item in it).
-- 
-- The value returned by this function never changes.  Once it returns a
-- particular value, it is guaranteed to always return the same value.
-- 
-- You may only call this function after the application has been
-- registered and after the base startup handler has run.  You\'re most
-- likely to want to use this from your own startup handler.  It may
-- also make sense to consult this function while constructing UI (in
-- activate, open or an action activation handler) in order to determine
-- if you should show a gear menu or not.
-- 
-- This function will return 'P.False' on Mac OS and a default app menu
-- will be created automatically with the \"usual\" contents of that menu
-- typical to most Mac OS applications.  If you call
-- 'GI.Gtk.Objects.Application.applicationSetAppMenu' anyway, then this menu will be
-- replaced with your own.
-- 
-- /Since: 3.14/
applicationPrefersAppMenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if you should set an app menu
applicationPrefersAppMenu application = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    result <- gtk_application_prefers_app_menu application'
    let result' = (/= 0) result
    touchManagedPtr application
    return result'

#if defined(ENABLE_OVERLOADING)
data ApplicationPrefersAppMenuMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationPrefersAppMenuMethodInfo a signature where
    overloadedMethod = applicationPrefersAppMenu

instance O.OverloadedMethodInfo ApplicationPrefersAppMenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationPrefersAppMenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationPrefersAppMenu"
        })


#endif

-- method Application::remove_accelerator
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the action to activate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parameter"
--           , argType = TVariant
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "parameter to pass when activating the action,\n  or %NULL if the action does not accept an activation parameter"
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

foreign import ccall "gtk_application_remove_accelerator" gtk_application_remove_accelerator :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    CString ->                              -- action_name : TBasicType TUTF8
    Ptr GVariant ->                         -- parameter : TVariant
    IO ()

{-# DEPRECATED applicationRemoveAccelerator ["(Since version 3.14)","Use 'GI.Gtk.Objects.Application.applicationSetAccelsForAction' instead"] #-}
-- | Removes an accelerator that has been previously added
-- with 'GI.Gtk.Objects.Application.applicationAddAccelerator'.
-- 
-- /Since: 3.4/
applicationRemoveAccelerator ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> T.Text
    -- ^ /@actionName@/: the name of the action to activate
    -> Maybe (GVariant)
    -- ^ /@parameter@/: parameter to pass when activating the action,
    --   or 'P.Nothing' if the action does not accept an activation parameter
    -> m ()
applicationRemoveAccelerator application actionName parameter = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    actionName' <- textToCString actionName
    maybeParameter <- case parameter of
        Nothing -> return nullPtr
        Just jParameter -> do
            jParameter' <- unsafeManagedPtrGetPtr jParameter
            return jParameter'
    gtk_application_remove_accelerator application' actionName' maybeParameter
    touchManagedPtr application
    whenJust parameter touchManagedPtr
    freeMem actionName'
    return ()

#if defined(ENABLE_OVERLOADING)
data ApplicationRemoveAcceleratorMethodInfo
instance (signature ~ (T.Text -> Maybe (GVariant) -> m ()), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationRemoveAcceleratorMethodInfo a signature where
    overloadedMethod = applicationRemoveAccelerator

instance O.OverloadedMethodInfo ApplicationRemoveAcceleratorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationRemoveAccelerator",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationRemoveAccelerator"
        })


#endif

-- method Application::remove_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_application_remove_window" gtk_application_remove_window :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    Ptr Gtk.Window.Window ->                -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Remove a window from /@application@/.
-- 
-- If /@window@/ belongs to /@application@/ then this call is equivalent to
-- setting the [Window:application]("GI.Gtk.Objects.Window#g:attr:application") property of /@window@/ to
-- 'P.Nothing'.
-- 
-- The application may stop running as a result of a call to this
-- function.
-- 
-- /Since: 3.0/
applicationRemoveWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a, Gtk.Window.IsWindow b) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> b
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
applicationRemoveWindow application window = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    window' <- unsafeManagedPtrCastPtr window
    gtk_application_remove_window application' window'
    touchManagedPtr application
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data ApplicationRemoveWindowMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsApplication a, Gtk.Window.IsWindow b) => O.OverloadedMethod ApplicationRemoveWindowMethodInfo a signature where
    overloadedMethod = applicationRemoveWindow

instance O.OverloadedMethodInfo ApplicationRemoveWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationRemoveWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationRemoveWindow"
        })


#endif

-- method Application::set_accels_for_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detailed_action_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a detailed action name, specifying an action\n    and target to associate accelerators with"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accels"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a list of accelerators in the format\n    understood by gtk_accelerator_parse()"
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

foreign import ccall "gtk_application_set_accels_for_action" gtk_application_set_accels_for_action :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    CString ->                              -- detailed_action_name : TBasicType TUTF8
    Ptr CString ->                          -- accels : TCArray True (-1) (-1) (TBasicType TUTF8)
    IO ()

-- | Sets zero or more keyboard accelerators that will trigger the
-- given action. The first item in /@accels@/ will be the primary
-- accelerator, which may be displayed in the UI.
-- 
-- To remove all accelerators for an action, use an empty, zero-terminated
-- array for /@accels@/.
-- 
-- For the /@detailedActionName@/, see 'GI.Gio.Functions.actionParseDetailedName' and
-- 'GI.Gio.Functions.actionPrintDetailedName'.
-- 
-- /Since: 3.12/
applicationSetAccelsForAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> T.Text
    -- ^ /@detailedActionName@/: a detailed action name, specifying an action
    --     and target to associate accelerators with
    -> [T.Text]
    -- ^ /@accels@/: a list of accelerators in the format
    --     understood by 'GI.Gtk.Functions.acceleratorParse'
    -> m ()
applicationSetAccelsForAction application detailedActionName accels = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    detailedActionName' <- textToCString detailedActionName
    accels' <- packZeroTerminatedUTF8CArray accels
    gtk_application_set_accels_for_action application' detailedActionName' accels'
    touchManagedPtr application
    freeMem detailedActionName'
    mapZeroTerminatedCArray freeMem accels'
    freeMem accels'
    return ()

#if defined(ENABLE_OVERLOADING)
data ApplicationSetAccelsForActionMethodInfo
instance (signature ~ (T.Text -> [T.Text] -> m ()), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationSetAccelsForActionMethodInfo a signature where
    overloadedMethod = applicationSetAccelsForAction

instance O.OverloadedMethodInfo ApplicationSetAccelsForActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationSetAccelsForAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationSetAccelsForAction"
        })


#endif

-- method Application::set_app_menu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "app_menu"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "MenuModel" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GMenuModel, or %NULL"
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

foreign import ccall "gtk_application_set_app_menu" gtk_application_set_app_menu :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    Ptr Gio.MenuModel.MenuModel ->          -- app_menu : TInterface (Name {namespace = "Gio", name = "MenuModel"})
    IO ()

-- | Sets or unsets the application menu for /@application@/.
-- 
-- This can only be done in the primary instance of the application,
-- after it has been registered.  [Application::startup]("GI.Gio.Objects.Application#g:signal:startup") is a good place
-- to call this.
-- 
-- The application menu is a single menu containing items that typically
-- impact the application as a whole, rather than acting on a specific
-- window or document.  For example, you would expect to see
-- “Preferences” or “Quit” in an application menu, but not “Save” or
-- “Print”.
-- 
-- If supported, the application menu will be rendered by the desktop
-- environment.
-- 
-- Use the base t'GI.Gio.Interfaces.ActionMap.ActionMap' interface to add actions, to respond to the user
-- selecting these menu items.
-- 
-- /Since: 3.4/
applicationSetAppMenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a, Gio.MenuModel.IsMenuModel b) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> Maybe (b)
    -- ^ /@appMenu@/: a t'GI.Gio.Objects.MenuModel.MenuModel', or 'P.Nothing'
    -> m ()
applicationSetAppMenu application appMenu = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    maybeAppMenu <- case appMenu of
        Nothing -> return nullPtr
        Just jAppMenu -> do
            jAppMenu' <- unsafeManagedPtrCastPtr jAppMenu
            return jAppMenu'
    gtk_application_set_app_menu application' maybeAppMenu
    touchManagedPtr application
    whenJust appMenu touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ApplicationSetAppMenuMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsApplication a, Gio.MenuModel.IsMenuModel b) => O.OverloadedMethod ApplicationSetAppMenuMethodInfo a signature where
    overloadedMethod = applicationSetAppMenu

instance O.OverloadedMethodInfo ApplicationSetAppMenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationSetAppMenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationSetAppMenu"
        })


#endif

-- method Application::set_menubar
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menubar"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "MenuModel" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GMenuModel, or %NULL"
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

foreign import ccall "gtk_application_set_menubar" gtk_application_set_menubar :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    Ptr Gio.MenuModel.MenuModel ->          -- menubar : TInterface (Name {namespace = "Gio", name = "MenuModel"})
    IO ()

-- | Sets or unsets the menubar for windows of /@application@/.
-- 
-- This is a menubar in the traditional sense.
-- 
-- This can only be done in the primary instance of the application,
-- after it has been registered.  [Application::startup]("GI.Gio.Objects.Application#g:signal:startup") is a good place
-- to call this.
-- 
-- Depending on the desktop environment, this may appear at the top of
-- each window, or at the top of the screen.  In some environments, if
-- both the application menu and the menubar are set, the application
-- menu will be presented as if it were the first item of the menubar.
-- Other environments treat the two as completely separate — for example,
-- the application menu may be rendered by the desktop shell while the
-- menubar (if set) remains in each individual window.
-- 
-- Use the base t'GI.Gio.Interfaces.ActionMap.ActionMap' interface to add actions, to respond to the
-- user selecting these menu items.
-- 
-- /Since: 3.4/
applicationSetMenubar ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a, Gio.MenuModel.IsMenuModel b) =>
    a
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application'
    -> Maybe (b)
    -- ^ /@menubar@/: a t'GI.Gio.Objects.MenuModel.MenuModel', or 'P.Nothing'
    -> m ()
applicationSetMenubar application menubar = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    maybeMenubar <- case menubar of
        Nothing -> return nullPtr
        Just jMenubar -> do
            jMenubar' <- unsafeManagedPtrCastPtr jMenubar
            return jMenubar'
    gtk_application_set_menubar application' maybeMenubar
    touchManagedPtr application
    whenJust menubar touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ApplicationSetMenubarMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsApplication a, Gio.MenuModel.IsMenuModel b) => O.OverloadedMethod ApplicationSetMenubarMethodInfo a signature where
    overloadedMethod = applicationSetMenubar

instance O.OverloadedMethodInfo ApplicationSetMenubarMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationSetMenubar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationSetMenubar"
        })


#endif

-- method Application::uninhibit
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkApplication"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cookie"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a cookie that was returned by gtk_application_inhibit()"
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

foreign import ccall "gtk_application_uninhibit" gtk_application_uninhibit :: 
    Ptr Application ->                      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    Word32 ->                               -- cookie : TBasicType TUInt
    IO ()

-- | Removes an inhibitor that has been established with 'GI.Gtk.Objects.Application.applicationInhibit'.
-- Inhibitors are also cleared when the application exits.
-- 
-- /Since: 3.4/
applicationUninhibit ::
    (B.CallStack.HasCallStack, MonadIO m, IsApplication a) =>
    a
    -- ^ /@application@/: the t'GI.Gtk.Objects.Application.Application'
    -> Word32
    -- ^ /@cookie@/: a cookie that was returned by 'GI.Gtk.Objects.Application.applicationInhibit'
    -> m ()
applicationUninhibit application cookie = liftIO $ do
    application' <- unsafeManagedPtrCastPtr application
    gtk_application_uninhibit application' cookie
    touchManagedPtr application
    return ()

#if defined(ENABLE_OVERLOADING)
data ApplicationUninhibitMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsApplication a) => O.OverloadedMethod ApplicationUninhibitMethodInfo a signature where
    overloadedMethod = applicationUninhibit

instance O.OverloadedMethodInfo ApplicationUninhibitMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Application.applicationUninhibit",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Application.html#v:applicationUninhibit"
        })


#endif


