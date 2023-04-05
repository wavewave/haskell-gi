{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Actions are organised into groups. An action group is essentially a
-- map from names to t'GI.Gtk.Objects.Action.Action' objects.
-- 
-- All actions that would make sense to use in a particular context
-- should be in a single group. Multiple action groups may be used for a
-- particular user interface. In fact, it is expected that most nontrivial
-- applications will make use of multiple groups. For example, in an
-- application that can edit multiple documents, one group holding global
-- actions (e.g. quit, about, new), and one group per document holding
-- actions that act on that document (eg. save, cut\/copy\/paste, etc). Each
-- window’s menus would be constructed from a combination of two action
-- groups.
-- 
-- ## Accelerators ## {@/Action/@-Accel}
-- 
-- Accelerators are handled by the GTK+ accelerator map. All actions are
-- assigned an accelerator path (which normally has the form
-- @\<Actions>\/group-name\/action-name@) and a shortcut is associated with
-- this accelerator path. All menuitems and toolitems take on this accelerator
-- path. The GTK+ accelerator map code makes sure that the correct shortcut
-- is displayed next to the menu item.
-- 
-- # GtkActionGroup as GtkBuildable # {t'GI.Gtk.Objects.ActionGroup.ActionGroup'-BUILDER-UI}
-- 
-- The t'GI.Gtk.Objects.ActionGroup.ActionGroup' implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface accepts
-- t'GI.Gtk.Objects.Action.Action' objects as @\<child>@ elements in UI definitions.
-- 
-- Note that it is probably more common to define actions and action groups
-- in the code, since they are directly related to what the code can do.
-- 
-- The GtkActionGroup implementation of the GtkBuildable interface supports
-- a custom @\<accelerator>@ element, which has attributes named “key“ and
-- “modifiers“ and allows to specify accelerators. This is similar to the
-- @\<accelerator>@ element of t'GI.Gtk.Objects.Widget.Widget', the main difference is that
-- it doesn’t allow you to specify a signal.
-- 
-- ## A t'GI.Gtk.Objects.Dialog.Dialog' UI definition fragment. ##
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkActionGroup" id="actiongroup">
-- >  <child>
-- >      <object class="GtkAction" id="About">
-- >          <property name="name">About</property>
-- >          <property name="stock_id">gtk-about</property>
-- >          <signal handler="about_activate" name="activate"/>
-- >      </object>
-- >      <accelerator key="F1" modifiers="GDK_CONTROL_MASK | GDK_SHIFT_MASK"/>
-- >  </child>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ActionGroup
    ( 

-- * Exported types
    ActionGroup(..)                         ,
    IsActionGroup                           ,
    toActionGroup                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addAction]("GI.Gtk.Objects.ActionGroup#g:method:addAction"), [addActionWithAccel]("GI.Gtk.Objects.ActionGroup#g:method:addActionWithAccel"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [listActions]("GI.Gtk.Objects.ActionGroup#g:method:listActions"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [removeAction]("GI.Gtk.Objects.ActionGroup#g:method:removeAction"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateString]("GI.Gtk.Objects.ActionGroup#g:method:translateString"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccelGroup]("GI.Gtk.Objects.ActionGroup#g:method:getAccelGroup"), [getAction]("GI.Gtk.Objects.ActionGroup#g:method:getAction"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getName]("GI.Gtk.Objects.ActionGroup#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSensitive]("GI.Gtk.Objects.ActionGroup#g:method:getSensitive"), [getVisible]("GI.Gtk.Objects.ActionGroup#g:method:getVisible").
-- 
-- ==== Setters
-- [setAccelGroup]("GI.Gtk.Objects.ActionGroup#g:method:setAccelGroup"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSensitive]("GI.Gtk.Objects.ActionGroup#g:method:setSensitive"), [setTranslateFunc]("GI.Gtk.Objects.ActionGroup#g:method:setTranslateFunc"), [setTranslationDomain]("GI.Gtk.Objects.ActionGroup#g:method:setTranslationDomain"), [setVisible]("GI.Gtk.Objects.ActionGroup#g:method:setVisible").

#if defined(ENABLE_OVERLOADING)
    ResolveActionGroupMethod                ,
#endif

-- ** addAction #method:addAction#

#if defined(ENABLE_OVERLOADING)
    ActionGroupAddActionMethodInfo          ,
#endif
    actionGroupAddAction                    ,


-- ** addActionWithAccel #method:addActionWithAccel#

#if defined(ENABLE_OVERLOADING)
    ActionGroupAddActionWithAccelMethodInfo ,
#endif
    actionGroupAddActionWithAccel           ,


-- ** getAccelGroup #method:getAccelGroup#

#if defined(ENABLE_OVERLOADING)
    ActionGroupGetAccelGroupMethodInfo      ,
#endif
    actionGroupGetAccelGroup                ,


-- ** getAction #method:getAction#

#if defined(ENABLE_OVERLOADING)
    ActionGroupGetActionMethodInfo          ,
#endif
    actionGroupGetAction                    ,


-- ** getName #method:getName#

#if defined(ENABLE_OVERLOADING)
    ActionGroupGetNameMethodInfo            ,
#endif
    actionGroupGetName                      ,


-- ** getSensitive #method:getSensitive#

#if defined(ENABLE_OVERLOADING)
    ActionGroupGetSensitiveMethodInfo       ,
#endif
    actionGroupGetSensitive                 ,


-- ** getVisible #method:getVisible#

#if defined(ENABLE_OVERLOADING)
    ActionGroupGetVisibleMethodInfo         ,
#endif
    actionGroupGetVisible                   ,


-- ** listActions #method:listActions#

#if defined(ENABLE_OVERLOADING)
    ActionGroupListActionsMethodInfo        ,
#endif
    actionGroupListActions                  ,


-- ** new #method:new#

    actionGroupNew                          ,


-- ** removeAction #method:removeAction#

#if defined(ENABLE_OVERLOADING)
    ActionGroupRemoveActionMethodInfo       ,
#endif
    actionGroupRemoveAction                 ,


-- ** setAccelGroup #method:setAccelGroup#

#if defined(ENABLE_OVERLOADING)
    ActionGroupSetAccelGroupMethodInfo      ,
#endif
    actionGroupSetAccelGroup                ,


-- ** setSensitive #method:setSensitive#

#if defined(ENABLE_OVERLOADING)
    ActionGroupSetSensitiveMethodInfo       ,
#endif
    actionGroupSetSensitive                 ,


-- ** setTranslateFunc #method:setTranslateFunc#

#if defined(ENABLE_OVERLOADING)
    ActionGroupSetTranslateFuncMethodInfo   ,
#endif
    actionGroupSetTranslateFunc             ,


-- ** setTranslationDomain #method:setTranslationDomain#

#if defined(ENABLE_OVERLOADING)
    ActionGroupSetTranslationDomainMethodInfo,
#endif
    actionGroupSetTranslationDomain         ,


-- ** setVisible #method:setVisible#

#if defined(ENABLE_OVERLOADING)
    ActionGroupSetVisibleMethodInfo         ,
#endif
    actionGroupSetVisible                   ,


-- ** translateString #method:translateString#

#if defined(ENABLE_OVERLOADING)
    ActionGroupTranslateStringMethodInfo    ,
#endif
    actionGroupTranslateString              ,




 -- * Properties


-- ** accelGroup #attr:accelGroup#
-- | The accelerator group the actions of this group should use.

#if defined(ENABLE_OVERLOADING)
    ActionGroupAccelGroupPropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    actionGroupAccelGroup                   ,
#endif
    clearActionGroupAccelGroup              ,
    constructActionGroupAccelGroup          ,
    getActionGroupAccelGroup                ,
    setActionGroupAccelGroup                ,


-- ** name #attr:name#
-- | A name for the action.

#if defined(ENABLE_OVERLOADING)
    ActionGroupNamePropertyInfo             ,
#endif
#if defined(ENABLE_OVERLOADING)
    actionGroupName                         ,
#endif
    constructActionGroupName                ,
    getActionGroupName                      ,


-- ** sensitive #attr:sensitive#
-- | Whether the action group is enabled.

#if defined(ENABLE_OVERLOADING)
    ActionGroupSensitivePropertyInfo        ,
#endif
#if defined(ENABLE_OVERLOADING)
    actionGroupSensitive                    ,
#endif
    constructActionGroupSensitive           ,
    getActionGroupSensitive                 ,
    setActionGroupSensitive                 ,


-- ** visible #attr:visible#
-- | Whether the action group is visible.

#if defined(ENABLE_OVERLOADING)
    ActionGroupVisiblePropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    actionGroupVisible                      ,
#endif
    constructActionGroupVisible             ,
    getActionGroupVisible                   ,
    setActionGroupVisible                   ,




 -- * Signals


-- ** connectProxy #signal:connectProxy#

    ActionGroupConnectProxyCallback         ,
#if defined(ENABLE_OVERLOADING)
    ActionGroupConnectProxySignalInfo       ,
#endif
    afterActionGroupConnectProxy            ,
    onActionGroupConnectProxy               ,


-- ** disconnectProxy #signal:disconnectProxy#

    ActionGroupDisconnectProxyCallback      ,
#if defined(ENABLE_OVERLOADING)
    ActionGroupDisconnectProxySignalInfo    ,
#endif
    afterActionGroupDisconnectProxy         ,
    onActionGroupDisconnectProxy            ,


-- ** postActivate #signal:postActivate#

    ActionGroupPostActivateCallback         ,
#if defined(ENABLE_OVERLOADING)
    ActionGroupPostActivateSignalInfo       ,
#endif
    afterActionGroupPostActivate            ,
    onActionGroupPostActivate               ,


-- ** preActivate #signal:preActivate#

    ActionGroupPreActivateCallback          ,
#if defined(ENABLE_OVERLOADING)
    ActionGroupPreActivateSignalInfo        ,
#endif
    afterActionGroupPreActivate             ,
    onActionGroupPreActivate                ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.AccelGroup as Gtk.AccelGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.Action as Gtk.Action
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype ActionGroup = ActionGroup (SP.ManagedPtr ActionGroup)
    deriving (Eq)

instance SP.ManagedPtrNewtype ActionGroup where
    toManagedPtr (ActionGroup p) = p

foreign import ccall "gtk_action_group_get_type"
    c_gtk_action_group_get_type :: IO B.Types.GType

instance B.Types.TypedObject ActionGroup where
    glibType = c_gtk_action_group_get_type

instance B.Types.GObject ActionGroup

-- | Type class for types which can be safely cast to `ActionGroup`, for instance with `toActionGroup`.
class (SP.GObject o, O.IsDescendantOf ActionGroup o) => IsActionGroup o
instance (SP.GObject o, O.IsDescendantOf ActionGroup o) => IsActionGroup o

instance O.HasParentTypes ActionGroup
type instance O.ParentTypes ActionGroup = '[GObject.Object.Object, Gtk.Buildable.Buildable]

-- | Cast to `ActionGroup`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toActionGroup :: (MIO.MonadIO m, IsActionGroup o) => o -> m ActionGroup
toActionGroup = MIO.liftIO . B.ManagedPtr.unsafeCastTo ActionGroup

-- | Convert 'ActionGroup' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ActionGroup) where
    gvalueGType_ = c_gtk_action_group_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ActionGroup)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ActionGroup)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ActionGroup ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveActionGroupMethod (t :: Symbol) (o :: *) :: * where
    ResolveActionGroupMethod "addAction" o = ActionGroupAddActionMethodInfo
    ResolveActionGroupMethod "addActionWithAccel" o = ActionGroupAddActionWithAccelMethodInfo
    ResolveActionGroupMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveActionGroupMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveActionGroupMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveActionGroupMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveActionGroupMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveActionGroupMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveActionGroupMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveActionGroupMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveActionGroupMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveActionGroupMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveActionGroupMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveActionGroupMethod "listActions" o = ActionGroupListActionsMethodInfo
    ResolveActionGroupMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveActionGroupMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveActionGroupMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveActionGroupMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveActionGroupMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveActionGroupMethod "removeAction" o = ActionGroupRemoveActionMethodInfo
    ResolveActionGroupMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveActionGroupMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveActionGroupMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveActionGroupMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveActionGroupMethod "translateString" o = ActionGroupTranslateStringMethodInfo
    ResolveActionGroupMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveActionGroupMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveActionGroupMethod "getAccelGroup" o = ActionGroupGetAccelGroupMethodInfo
    ResolveActionGroupMethod "getAction" o = ActionGroupGetActionMethodInfo
    ResolveActionGroupMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveActionGroupMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveActionGroupMethod "getName" o = ActionGroupGetNameMethodInfo
    ResolveActionGroupMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveActionGroupMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveActionGroupMethod "getSensitive" o = ActionGroupGetSensitiveMethodInfo
    ResolveActionGroupMethod "getVisible" o = ActionGroupGetVisibleMethodInfo
    ResolveActionGroupMethod "setAccelGroup" o = ActionGroupSetAccelGroupMethodInfo
    ResolveActionGroupMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveActionGroupMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveActionGroupMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveActionGroupMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveActionGroupMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveActionGroupMethod "setSensitive" o = ActionGroupSetSensitiveMethodInfo
    ResolveActionGroupMethod "setTranslateFunc" o = ActionGroupSetTranslateFuncMethodInfo
    ResolveActionGroupMethod "setTranslationDomain" o = ActionGroupSetTranslationDomainMethodInfo
    ResolveActionGroupMethod "setVisible" o = ActionGroupSetVisibleMethodInfo
    ResolveActionGroupMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveActionGroupMethod t ActionGroup, O.OverloadedMethod info ActionGroup p) => OL.IsLabel t (ActionGroup -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveActionGroupMethod t ActionGroup, O.OverloadedMethod info ActionGroup p, R.HasField t ActionGroup p) => R.HasField t ActionGroup p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveActionGroupMethod t ActionGroup, O.OverloadedMethodInfo info ActionGroup) => OL.IsLabel t (O.MethodProxy info ActionGroup) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal ActionGroup::connect-proxy
{-# DEPRECATED ActionGroupConnectProxyCallback ["(Since version 3.10)"] #-}
-- | The [connectProxy](#g:signal:connectProxy) signal is emitted after connecting a proxy to
-- an action in the group. Note that the proxy may have been connected
-- to a different action before.
-- 
-- This is intended for simple customizations for which a custom action
-- class would be too clumsy, e.g. showing tooltips for menuitems in the
-- statusbar.
-- 
-- t'GI.Gtk.Objects.UIManager.UIManager' proxies the signal and provides global notification
-- just before any action is connected to a proxy, which is probably more
-- convenient to use.
-- 
-- /Since: 2.4/
type ActionGroupConnectProxyCallback =
    Gtk.Action.Action
    -- ^ /@action@/: the action
    -> Gtk.Widget.Widget
    -- ^ /@proxy@/: the proxy
    -> IO ()

type C_ActionGroupConnectProxyCallback =
    Ptr ActionGroup ->                      -- object
    Ptr Gtk.Action.Action ->
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ActionGroupConnectProxyCallback`.
foreign import ccall "wrapper"
    mk_ActionGroupConnectProxyCallback :: C_ActionGroupConnectProxyCallback -> IO (FunPtr C_ActionGroupConnectProxyCallback)

wrap_ActionGroupConnectProxyCallback :: 
    GObject a => (a -> ActionGroupConnectProxyCallback) ->
    C_ActionGroupConnectProxyCallback
wrap_ActionGroupConnectProxyCallback gi'cb gi'selfPtr action proxy _ = do
    action' <- (newObject Gtk.Action.Action) action
    proxy' <- (newObject Gtk.Widget.Widget) proxy
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  action' proxy'


-- | Connect a signal handler for the [connectProxy](#signal:connectProxy) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' actionGroup #connectProxy callback
-- @
-- 
-- 
onActionGroupConnectProxy :: (IsActionGroup a, MonadIO m) => a -> ((?self :: a) => ActionGroupConnectProxyCallback) -> m SignalHandlerId
onActionGroupConnectProxy obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ActionGroupConnectProxyCallback wrapped
    wrapped'' <- mk_ActionGroupConnectProxyCallback wrapped'
    connectSignalFunPtr obj "connect-proxy" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [connectProxy](#signal:connectProxy) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' actionGroup #connectProxy callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterActionGroupConnectProxy :: (IsActionGroup a, MonadIO m) => a -> ((?self :: a) => ActionGroupConnectProxyCallback) -> m SignalHandlerId
afterActionGroupConnectProxy obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ActionGroupConnectProxyCallback wrapped
    wrapped'' <- mk_ActionGroupConnectProxyCallback wrapped'
    connectSignalFunPtr obj "connect-proxy" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ActionGroupConnectProxySignalInfo
instance SignalInfo ActionGroupConnectProxySignalInfo where
    type HaskellCallbackType ActionGroupConnectProxySignalInfo = ActionGroupConnectProxyCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ActionGroupConnectProxyCallback cb
        cb'' <- mk_ActionGroupConnectProxyCallback cb'
        connectSignalFunPtr obj "connect-proxy" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup::connect-proxy"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#g:signal:connectProxy"})

#endif

-- signal ActionGroup::disconnect-proxy
{-# DEPRECATED ActionGroupDisconnectProxyCallback ["(Since version 3.10)"] #-}
-- | The [disconnectProxy](#g:signal:disconnectProxy) signal is emitted after disconnecting a proxy
-- from an action in the group.
-- 
-- t'GI.Gtk.Objects.UIManager.UIManager' proxies the signal and provides global notification
-- just before any action is connected to a proxy, which is probably more
-- convenient to use.
-- 
-- /Since: 2.4/
type ActionGroupDisconnectProxyCallback =
    Gtk.Action.Action
    -- ^ /@action@/: the action
    -> Gtk.Widget.Widget
    -- ^ /@proxy@/: the proxy
    -> IO ()

type C_ActionGroupDisconnectProxyCallback =
    Ptr ActionGroup ->                      -- object
    Ptr Gtk.Action.Action ->
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ActionGroupDisconnectProxyCallback`.
foreign import ccall "wrapper"
    mk_ActionGroupDisconnectProxyCallback :: C_ActionGroupDisconnectProxyCallback -> IO (FunPtr C_ActionGroupDisconnectProxyCallback)

wrap_ActionGroupDisconnectProxyCallback :: 
    GObject a => (a -> ActionGroupDisconnectProxyCallback) ->
    C_ActionGroupDisconnectProxyCallback
wrap_ActionGroupDisconnectProxyCallback gi'cb gi'selfPtr action proxy _ = do
    action' <- (newObject Gtk.Action.Action) action
    proxy' <- (newObject Gtk.Widget.Widget) proxy
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  action' proxy'


-- | Connect a signal handler for the [disconnectProxy](#signal:disconnectProxy) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' actionGroup #disconnectProxy callback
-- @
-- 
-- 
onActionGroupDisconnectProxy :: (IsActionGroup a, MonadIO m) => a -> ((?self :: a) => ActionGroupDisconnectProxyCallback) -> m SignalHandlerId
onActionGroupDisconnectProxy obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ActionGroupDisconnectProxyCallback wrapped
    wrapped'' <- mk_ActionGroupDisconnectProxyCallback wrapped'
    connectSignalFunPtr obj "disconnect-proxy" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [disconnectProxy](#signal:disconnectProxy) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' actionGroup #disconnectProxy callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterActionGroupDisconnectProxy :: (IsActionGroup a, MonadIO m) => a -> ((?self :: a) => ActionGroupDisconnectProxyCallback) -> m SignalHandlerId
afterActionGroupDisconnectProxy obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ActionGroupDisconnectProxyCallback wrapped
    wrapped'' <- mk_ActionGroupDisconnectProxyCallback wrapped'
    connectSignalFunPtr obj "disconnect-proxy" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ActionGroupDisconnectProxySignalInfo
instance SignalInfo ActionGroupDisconnectProxySignalInfo where
    type HaskellCallbackType ActionGroupDisconnectProxySignalInfo = ActionGroupDisconnectProxyCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ActionGroupDisconnectProxyCallback cb
        cb'' <- mk_ActionGroupDisconnectProxyCallback cb'
        connectSignalFunPtr obj "disconnect-proxy" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup::disconnect-proxy"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#g:signal:disconnectProxy"})

#endif

-- signal ActionGroup::post-activate
{-# DEPRECATED ActionGroupPostActivateCallback ["(Since version 3.10)"] #-}
-- | The [postActivate](#g:signal:postActivate) signal is emitted just after the /@action@/ in the
-- /@actionGroup@/ is activated
-- 
-- This is intended for t'GI.Gtk.Objects.UIManager.UIManager' to proxy the signal and provide global
-- notification just after any action is activated.
-- 
-- /Since: 2.4/
type ActionGroupPostActivateCallback =
    Gtk.Action.Action
    -- ^ /@action@/: the action
    -> IO ()

type C_ActionGroupPostActivateCallback =
    Ptr ActionGroup ->                      -- object
    Ptr Gtk.Action.Action ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ActionGroupPostActivateCallback`.
foreign import ccall "wrapper"
    mk_ActionGroupPostActivateCallback :: C_ActionGroupPostActivateCallback -> IO (FunPtr C_ActionGroupPostActivateCallback)

wrap_ActionGroupPostActivateCallback :: 
    GObject a => (a -> ActionGroupPostActivateCallback) ->
    C_ActionGroupPostActivateCallback
wrap_ActionGroupPostActivateCallback gi'cb gi'selfPtr action _ = do
    action' <- (newObject Gtk.Action.Action) action
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  action'


-- | Connect a signal handler for the [postActivate](#signal:postActivate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' actionGroup #postActivate callback
-- @
-- 
-- 
onActionGroupPostActivate :: (IsActionGroup a, MonadIO m) => a -> ((?self :: a) => ActionGroupPostActivateCallback) -> m SignalHandlerId
onActionGroupPostActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ActionGroupPostActivateCallback wrapped
    wrapped'' <- mk_ActionGroupPostActivateCallback wrapped'
    connectSignalFunPtr obj "post-activate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [postActivate](#signal:postActivate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' actionGroup #postActivate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterActionGroupPostActivate :: (IsActionGroup a, MonadIO m) => a -> ((?self :: a) => ActionGroupPostActivateCallback) -> m SignalHandlerId
afterActionGroupPostActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ActionGroupPostActivateCallback wrapped
    wrapped'' <- mk_ActionGroupPostActivateCallback wrapped'
    connectSignalFunPtr obj "post-activate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ActionGroupPostActivateSignalInfo
instance SignalInfo ActionGroupPostActivateSignalInfo where
    type HaskellCallbackType ActionGroupPostActivateSignalInfo = ActionGroupPostActivateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ActionGroupPostActivateCallback cb
        cb'' <- mk_ActionGroupPostActivateCallback cb'
        connectSignalFunPtr obj "post-activate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup::post-activate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#g:signal:postActivate"})

#endif

-- signal ActionGroup::pre-activate
{-# DEPRECATED ActionGroupPreActivateCallback ["(Since version 3.10)"] #-}
-- | The [preActivate](#g:signal:preActivate) signal is emitted just before the /@action@/ in the
-- /@actionGroup@/ is activated
-- 
-- This is intended for t'GI.Gtk.Objects.UIManager.UIManager' to proxy the signal and provide global
-- notification just before any action is activated.
-- 
-- /Since: 2.4/
type ActionGroupPreActivateCallback =
    Gtk.Action.Action
    -- ^ /@action@/: the action
    -> IO ()

type C_ActionGroupPreActivateCallback =
    Ptr ActionGroup ->                      -- object
    Ptr Gtk.Action.Action ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ActionGroupPreActivateCallback`.
foreign import ccall "wrapper"
    mk_ActionGroupPreActivateCallback :: C_ActionGroupPreActivateCallback -> IO (FunPtr C_ActionGroupPreActivateCallback)

wrap_ActionGroupPreActivateCallback :: 
    GObject a => (a -> ActionGroupPreActivateCallback) ->
    C_ActionGroupPreActivateCallback
wrap_ActionGroupPreActivateCallback gi'cb gi'selfPtr action _ = do
    action' <- (newObject Gtk.Action.Action) action
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  action'


-- | Connect a signal handler for the [preActivate](#signal:preActivate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' actionGroup #preActivate callback
-- @
-- 
-- 
onActionGroupPreActivate :: (IsActionGroup a, MonadIO m) => a -> ((?self :: a) => ActionGroupPreActivateCallback) -> m SignalHandlerId
onActionGroupPreActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ActionGroupPreActivateCallback wrapped
    wrapped'' <- mk_ActionGroupPreActivateCallback wrapped'
    connectSignalFunPtr obj "pre-activate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [preActivate](#signal:preActivate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' actionGroup #preActivate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterActionGroupPreActivate :: (IsActionGroup a, MonadIO m) => a -> ((?self :: a) => ActionGroupPreActivateCallback) -> m SignalHandlerId
afterActionGroupPreActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ActionGroupPreActivateCallback wrapped
    wrapped'' <- mk_ActionGroupPreActivateCallback wrapped'
    connectSignalFunPtr obj "pre-activate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ActionGroupPreActivateSignalInfo
instance SignalInfo ActionGroupPreActivateSignalInfo where
    type HaskellCallbackType ActionGroupPreActivateSignalInfo = ActionGroupPreActivateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ActionGroupPreActivateCallback cb
        cb'' <- mk_ActionGroupPreActivateCallback cb'
        connectSignalFunPtr obj "pre-activate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup::pre-activate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#g:signal:preActivate"})

#endif

-- VVV Prop "accel-group"
   -- Type: TInterface (Name {namespace = "Gtk", name = "AccelGroup"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@accel-group@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionGroup #accelGroup
-- @
getActionGroupAccelGroup :: (MonadIO m, IsActionGroup o) => o -> m Gtk.AccelGroup.AccelGroup
getActionGroupAccelGroup obj = MIO.liftIO $ checkUnexpectedNothing "getActionGroupAccelGroup" $ B.Properties.getObjectPropertyObject obj "accel-group" Gtk.AccelGroup.AccelGroup

-- | Set the value of the “@accel-group@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionGroup [ #accelGroup 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionGroupAccelGroup :: (MonadIO m, IsActionGroup o, Gtk.AccelGroup.IsAccelGroup a) => o -> a -> m ()
setActionGroupAccelGroup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "accel-group" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@accel-group@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructActionGroupAccelGroup :: (IsActionGroup o, MIO.MonadIO m, Gtk.AccelGroup.IsAccelGroup a) => a -> m (GValueConstruct o)
constructActionGroupAccelGroup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "accel-group" (P.Just val)

-- | Set the value of the “@accel-group@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelGroup
-- @
clearActionGroupAccelGroup :: (MonadIO m, IsActionGroup o) => o -> m ()
clearActionGroupAccelGroup obj = liftIO $ B.Properties.setObjectPropertyObject obj "accel-group" (Nothing :: Maybe Gtk.AccelGroup.AccelGroup)

#if defined(ENABLE_OVERLOADING)
data ActionGroupAccelGroupPropertyInfo
instance AttrInfo ActionGroupAccelGroupPropertyInfo where
    type AttrAllowedOps ActionGroupAccelGroupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ActionGroupAccelGroupPropertyInfo = IsActionGroup
    type AttrSetTypeConstraint ActionGroupAccelGroupPropertyInfo = Gtk.AccelGroup.IsAccelGroup
    type AttrTransferTypeConstraint ActionGroupAccelGroupPropertyInfo = Gtk.AccelGroup.IsAccelGroup
    type AttrTransferType ActionGroupAccelGroupPropertyInfo = Gtk.AccelGroup.AccelGroup
    type AttrGetType ActionGroupAccelGroupPropertyInfo = Gtk.AccelGroup.AccelGroup
    type AttrLabel ActionGroupAccelGroupPropertyInfo = "accel-group"
    type AttrOrigin ActionGroupAccelGroupPropertyInfo = ActionGroup
    attrGet = getActionGroupAccelGroup
    attrSet = setActionGroupAccelGroup
    attrTransfer _ v = do
        unsafeCastTo Gtk.AccelGroup.AccelGroup v
    attrConstruct = constructActionGroupAccelGroup
    attrClear = clearActionGroupAccelGroup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.accelGroup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#g:attr:accelGroup"
        })
#endif

-- VVV Prop "name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionGroup #name
-- @
getActionGroupName :: (MonadIO m, IsActionGroup o) => o -> m T.Text
getActionGroupName obj = MIO.liftIO $ checkUnexpectedNothing "getActionGroupName" $ B.Properties.getObjectPropertyString obj "name"

-- | Construct a `GValueConstruct` with valid value for the “@name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructActionGroupName :: (IsActionGroup o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructActionGroupName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "name" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ActionGroupNamePropertyInfo
instance AttrInfo ActionGroupNamePropertyInfo where
    type AttrAllowedOps ActionGroupNamePropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ActionGroupNamePropertyInfo = IsActionGroup
    type AttrSetTypeConstraint ActionGroupNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ActionGroupNamePropertyInfo = (~) T.Text
    type AttrTransferType ActionGroupNamePropertyInfo = T.Text
    type AttrGetType ActionGroupNamePropertyInfo = T.Text
    type AttrLabel ActionGroupNamePropertyInfo = "name"
    type AttrOrigin ActionGroupNamePropertyInfo = ActionGroup
    attrGet = getActionGroupName
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructActionGroupName
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.name"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#g:attr:name"
        })
#endif

-- VVV Prop "sensitive"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@sensitive@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionGroup #sensitive
-- @
getActionGroupSensitive :: (MonadIO m, IsActionGroup o) => o -> m Bool
getActionGroupSensitive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "sensitive"

-- | Set the value of the “@sensitive@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionGroup [ #sensitive 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionGroupSensitive :: (MonadIO m, IsActionGroup o) => o -> Bool -> m ()
setActionGroupSensitive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "sensitive" val

-- | Construct a `GValueConstruct` with valid value for the “@sensitive@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructActionGroupSensitive :: (IsActionGroup o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructActionGroupSensitive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "sensitive" val

#if defined(ENABLE_OVERLOADING)
data ActionGroupSensitivePropertyInfo
instance AttrInfo ActionGroupSensitivePropertyInfo where
    type AttrAllowedOps ActionGroupSensitivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ActionGroupSensitivePropertyInfo = IsActionGroup
    type AttrSetTypeConstraint ActionGroupSensitivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ActionGroupSensitivePropertyInfo = (~) Bool
    type AttrTransferType ActionGroupSensitivePropertyInfo = Bool
    type AttrGetType ActionGroupSensitivePropertyInfo = Bool
    type AttrLabel ActionGroupSensitivePropertyInfo = "sensitive"
    type AttrOrigin ActionGroupSensitivePropertyInfo = ActionGroup
    attrGet = getActionGroupSensitive
    attrSet = setActionGroupSensitive
    attrTransfer _ v = do
        return v
    attrConstruct = constructActionGroupSensitive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.sensitive"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#g:attr:sensitive"
        })
#endif

-- VVV Prop "visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionGroup #visible
-- @
getActionGroupVisible :: (MonadIO m, IsActionGroup o) => o -> m Bool
getActionGroupVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "visible"

-- | Set the value of the “@visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionGroup [ #visible 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionGroupVisible :: (MonadIO m, IsActionGroup o) => o -> Bool -> m ()
setActionGroupVisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "visible" val

-- | Construct a `GValueConstruct` with valid value for the “@visible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructActionGroupVisible :: (IsActionGroup o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructActionGroupVisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "visible" val

#if defined(ENABLE_OVERLOADING)
data ActionGroupVisiblePropertyInfo
instance AttrInfo ActionGroupVisiblePropertyInfo where
    type AttrAllowedOps ActionGroupVisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ActionGroupVisiblePropertyInfo = IsActionGroup
    type AttrSetTypeConstraint ActionGroupVisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ActionGroupVisiblePropertyInfo = (~) Bool
    type AttrTransferType ActionGroupVisiblePropertyInfo = Bool
    type AttrGetType ActionGroupVisiblePropertyInfo = Bool
    type AttrLabel ActionGroupVisiblePropertyInfo = "visible"
    type AttrOrigin ActionGroupVisiblePropertyInfo = ActionGroup
    attrGet = getActionGroupVisible
    attrSet = setActionGroupVisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructActionGroupVisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.visible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#g:attr:visible"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ActionGroup
type instance O.AttributeList ActionGroup = ActionGroupAttributeList
type ActionGroupAttributeList = ('[ '("accelGroup", ActionGroupAccelGroupPropertyInfo), '("name", ActionGroupNamePropertyInfo), '("sensitive", ActionGroupSensitivePropertyInfo), '("visible", ActionGroupVisiblePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
actionGroupAccelGroup :: AttrLabelProxy "accelGroup"
actionGroupAccelGroup = AttrLabelProxy

actionGroupName :: AttrLabelProxy "name"
actionGroupName = AttrLabelProxy

actionGroupSensitive :: AttrLabelProxy "sensitive"
actionGroupSensitive = AttrLabelProxy

actionGroupVisible :: AttrLabelProxy "visible"
actionGroupVisible = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ActionGroup = ActionGroupSignalList
type ActionGroupSignalList = ('[ '("connectProxy", ActionGroupConnectProxySignalInfo), '("disconnectProxy", ActionGroupDisconnectProxySignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("postActivate", ActionGroupPostActivateSignalInfo), '("preActivate", ActionGroupPreActivateSignalInfo)] :: [(Symbol, *)])

#endif

-- method ActionGroup::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the action group."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ActionGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_action_group_new" gtk_action_group_new :: 
    CString ->                              -- name : TBasicType TUTF8
    IO (Ptr ActionGroup)

{-# DEPRECATED actionGroupNew ["(Since version 3.10)"] #-}
-- | Creates a new t'GI.Gtk.Objects.ActionGroup.ActionGroup' object. The name of the action group
-- is used when associating [keybindings][Action-Accel]
-- with the actions.
-- 
-- /Since: 2.4/
actionGroupNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@name@/: the name of the action group.
    -> m ActionGroup
    -- ^ __Returns:__ the new t'GI.Gtk.Objects.ActionGroup.ActionGroup'
actionGroupNew name = liftIO $ do
    name' <- textToCString name
    result <- gtk_action_group_new name'
    checkUnexpectedReturnNULL "actionGroupNew" result
    result' <- (wrapObject ActionGroup) result
    freeMem name'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ActionGroup::add_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Action" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an action" , sinceVersion = Nothing }
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

foreign import ccall "gtk_action_group_add_action" gtk_action_group_add_action :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    Ptr Gtk.Action.Action ->                -- action : TInterface (Name {namespace = "Gtk", name = "Action"})
    IO ()

{-# DEPRECATED actionGroupAddAction ["(Since version 3.10)"] #-}
-- | Adds an action object to the action group. Note that this function
-- does not set up the accel path of the action, which can lead to problems
-- if a user tries to modify the accelerator of a menuitem associated with
-- the action. Therefore you must either set the accel path yourself with
-- 'GI.Gtk.Objects.Action.actionSetAccelPath', or use
-- @gtk_action_group_add_action_with_accel (..., NULL)@.
-- 
-- /Since: 2.4/
actionGroupAddAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a, Gtk.Action.IsAction b) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> b
    -- ^ /@action@/: an action
    -> m ()
actionGroupAddAction actionGroup action = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    action' <- unsafeManagedPtrCastPtr action
    gtk_action_group_add_action actionGroup' action'
    touchManagedPtr actionGroup
    touchManagedPtr action
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionGroupAddActionMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsActionGroup a, Gtk.Action.IsAction b) => O.OverloadedMethod ActionGroupAddActionMethodInfo a signature where
    overloadedMethod = actionGroupAddAction

instance O.OverloadedMethodInfo ActionGroupAddActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupAddAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupAddAction"
        })


#endif

-- method ActionGroup::add_action_with_accel
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Action" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action to add" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the accelerator for the action, in\n  the format understood by gtk_accelerator_parse(), or \"\" for no accelerator, or\n  %NULL to use the stock accelerator"
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

foreign import ccall "gtk_action_group_add_action_with_accel" gtk_action_group_add_action_with_accel :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    Ptr Gtk.Action.Action ->                -- action : TInterface (Name {namespace = "Gtk", name = "Action"})
    CString ->                              -- accelerator : TBasicType TUTF8
    IO ()

{-# DEPRECATED actionGroupAddActionWithAccel ["(Since version 3.10)"] #-}
-- | Adds an action object to the action group and sets up the accelerator.
-- 
-- If /@accelerator@/ is 'P.Nothing', attempts to use the accelerator associated
-- with the stock_id of the action.
-- 
-- Accel paths are set to @\<Actions>\/group-name\/action-name@.
-- 
-- /Since: 2.4/
actionGroupAddActionWithAccel ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a, Gtk.Action.IsAction b) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> b
    -- ^ /@action@/: the action to add
    -> Maybe (T.Text)
    -- ^ /@accelerator@/: the accelerator for the action, in
    --   the format understood by 'GI.Gtk.Functions.acceleratorParse', or \"\" for no accelerator, or
    --   'P.Nothing' to use the stock accelerator
    -> m ()
actionGroupAddActionWithAccel actionGroup action accelerator = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    action' <- unsafeManagedPtrCastPtr action
    maybeAccelerator <- case accelerator of
        Nothing -> return nullPtr
        Just jAccelerator -> do
            jAccelerator' <- textToCString jAccelerator
            return jAccelerator'
    gtk_action_group_add_action_with_accel actionGroup' action' maybeAccelerator
    touchManagedPtr actionGroup
    touchManagedPtr action
    freeMem maybeAccelerator
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionGroupAddActionWithAccelMethodInfo
instance (signature ~ (b -> Maybe (T.Text) -> m ()), MonadIO m, IsActionGroup a, Gtk.Action.IsAction b) => O.OverloadedMethod ActionGroupAddActionWithAccelMethodInfo a signature where
    overloadedMethod = actionGroupAddActionWithAccel

instance O.OverloadedMethodInfo ActionGroupAddActionWithAccelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupAddActionWithAccel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupAddActionWithAccel"
        })


#endif

-- method ActionGroup::get_accel_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "AccelGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_action_group_get_accel_group" gtk_action_group_get_accel_group :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    IO (Ptr Gtk.AccelGroup.AccelGroup)

{-# DEPRECATED actionGroupGetAccelGroup ["(Since version 3.10)"] #-}
-- | Gets the accelerator group.
-- 
-- /Since: 3.6/
actionGroupGetAccelGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: a t'GI.Gtk.Objects.ActionGroup.ActionGroup'
    -> m Gtk.AccelGroup.AccelGroup
    -- ^ __Returns:__ the accelerator group associated with this action
    -- group or 'P.Nothing' if there is none.
actionGroupGetAccelGroup actionGroup = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    result <- gtk_action_group_get_accel_group actionGroup'
    checkUnexpectedReturnNULL "actionGroupGetAccelGroup" result
    result' <- (newObject Gtk.AccelGroup.AccelGroup) result
    touchManagedPtr actionGroup
    return result'

#if defined(ENABLE_OVERLOADING)
data ActionGroupGetAccelGroupMethodInfo
instance (signature ~ (m Gtk.AccelGroup.AccelGroup), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupGetAccelGroupMethodInfo a signature where
    overloadedMethod = actionGroupGetAccelGroup

instance O.OverloadedMethodInfo ActionGroupGetAccelGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupGetAccelGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupGetAccelGroup"
        })


#endif

-- method ActionGroup::get_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the name of the action"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Action" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_action_group_get_action" gtk_action_group_get_action :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    CString ->                              -- action_name : TBasicType TUTF8
    IO (Ptr Gtk.Action.Action)

{-# DEPRECATED actionGroupGetAction ["(Since version 3.10)"] #-}
-- | Looks up an action in the action group by name.
-- 
-- /Since: 2.4/
actionGroupGetAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> T.Text
    -- ^ /@actionName@/: the name of the action
    -> m (Maybe Gtk.Action.Action)
    -- ^ __Returns:__ the action, or 'P.Nothing' if no action by that name exists
actionGroupGetAction actionGroup actionName = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    actionName' <- textToCString actionName
    result <- gtk_action_group_get_action actionGroup' actionName'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Action.Action) result'
        return result''
    touchManagedPtr actionGroup
    freeMem actionName'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ActionGroupGetActionMethodInfo
instance (signature ~ (T.Text -> m (Maybe Gtk.Action.Action)), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupGetActionMethodInfo a signature where
    overloadedMethod = actionGroupGetAction

instance O.OverloadedMethodInfo ActionGroupGetActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupGetAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupGetAction"
        })


#endif

-- method ActionGroup::get_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
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

foreign import ccall "gtk_action_group_get_name" gtk_action_group_get_name :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    IO CString

{-# DEPRECATED actionGroupGetName ["(Since version 3.10)"] #-}
-- | Gets the name of the action group.
-- 
-- /Since: 2.4/
actionGroupGetName ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> m T.Text
    -- ^ __Returns:__ the name of the action group.
actionGroupGetName actionGroup = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    result <- gtk_action_group_get_name actionGroup'
    checkUnexpectedReturnNULL "actionGroupGetName" result
    result' <- cstringToText result
    touchManagedPtr actionGroup
    return result'

#if defined(ENABLE_OVERLOADING)
data ActionGroupGetNameMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupGetNameMethodInfo a signature where
    overloadedMethod = actionGroupGetName

instance O.OverloadedMethodInfo ActionGroupGetNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupGetName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupGetName"
        })


#endif

-- method ActionGroup::get_sensitive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
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

foreign import ccall "gtk_action_group_get_sensitive" gtk_action_group_get_sensitive :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    IO CInt

{-# DEPRECATED actionGroupGetSensitive ["(Since version 3.10)"] #-}
-- | Returns 'P.True' if the group is sensitive.  The constituent actions
-- can only be logically sensitive (see 'GI.Gtk.Objects.Action.actionIsSensitive') if
-- they are sensitive (see 'GI.Gtk.Objects.Action.actionGetSensitive') and their group
-- is sensitive.
-- 
-- /Since: 2.4/
actionGroupGetSensitive ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the group is sensitive.
actionGroupGetSensitive actionGroup = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    result <- gtk_action_group_get_sensitive actionGroup'
    let result' = (/= 0) result
    touchManagedPtr actionGroup
    return result'

#if defined(ENABLE_OVERLOADING)
data ActionGroupGetSensitiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupGetSensitiveMethodInfo a signature where
    overloadedMethod = actionGroupGetSensitive

instance O.OverloadedMethodInfo ActionGroupGetSensitiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupGetSensitive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupGetSensitive"
        })


#endif

-- method ActionGroup::get_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
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

foreign import ccall "gtk_action_group_get_visible" gtk_action_group_get_visible :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    IO CInt

{-# DEPRECATED actionGroupGetVisible ["(Since version 3.10)"] #-}
-- | Returns 'P.True' if the group is visible.  The constituent actions
-- can only be logically visible (see 'GI.Gtk.Objects.Action.actionIsVisible') if
-- they are visible (see 'GI.Gtk.Objects.Action.actionGetVisible') and their group
-- is visible.
-- 
-- /Since: 2.4/
actionGroupGetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the group is visible.
actionGroupGetVisible actionGroup = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    result <- gtk_action_group_get_visible actionGroup'
    let result' = (/= 0) result
    touchManagedPtr actionGroup
    return result'

#if defined(ENABLE_OVERLOADING)
data ActionGroupGetVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupGetVisibleMethodInfo a signature where
    overloadedMethod = actionGroupGetVisible

instance O.OverloadedMethodInfo ActionGroupGetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupGetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupGetVisible"
        })


#endif

-- method ActionGroup::list_actions
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Action" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_action_group_list_actions" gtk_action_group_list_actions :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    IO (Ptr (GList (Ptr Gtk.Action.Action)))

{-# DEPRECATED actionGroupListActions ["(Since version 3.10)"] #-}
-- | Lists the actions in the action group.
-- 
-- /Since: 2.4/
actionGroupListActions ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> m [Gtk.Action.Action]
    -- ^ __Returns:__ an allocated list of the action objects in the action group
actionGroupListActions actionGroup = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    result <- gtk_action_group_list_actions actionGroup'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.Action.Action) result'
    g_list_free result
    touchManagedPtr actionGroup
    return result''

#if defined(ENABLE_OVERLOADING)
data ActionGroupListActionsMethodInfo
instance (signature ~ (m [Gtk.Action.Action]), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupListActionsMethodInfo a signature where
    overloadedMethod = actionGroupListActions

instance O.OverloadedMethodInfo ActionGroupListActionsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupListActions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupListActions"
        })


#endif

-- method ActionGroup::remove_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Action" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an action" , sinceVersion = Nothing }
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

foreign import ccall "gtk_action_group_remove_action" gtk_action_group_remove_action :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    Ptr Gtk.Action.Action ->                -- action : TInterface (Name {namespace = "Gtk", name = "Action"})
    IO ()

{-# DEPRECATED actionGroupRemoveAction ["(Since version 3.10)"] #-}
-- | Removes an action object from the action group.
-- 
-- /Since: 2.4/
actionGroupRemoveAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a, Gtk.Action.IsAction b) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> b
    -- ^ /@action@/: an action
    -> m ()
actionGroupRemoveAction actionGroup action = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    action' <- unsafeManagedPtrCastPtr action
    gtk_action_group_remove_action actionGroup' action'
    touchManagedPtr actionGroup
    touchManagedPtr action
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionGroupRemoveActionMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsActionGroup a, Gtk.Action.IsAction b) => O.OverloadedMethod ActionGroupRemoveActionMethodInfo a signature where
    overloadedMethod = actionGroupRemoveAction

instance O.OverloadedMethodInfo ActionGroupRemoveActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupRemoveAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupRemoveAction"
        })


#endif

-- method ActionGroup::set_accel_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelGroup" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelGroup to set or %NULL"
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

foreign import ccall "gtk_action_group_set_accel_group" gtk_action_group_set_accel_group :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    Ptr Gtk.AccelGroup.AccelGroup ->        -- accel_group : TInterface (Name {namespace = "Gtk", name = "AccelGroup"})
    IO ()

{-# DEPRECATED actionGroupSetAccelGroup ["(Since version 3.10)"] #-}
-- | Sets the accelerator group to be used by every action in this group.
-- 
-- /Since: 3.6/
actionGroupSetAccelGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a, Gtk.AccelGroup.IsAccelGroup b) =>
    a
    -- ^ /@actionGroup@/: a t'GI.Gtk.Objects.ActionGroup.ActionGroup'
    -> Maybe (b)
    -- ^ /@accelGroup@/: a t'GI.Gtk.Objects.AccelGroup.AccelGroup' to set or 'P.Nothing'
    -> m ()
actionGroupSetAccelGroup actionGroup accelGroup = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    maybeAccelGroup <- case accelGroup of
        Nothing -> return nullPtr
        Just jAccelGroup -> do
            jAccelGroup' <- unsafeManagedPtrCastPtr jAccelGroup
            return jAccelGroup'
    gtk_action_group_set_accel_group actionGroup' maybeAccelGroup
    touchManagedPtr actionGroup
    whenJust accelGroup touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionGroupSetAccelGroupMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsActionGroup a, Gtk.AccelGroup.IsAccelGroup b) => O.OverloadedMethod ActionGroupSetAccelGroupMethodInfo a signature where
    overloadedMethod = actionGroupSetAccelGroup

instance O.OverloadedMethodInfo ActionGroupSetAccelGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupSetAccelGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupSetAccelGroup"
        })


#endif

-- method ActionGroup::set_sensitive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sensitive"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new sensitivity" , sinceVersion = Nothing }
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

foreign import ccall "gtk_action_group_set_sensitive" gtk_action_group_set_sensitive :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    CInt ->                                 -- sensitive : TBasicType TBoolean
    IO ()

{-# DEPRECATED actionGroupSetSensitive ["(Since version 3.10)"] #-}
-- | Changes the sensitivity of /@actionGroup@/
-- 
-- /Since: 2.4/
actionGroupSetSensitive ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> Bool
    -- ^ /@sensitive@/: new sensitivity
    -> m ()
actionGroupSetSensitive actionGroup sensitive = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    let sensitive' = (fromIntegral . fromEnum) sensitive
    gtk_action_group_set_sensitive actionGroup' sensitive'
    touchManagedPtr actionGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionGroupSetSensitiveMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupSetSensitiveMethodInfo a signature where
    overloadedMethod = actionGroupSetSensitive

instance O.OverloadedMethodInfo ActionGroupSetSensitiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupSetSensitive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupSetSensitive"
        })


#endif

-- method ActionGroup::set_translate_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TranslateFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTranslateFunc"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
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
--                 { rawDocText = Just "data to be passed to @func and @notify"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "notify"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GDestroyNotify function to be called when @action_group is\n  destroyed and when the translation function is changed again"
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

foreign import ccall "gtk_action_group_set_translate_func" gtk_action_group_set_translate_func :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    FunPtr Gtk.Callbacks.C_TranslateFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TranslateFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- notify : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

{-# DEPRECATED actionGroupSetTranslateFunc ["(Since version 3.10)"] #-}
-- | Sets a function to be used for translating the /@label@/ and /@tooltip@/ of
-- @/GtkActionEntrys/@ added by @/gtk_action_group_add_actions()/@.
-- 
-- If you’re using @/gettext()/@, it is enough to set the translation domain
-- with 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslationDomain'.
-- 
-- /Since: 2.4/
actionGroupSetTranslateFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: a t'GI.Gtk.Objects.ActionGroup.ActionGroup'
    -> Gtk.Callbacks.TranslateFunc
    -- ^ /@func@/: a t'GI.Gtk.Callbacks.TranslateFunc'
    -> m ()
actionGroupSetTranslateFunc actionGroup func = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    func' <- Gtk.Callbacks.mk_TranslateFunc (Gtk.Callbacks.wrap_TranslateFunc Nothing (Gtk.Callbacks.drop_closures_TranslateFunc func))
    let data_ = castFunPtrToPtr func'
    let notify = SP.safeFreeFunPtrPtr
    gtk_action_group_set_translate_func actionGroup' func' data_ notify
    touchManagedPtr actionGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionGroupSetTranslateFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.TranslateFunc -> m ()), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupSetTranslateFuncMethodInfo a signature where
    overloadedMethod = actionGroupSetTranslateFunc

instance O.OverloadedMethodInfo ActionGroupSetTranslateFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupSetTranslateFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupSetTranslateFunc"
        })


#endif

-- method ActionGroup::set_translation_domain
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "domain"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the translation domain to use for g_dgettext()\ncalls, or %NULL to use the domain set with textdomain()"
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

foreign import ccall "gtk_action_group_set_translation_domain" gtk_action_group_set_translation_domain :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    CString ->                              -- domain : TBasicType TUTF8
    IO ()

{-# DEPRECATED actionGroupSetTranslationDomain ["(Since version 3.10)"] #-}
-- | Sets the translation domain and uses 'GI.GLib.Functions.dgettext' for translating the
-- /@label@/ and /@tooltip@/ of @/GtkActionEntrys/@ added by
-- @/gtk_action_group_add_actions()/@.
-- 
-- If you’re not using @/gettext()/@ for localization, see
-- 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslateFunc'.
-- 
-- /Since: 2.4/
actionGroupSetTranslationDomain ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: a t'GI.Gtk.Objects.ActionGroup.ActionGroup'
    -> Maybe (T.Text)
    -- ^ /@domain@/: the translation domain to use for 'GI.GLib.Functions.dgettext'
    -- calls, or 'P.Nothing' to use the domain set with @/textdomain()/@
    -> m ()
actionGroupSetTranslationDomain actionGroup domain = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    maybeDomain <- case domain of
        Nothing -> return nullPtr
        Just jDomain -> do
            jDomain' <- textToCString jDomain
            return jDomain'
    gtk_action_group_set_translation_domain actionGroup' maybeDomain
    touchManagedPtr actionGroup
    freeMem maybeDomain
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionGroupSetTranslationDomainMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupSetTranslationDomainMethodInfo a signature where
    overloadedMethod = actionGroupSetTranslationDomain

instance O.OverloadedMethodInfo ActionGroupSetTranslationDomainMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupSetTranslationDomain",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupSetTranslationDomain"
        })


#endif

-- method ActionGroup::set_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action group" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "visible"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new visiblity" , sinceVersion = Nothing }
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

foreign import ccall "gtk_action_group_set_visible" gtk_action_group_set_visible :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    CInt ->                                 -- visible : TBasicType TBoolean
    IO ()

{-# DEPRECATED actionGroupSetVisible ["(Since version 3.10)"] #-}
-- | Changes the visible of /@actionGroup@/.
-- 
-- /Since: 2.4/
actionGroupSetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: the action group
    -> Bool
    -- ^ /@visible@/: new visiblity
    -> m ()
actionGroupSetVisible actionGroup visible = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    let visible' = (fromIntegral . fromEnum) visible
    gtk_action_group_set_visible actionGroup' visible'
    touchManagedPtr actionGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionGroupSetVisibleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupSetVisibleMethodInfo a signature where
    overloadedMethod = actionGroupSetVisible

instance O.OverloadedMethodInfo ActionGroupSetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupSetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupSetVisible"
        })


#endif

-- method ActionGroup::translate_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "string"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string" , sinceVersion = Nothing }
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

foreign import ccall "gtk_action_group_translate_string" gtk_action_group_translate_string :: 
    Ptr ActionGroup ->                      -- action_group : TInterface (Name {namespace = "Gtk", name = "ActionGroup"})
    CString ->                              -- string : TBasicType TUTF8
    IO CString

{-# DEPRECATED actionGroupTranslateString ["(Since version 3.10)"] #-}
-- | Translates a string using the function set with
-- 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslateFunc'. This
-- is mainly intended for language bindings.
-- 
-- /Since: 2.6/
actionGroupTranslateString ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionGroup a) =>
    a
    -- ^ /@actionGroup@/: a t'GI.Gtk.Objects.ActionGroup.ActionGroup'
    -> T.Text
    -- ^ /@string@/: a string
    -> m T.Text
    -- ^ __Returns:__ the translation of /@string@/
actionGroupTranslateString actionGroup string = liftIO $ do
    actionGroup' <- unsafeManagedPtrCastPtr actionGroup
    string' <- textToCString string
    result <- gtk_action_group_translate_string actionGroup' string'
    checkUnexpectedReturnNULL "actionGroupTranslateString" result
    result' <- cstringToText result
    touchManagedPtr actionGroup
    freeMem string'
    return result'

#if defined(ENABLE_OVERLOADING)
data ActionGroupTranslateStringMethodInfo
instance (signature ~ (T.Text -> m T.Text), MonadIO m, IsActionGroup a) => O.OverloadedMethod ActionGroupTranslateStringMethodInfo a signature where
    overloadedMethod = actionGroupTranslateString

instance O.OverloadedMethodInfo ActionGroupTranslateStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ActionGroup.actionGroupTranslateString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ActionGroup.html#v:actionGroupTranslateString"
        })


#endif


