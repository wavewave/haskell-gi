{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.RadioAction.RadioAction' is similar to t'GI.Gtk.Objects.RadioMenuItem.RadioMenuItem'. A number of radio
-- actions can be linked together so that only one may be active at any
-- one time.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.RadioAction
    ( 

-- * Exported types
    RadioAction(..)                         ,
    IsRadioAction                           ,
    toRadioAction                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Action#g:method:activate"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [blockActivate]("GI.Gtk.Objects.Action#g:method:blockActivate"), [connectAccelerator]("GI.Gtk.Objects.Action#g:method:connectAccelerator"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createIcon]("GI.Gtk.Objects.Action#g:method:createIcon"), [createMenu]("GI.Gtk.Objects.Action#g:method:createMenu"), [createMenuItem]("GI.Gtk.Objects.Action#g:method:createMenuItem"), [createToolItem]("GI.Gtk.Objects.Action#g:method:createToolItem"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [disconnectAccelerator]("GI.Gtk.Objects.Action#g:method:disconnectAccelerator"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isSensitive]("GI.Gtk.Objects.Action#g:method:isSensitive"), [isVisible]("GI.Gtk.Objects.Action#g:method:isVisible"), [joinGroup]("GI.Gtk.Objects.RadioAction#g:method:joinGroup"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toggled]("GI.Gtk.Objects.ToggleAction#g:method:toggled"), [unblockActivate]("GI.Gtk.Objects.Action#g:method:unblockActivate"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccelClosure]("GI.Gtk.Objects.Action#g:method:getAccelClosure"), [getAccelPath]("GI.Gtk.Objects.Action#g:method:getAccelPath"), [getActive]("GI.Gtk.Objects.ToggleAction#g:method:getActive"), [getAlwaysShowImage]("GI.Gtk.Objects.Action#g:method:getAlwaysShowImage"), [getCurrentValue]("GI.Gtk.Objects.RadioAction#g:method:getCurrentValue"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDrawAsRadio]("GI.Gtk.Objects.ToggleAction#g:method:getDrawAsRadio"), [getGicon]("GI.Gtk.Objects.Action#g:method:getGicon"), [getGroup]("GI.Gtk.Objects.RadioAction#g:method:getGroup"), [getIconName]("GI.Gtk.Objects.Action#g:method:getIconName"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getIsImportant]("GI.Gtk.Objects.Action#g:method:getIsImportant"), [getLabel]("GI.Gtk.Objects.Action#g:method:getLabel"), [getName]("GI.Gtk.Objects.Action#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getProxies]("GI.Gtk.Objects.Action#g:method:getProxies"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSensitive]("GI.Gtk.Objects.Action#g:method:getSensitive"), [getShortLabel]("GI.Gtk.Objects.Action#g:method:getShortLabel"), [getStockId]("GI.Gtk.Objects.Action#g:method:getStockId"), [getTooltip]("GI.Gtk.Objects.Action#g:method:getTooltip"), [getVisible]("GI.Gtk.Objects.Action#g:method:getVisible"), [getVisibleHorizontal]("GI.Gtk.Objects.Action#g:method:getVisibleHorizontal"), [getVisibleVertical]("GI.Gtk.Objects.Action#g:method:getVisibleVertical").
-- 
-- ==== Setters
-- [setAccelGroup]("GI.Gtk.Objects.Action#g:method:setAccelGroup"), [setAccelPath]("GI.Gtk.Objects.Action#g:method:setAccelPath"), [setActive]("GI.Gtk.Objects.ToggleAction#g:method:setActive"), [setAlwaysShowImage]("GI.Gtk.Objects.Action#g:method:setAlwaysShowImage"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCurrentValue]("GI.Gtk.Objects.RadioAction#g:method:setCurrentValue"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDrawAsRadio]("GI.Gtk.Objects.ToggleAction#g:method:setDrawAsRadio"), [setGicon]("GI.Gtk.Objects.Action#g:method:setGicon"), [setGroup]("GI.Gtk.Objects.RadioAction#g:method:setGroup"), [setIconName]("GI.Gtk.Objects.Action#g:method:setIconName"), [setIsImportant]("GI.Gtk.Objects.Action#g:method:setIsImportant"), [setLabel]("GI.Gtk.Objects.Action#g:method:setLabel"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSensitive]("GI.Gtk.Objects.Action#g:method:setSensitive"), [setShortLabel]("GI.Gtk.Objects.Action#g:method:setShortLabel"), [setStockId]("GI.Gtk.Objects.Action#g:method:setStockId"), [setTooltip]("GI.Gtk.Objects.Action#g:method:setTooltip"), [setVisible]("GI.Gtk.Objects.Action#g:method:setVisible"), [setVisibleHorizontal]("GI.Gtk.Objects.Action#g:method:setVisibleHorizontal"), [setVisibleVertical]("GI.Gtk.Objects.Action#g:method:setVisibleVertical").

#if defined(ENABLE_OVERLOADING)
    ResolveRadioActionMethod                ,
#endif

-- ** getCurrentValue #method:getCurrentValue#

#if defined(ENABLE_OVERLOADING)
    RadioActionGetCurrentValueMethodInfo    ,
#endif
    radioActionGetCurrentValue              ,


-- ** getGroup #method:getGroup#

#if defined(ENABLE_OVERLOADING)
    RadioActionGetGroupMethodInfo           ,
#endif
    radioActionGetGroup                     ,


-- ** joinGroup #method:joinGroup#

#if defined(ENABLE_OVERLOADING)
    RadioActionJoinGroupMethodInfo          ,
#endif
    radioActionJoinGroup                    ,


-- ** new #method:new#

    radioActionNew                          ,


-- ** setCurrentValue #method:setCurrentValue#

#if defined(ENABLE_OVERLOADING)
    RadioActionSetCurrentValueMethodInfo    ,
#endif
    radioActionSetCurrentValue              ,


-- ** setGroup #method:setGroup#

#if defined(ENABLE_OVERLOADING)
    RadioActionSetGroupMethodInfo           ,
#endif
    radioActionSetGroup                     ,




 -- * Properties


-- ** currentValue #attr:currentValue#
-- | The value property of the currently active member of the group to which
-- this action belongs.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RadioActionCurrentValuePropertyInfo     ,
#endif
    constructRadioActionCurrentValue        ,
    getRadioActionCurrentValue              ,
#if defined(ENABLE_OVERLOADING)
    radioActionCurrentValue                 ,
#endif
    setRadioActionCurrentValue              ,


-- ** group #attr:group#
-- | Sets a new group for a radio action.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    RadioActionGroupPropertyInfo            ,
#endif
    clearRadioActionGroup                   ,
    constructRadioActionGroup               ,
#if defined(ENABLE_OVERLOADING)
    radioActionGroup                        ,
#endif
    setRadioActionGroup                     ,


-- ** value #attr:value#
-- | The value is an arbitrary integer which can be used as a
-- convenient way to determine which action in the group is
-- currently active in an [activate](#g:signal:activate) or [changed](#g:signal:changed) signal handler.
-- See 'GI.Gtk.Objects.RadioAction.radioActionGetCurrentValue' and t'GI.Gtk.Structs.RadioActionEntry.RadioActionEntry'
-- for convenient ways to get and set this property.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    RadioActionValuePropertyInfo            ,
#endif
    constructRadioActionValue               ,
    getRadioActionValue                     ,
#if defined(ENABLE_OVERLOADING)
    radioActionValue                        ,
#endif
    setRadioActionValue                     ,




 -- * Signals


-- ** changed #signal:changed#

    RadioActionChangedCallback              ,
#if defined(ENABLE_OVERLOADING)
    RadioActionChangedSignalInfo            ,
#endif
    afterRadioActionChanged                 ,
    onRadioActionChanged                    ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Action as Gtk.Action
import {-# SOURCE #-} qualified GI.Gtk.Objects.ToggleAction as Gtk.ToggleAction

-- | Memory-managed wrapper type.
newtype RadioAction = RadioAction (SP.ManagedPtr RadioAction)
    deriving (Eq)

instance SP.ManagedPtrNewtype RadioAction where
    toManagedPtr (RadioAction p) = p

foreign import ccall "gtk_radio_action_get_type"
    c_gtk_radio_action_get_type :: IO B.Types.GType

instance B.Types.TypedObject RadioAction where
    glibType = c_gtk_radio_action_get_type

instance B.Types.GObject RadioAction

-- | Type class for types which can be safely cast to `RadioAction`, for instance with `toRadioAction`.
class (SP.GObject o, O.IsDescendantOf RadioAction o) => IsRadioAction o
instance (SP.GObject o, O.IsDescendantOf RadioAction o) => IsRadioAction o

instance O.HasParentTypes RadioAction
type instance O.ParentTypes RadioAction = '[Gtk.ToggleAction.ToggleAction, Gtk.Action.Action, GObject.Object.Object, Gtk.Buildable.Buildable]

-- | Cast to `RadioAction`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toRadioAction :: (MIO.MonadIO m, IsRadioAction o) => o -> m RadioAction
toRadioAction = MIO.liftIO . B.ManagedPtr.unsafeCastTo RadioAction

-- | Convert 'RadioAction' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe RadioAction) where
    gvalueGType_ = c_gtk_radio_action_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr RadioAction)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr RadioAction)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject RadioAction ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveRadioActionMethod (t :: Symbol) (o :: *) :: * where
    ResolveRadioActionMethod "activate" o = Gtk.Action.ActionActivateMethodInfo
    ResolveRadioActionMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveRadioActionMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveRadioActionMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveRadioActionMethod "blockActivate" o = Gtk.Action.ActionBlockActivateMethodInfo
    ResolveRadioActionMethod "connectAccelerator" o = Gtk.Action.ActionConnectAcceleratorMethodInfo
    ResolveRadioActionMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveRadioActionMethod "createIcon" o = Gtk.Action.ActionCreateIconMethodInfo
    ResolveRadioActionMethod "createMenu" o = Gtk.Action.ActionCreateMenuMethodInfo
    ResolveRadioActionMethod "createMenuItem" o = Gtk.Action.ActionCreateMenuItemMethodInfo
    ResolveRadioActionMethod "createToolItem" o = Gtk.Action.ActionCreateToolItemMethodInfo
    ResolveRadioActionMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveRadioActionMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveRadioActionMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveRadioActionMethod "disconnectAccelerator" o = Gtk.Action.ActionDisconnectAcceleratorMethodInfo
    ResolveRadioActionMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveRadioActionMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveRadioActionMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveRadioActionMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveRadioActionMethod "isSensitive" o = Gtk.Action.ActionIsSensitiveMethodInfo
    ResolveRadioActionMethod "isVisible" o = Gtk.Action.ActionIsVisibleMethodInfo
    ResolveRadioActionMethod "joinGroup" o = RadioActionJoinGroupMethodInfo
    ResolveRadioActionMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveRadioActionMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveRadioActionMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveRadioActionMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveRadioActionMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveRadioActionMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveRadioActionMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveRadioActionMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveRadioActionMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveRadioActionMethod "toggled" o = Gtk.ToggleAction.ToggleActionToggledMethodInfo
    ResolveRadioActionMethod "unblockActivate" o = Gtk.Action.ActionUnblockActivateMethodInfo
    ResolveRadioActionMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveRadioActionMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveRadioActionMethod "getAccelClosure" o = Gtk.Action.ActionGetAccelClosureMethodInfo
    ResolveRadioActionMethod "getAccelPath" o = Gtk.Action.ActionGetAccelPathMethodInfo
    ResolveRadioActionMethod "getActive" o = Gtk.ToggleAction.ToggleActionGetActiveMethodInfo
    ResolveRadioActionMethod "getAlwaysShowImage" o = Gtk.Action.ActionGetAlwaysShowImageMethodInfo
    ResolveRadioActionMethod "getCurrentValue" o = RadioActionGetCurrentValueMethodInfo
    ResolveRadioActionMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveRadioActionMethod "getDrawAsRadio" o = Gtk.ToggleAction.ToggleActionGetDrawAsRadioMethodInfo
    ResolveRadioActionMethod "getGicon" o = Gtk.Action.ActionGetGiconMethodInfo
    ResolveRadioActionMethod "getGroup" o = RadioActionGetGroupMethodInfo
    ResolveRadioActionMethod "getIconName" o = Gtk.Action.ActionGetIconNameMethodInfo
    ResolveRadioActionMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveRadioActionMethod "getIsImportant" o = Gtk.Action.ActionGetIsImportantMethodInfo
    ResolveRadioActionMethod "getLabel" o = Gtk.Action.ActionGetLabelMethodInfo
    ResolveRadioActionMethod "getName" o = Gtk.Action.ActionGetNameMethodInfo
    ResolveRadioActionMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveRadioActionMethod "getProxies" o = Gtk.Action.ActionGetProxiesMethodInfo
    ResolveRadioActionMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveRadioActionMethod "getSensitive" o = Gtk.Action.ActionGetSensitiveMethodInfo
    ResolveRadioActionMethod "getShortLabel" o = Gtk.Action.ActionGetShortLabelMethodInfo
    ResolveRadioActionMethod "getStockId" o = Gtk.Action.ActionGetStockIdMethodInfo
    ResolveRadioActionMethod "getTooltip" o = Gtk.Action.ActionGetTooltipMethodInfo
    ResolveRadioActionMethod "getVisible" o = Gtk.Action.ActionGetVisibleMethodInfo
    ResolveRadioActionMethod "getVisibleHorizontal" o = Gtk.Action.ActionGetVisibleHorizontalMethodInfo
    ResolveRadioActionMethod "getVisibleVertical" o = Gtk.Action.ActionGetVisibleVerticalMethodInfo
    ResolveRadioActionMethod "setAccelGroup" o = Gtk.Action.ActionSetAccelGroupMethodInfo
    ResolveRadioActionMethod "setAccelPath" o = Gtk.Action.ActionSetAccelPathMethodInfo
    ResolveRadioActionMethod "setActive" o = Gtk.ToggleAction.ToggleActionSetActiveMethodInfo
    ResolveRadioActionMethod "setAlwaysShowImage" o = Gtk.Action.ActionSetAlwaysShowImageMethodInfo
    ResolveRadioActionMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveRadioActionMethod "setCurrentValue" o = RadioActionSetCurrentValueMethodInfo
    ResolveRadioActionMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveRadioActionMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveRadioActionMethod "setDrawAsRadio" o = Gtk.ToggleAction.ToggleActionSetDrawAsRadioMethodInfo
    ResolveRadioActionMethod "setGicon" o = Gtk.Action.ActionSetGiconMethodInfo
    ResolveRadioActionMethod "setGroup" o = RadioActionSetGroupMethodInfo
    ResolveRadioActionMethod "setIconName" o = Gtk.Action.ActionSetIconNameMethodInfo
    ResolveRadioActionMethod "setIsImportant" o = Gtk.Action.ActionSetIsImportantMethodInfo
    ResolveRadioActionMethod "setLabel" o = Gtk.Action.ActionSetLabelMethodInfo
    ResolveRadioActionMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveRadioActionMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveRadioActionMethod "setSensitive" o = Gtk.Action.ActionSetSensitiveMethodInfo
    ResolveRadioActionMethod "setShortLabel" o = Gtk.Action.ActionSetShortLabelMethodInfo
    ResolveRadioActionMethod "setStockId" o = Gtk.Action.ActionSetStockIdMethodInfo
    ResolveRadioActionMethod "setTooltip" o = Gtk.Action.ActionSetTooltipMethodInfo
    ResolveRadioActionMethod "setVisible" o = Gtk.Action.ActionSetVisibleMethodInfo
    ResolveRadioActionMethod "setVisibleHorizontal" o = Gtk.Action.ActionSetVisibleHorizontalMethodInfo
    ResolveRadioActionMethod "setVisibleVertical" o = Gtk.Action.ActionSetVisibleVerticalMethodInfo
    ResolveRadioActionMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRadioActionMethod t RadioAction, O.OverloadedMethod info RadioAction p) => OL.IsLabel t (RadioAction -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRadioActionMethod t RadioAction, O.OverloadedMethod info RadioAction p, R.HasField t RadioAction p) => R.HasField t RadioAction p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRadioActionMethod t RadioAction, O.OverloadedMethodInfo info RadioAction) => OL.IsLabel t (O.MethodProxy info RadioAction) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal RadioAction::changed
{-# DEPRECATED RadioActionChangedCallback ["(Since version 3.10)"] #-}
-- | The [changed](#g:signal:changed) signal is emitted on every member of a radio group when the
-- active member is changed. The signal gets emitted after the [activate](#g:signal:activate) signals
-- for the previous and current active members.
-- 
-- /Since: 2.4/
type RadioActionChangedCallback =
    RadioAction
    -- ^ /@current@/: the member of /@action@/\'s group which has just been activated
    -> IO ()

type C_RadioActionChangedCallback =
    Ptr RadioAction ->                      -- object
    Ptr RadioAction ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_RadioActionChangedCallback`.
foreign import ccall "wrapper"
    mk_RadioActionChangedCallback :: C_RadioActionChangedCallback -> IO (FunPtr C_RadioActionChangedCallback)

wrap_RadioActionChangedCallback :: 
    GObject a => (a -> RadioActionChangedCallback) ->
    C_RadioActionChangedCallback
wrap_RadioActionChangedCallback gi'cb gi'selfPtr current _ = do
    current' <- (newObject RadioAction) current
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  current'


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' radioAction #changed callback
-- @
-- 
-- 
onRadioActionChanged :: (IsRadioAction a, MonadIO m) => a -> ((?self :: a) => RadioActionChangedCallback) -> m SignalHandlerId
onRadioActionChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_RadioActionChangedCallback wrapped
    wrapped'' <- mk_RadioActionChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' radioAction #changed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterRadioActionChanged :: (IsRadioAction a, MonadIO m) => a -> ((?self :: a) => RadioActionChangedCallback) -> m SignalHandlerId
afterRadioActionChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_RadioActionChangedCallback wrapped
    wrapped'' <- mk_RadioActionChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data RadioActionChangedSignalInfo
instance SignalInfo RadioActionChangedSignalInfo where
    type HaskellCallbackType RadioActionChangedSignalInfo = RadioActionChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_RadioActionChangedCallback cb
        cb'' <- mk_RadioActionChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#g:signal:changed"})

#endif

-- VVV Prop "current-value"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@current-value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' radioAction #currentValue
-- @
getRadioActionCurrentValue :: (MonadIO m, IsRadioAction o) => o -> m Int32
getRadioActionCurrentValue obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "current-value"

-- | Set the value of the “@current-value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioAction [ #currentValue 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionCurrentValue :: (MonadIO m, IsRadioAction o) => o -> Int32 -> m ()
setRadioActionCurrentValue obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "current-value" val

-- | Construct a `GValueConstruct` with valid value for the “@current-value@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRadioActionCurrentValue :: (IsRadioAction o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructRadioActionCurrentValue val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "current-value" val

#if defined(ENABLE_OVERLOADING)
data RadioActionCurrentValuePropertyInfo
instance AttrInfo RadioActionCurrentValuePropertyInfo where
    type AttrAllowedOps RadioActionCurrentValuePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RadioActionCurrentValuePropertyInfo = IsRadioAction
    type AttrSetTypeConstraint RadioActionCurrentValuePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint RadioActionCurrentValuePropertyInfo = (~) Int32
    type AttrTransferType RadioActionCurrentValuePropertyInfo = Int32
    type AttrGetType RadioActionCurrentValuePropertyInfo = Int32
    type AttrLabel RadioActionCurrentValuePropertyInfo = "current-value"
    type AttrOrigin RadioActionCurrentValuePropertyInfo = RadioAction
    attrGet = getRadioActionCurrentValue
    attrSet = setRadioActionCurrentValue
    attrTransfer _ v = do
        return v
    attrConstruct = constructRadioActionCurrentValue
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction.currentValue"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#g:attr:currentValue"
        })
#endif

-- VVV Prop "group"
   -- Type: TInterface (Name {namespace = "Gtk", name = "RadioAction"})
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@group@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioAction [ #group 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionGroup :: (MonadIO m, IsRadioAction o, IsRadioAction a) => o -> a -> m ()
setRadioActionGroup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "group" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@group@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRadioActionGroup :: (IsRadioAction o, MIO.MonadIO m, IsRadioAction a) => a -> m (GValueConstruct o)
constructRadioActionGroup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "group" (P.Just val)

-- | Set the value of the “@group@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #group
-- @
clearRadioActionGroup :: (MonadIO m, IsRadioAction o) => o -> m ()
clearRadioActionGroup obj = liftIO $ B.Properties.setObjectPropertyObject obj "group" (Nothing :: Maybe RadioAction)

#if defined(ENABLE_OVERLOADING)
data RadioActionGroupPropertyInfo
instance AttrInfo RadioActionGroupPropertyInfo where
    type AttrAllowedOps RadioActionGroupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint RadioActionGroupPropertyInfo = IsRadioAction
    type AttrSetTypeConstraint RadioActionGroupPropertyInfo = IsRadioAction
    type AttrTransferTypeConstraint RadioActionGroupPropertyInfo = IsRadioAction
    type AttrTransferType RadioActionGroupPropertyInfo = RadioAction
    type AttrGetType RadioActionGroupPropertyInfo = ()
    type AttrLabel RadioActionGroupPropertyInfo = "group"
    type AttrOrigin RadioActionGroupPropertyInfo = RadioAction
    attrGet = undefined
    attrSet = setRadioActionGroup
    attrTransfer _ v = do
        unsafeCastTo RadioAction v
    attrConstruct = constructRadioActionGroup
    attrClear = clearRadioActionGroup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction.group"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#g:attr:group"
        })
#endif

-- VVV Prop "value"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' radioAction #value
-- @
getRadioActionValue :: (MonadIO m, IsRadioAction o) => o -> m Int32
getRadioActionValue obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "value"

-- | Set the value of the “@value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioAction [ #value 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionValue :: (MonadIO m, IsRadioAction o) => o -> Int32 -> m ()
setRadioActionValue obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "value" val

-- | Construct a `GValueConstruct` with valid value for the “@value@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRadioActionValue :: (IsRadioAction o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructRadioActionValue val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "value" val

#if defined(ENABLE_OVERLOADING)
data RadioActionValuePropertyInfo
instance AttrInfo RadioActionValuePropertyInfo where
    type AttrAllowedOps RadioActionValuePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RadioActionValuePropertyInfo = IsRadioAction
    type AttrSetTypeConstraint RadioActionValuePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint RadioActionValuePropertyInfo = (~) Int32
    type AttrTransferType RadioActionValuePropertyInfo = Int32
    type AttrGetType RadioActionValuePropertyInfo = Int32
    type AttrLabel RadioActionValuePropertyInfo = "value"
    type AttrOrigin RadioActionValuePropertyInfo = RadioAction
    attrGet = getRadioActionValue
    attrSet = setRadioActionValue
    attrTransfer _ v = do
        return v
    attrConstruct = constructRadioActionValue
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction.value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#g:attr:value"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RadioAction
type instance O.AttributeList RadioAction = RadioActionAttributeList
type RadioActionAttributeList = ('[ '("actionGroup", Gtk.Action.ActionActionGroupPropertyInfo), '("active", Gtk.ToggleAction.ToggleActionActivePropertyInfo), '("alwaysShowImage", Gtk.Action.ActionAlwaysShowImagePropertyInfo), '("currentValue", RadioActionCurrentValuePropertyInfo), '("drawAsRadio", Gtk.ToggleAction.ToggleActionDrawAsRadioPropertyInfo), '("gicon", Gtk.Action.ActionGiconPropertyInfo), '("group", RadioActionGroupPropertyInfo), '("hideIfEmpty", Gtk.Action.ActionHideIfEmptyPropertyInfo), '("iconName", Gtk.Action.ActionIconNamePropertyInfo), '("isImportant", Gtk.Action.ActionIsImportantPropertyInfo), '("label", Gtk.Action.ActionLabelPropertyInfo), '("name", Gtk.Action.ActionNamePropertyInfo), '("sensitive", Gtk.Action.ActionSensitivePropertyInfo), '("shortLabel", Gtk.Action.ActionShortLabelPropertyInfo), '("stockId", Gtk.Action.ActionStockIdPropertyInfo), '("tooltip", Gtk.Action.ActionTooltipPropertyInfo), '("value", RadioActionValuePropertyInfo), '("visible", Gtk.Action.ActionVisiblePropertyInfo), '("visibleHorizontal", Gtk.Action.ActionVisibleHorizontalPropertyInfo), '("visibleOverflown", Gtk.Action.ActionVisibleOverflownPropertyInfo), '("visibleVertical", Gtk.Action.ActionVisibleVerticalPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
radioActionCurrentValue :: AttrLabelProxy "currentValue"
radioActionCurrentValue = AttrLabelProxy

radioActionGroup :: AttrLabelProxy "group"
radioActionGroup = AttrLabelProxy

radioActionValue :: AttrLabelProxy "value"
radioActionValue = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList RadioAction = RadioActionSignalList
type RadioActionSignalList = ('[ '("activate", Gtk.Action.ActionActivateSignalInfo), '("changed", RadioActionChangedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("toggled", Gtk.ToggleAction.ToggleActionToggledSignalInfo)] :: [(Symbol, *)])

#endif

-- method RadioAction::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A unique name for the action"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The label displayed in menu items and on buttons,\n  or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tooltip"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A tooltip for this action, or %NULL"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText =
--                     Just
--                       "The stock icon to display in widgets representing\n  this action, or %NULL"
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
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The value which gtk_radio_action_get_current_value() should\n  return if this action is selected."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "RadioAction" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_radio_action_new" gtk_radio_action_new :: 
    CString ->                              -- name : TBasicType TUTF8
    CString ->                              -- label : TBasicType TUTF8
    CString ->                              -- tooltip : TBasicType TUTF8
    CString ->                              -- stock_id : TBasicType TUTF8
    Int32 ->                                -- value : TBasicType TInt
    IO (Ptr RadioAction)

{-# DEPRECATED radioActionNew ["(Since version 3.10)"] #-}
-- | Creates a new t'GI.Gtk.Objects.RadioAction.RadioAction' object. To add the action to
-- a t'GI.Gtk.Objects.ActionGroup.ActionGroup' and set the accelerator for the action,
-- call 'GI.Gtk.Objects.ActionGroup.actionGroupAddActionWithAccel'.
-- 
-- /Since: 2.4/
radioActionNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@name@/: A unique name for the action
    -> Maybe (T.Text)
    -- ^ /@label@/: The label displayed in menu items and on buttons,
    --   or 'P.Nothing'
    -> Maybe (T.Text)
    -- ^ /@tooltip@/: A tooltip for this action, or 'P.Nothing'
    -> Maybe (T.Text)
    -- ^ /@stockId@/: The stock icon to display in widgets representing
    --   this action, or 'P.Nothing'
    -> Int32
    -- ^ /@value@/: The value which 'GI.Gtk.Objects.RadioAction.radioActionGetCurrentValue' should
    --   return if this action is selected.
    -> m RadioAction
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.RadioAction.RadioAction'
radioActionNew name label tooltip stockId value = liftIO $ do
    name' <- textToCString name
    maybeLabel <- case label of
        Nothing -> return nullPtr
        Just jLabel -> do
            jLabel' <- textToCString jLabel
            return jLabel'
    maybeTooltip <- case tooltip of
        Nothing -> return nullPtr
        Just jTooltip -> do
            jTooltip' <- textToCString jTooltip
            return jTooltip'
    maybeStockId <- case stockId of
        Nothing -> return nullPtr
        Just jStockId -> do
            jStockId' <- textToCString jStockId
            return jStockId'
    result <- gtk_radio_action_new name' maybeLabel maybeTooltip maybeStockId value
    checkUnexpectedReturnNULL "radioActionNew" result
    result' <- (wrapObject RadioAction) result
    freeMem name'
    freeMem maybeLabel
    freeMem maybeTooltip
    freeMem maybeStockId
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RadioAction::get_current_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RadioAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRadioAction" , sinceVersion = Nothing }
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

foreign import ccall "gtk_radio_action_get_current_value" gtk_radio_action_get_current_value :: 
    Ptr RadioAction ->                      -- action : TInterface (Name {namespace = "Gtk", name = "RadioAction"})
    IO Int32

{-# DEPRECATED radioActionGetCurrentValue ["(Since version 3.10)"] #-}
-- | Obtains the value property of the currently active member of
-- the group to which /@action@/ belongs.
-- 
-- /Since: 2.4/
radioActionGetCurrentValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsRadioAction a) =>
    a
    -- ^ /@action@/: a t'GI.Gtk.Objects.RadioAction.RadioAction'
    -> m Int32
    -- ^ __Returns:__ The value of the currently active group member
radioActionGetCurrentValue action = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    result <- gtk_radio_action_get_current_value action'
    touchManagedPtr action
    return result

#if defined(ENABLE_OVERLOADING)
data RadioActionGetCurrentValueMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsRadioAction a) => O.OverloadedMethod RadioActionGetCurrentValueMethodInfo a signature where
    overloadedMethod = radioActionGetCurrentValue

instance O.OverloadedMethodInfo RadioActionGetCurrentValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction.radioActionGetCurrentValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#v:radioActionGetCurrentValue"
        })


#endif

-- method RadioAction::get_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RadioAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action object" , sinceVersion = Nothing }
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
--                  (TInterface Name { namespace = "Gtk" , name = "RadioAction" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_radio_action_get_group" gtk_radio_action_get_group :: 
    Ptr RadioAction ->                      -- action : TInterface (Name {namespace = "Gtk", name = "RadioAction"})
    IO (Ptr (GSList (Ptr RadioAction)))

{-# DEPRECATED radioActionGetGroup ["(Since version 3.10)"] #-}
-- | Returns the list representing the radio group for this object.
-- Note that the returned list is only valid until the next change
-- to the group.
-- 
-- A common way to set up a group of radio group is the following:
-- 
-- === /C code/
-- >
-- >  GSList *group = NULL;
-- >  GtkRadioAction *action;
-- > 
-- >  while ( ...more actions to add... /)
-- >    {
-- >       action = gtk_radio_action_new (...);
-- >       
-- >       gtk_radio_action_set_group (action, group);
-- >       group = gtk_radio_action_get_group (action);
-- >    }
-- 
-- 
-- /Since: 2.4/
radioActionGetGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsRadioAction a) =>
    a
    -- ^ /@action@/: the action object
    -> m [RadioAction]
    -- ^ __Returns:__ the list representing the radio group for this object
radioActionGetGroup action = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    result <- gtk_radio_action_get_group action'
    result' <- unpackGSList result
    result'' <- mapM (newObject RadioAction) result'
    touchManagedPtr action
    return result''

#if defined(ENABLE_OVERLOADING)
data RadioActionGetGroupMethodInfo
instance (signature ~ (m [RadioAction]), MonadIO m, IsRadioAction a) => O.OverloadedMethod RadioActionGetGroupMethodInfo a signature where
    overloadedMethod = radioActionGetGroup

instance O.OverloadedMethodInfo RadioActionGetGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction.radioActionGetGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#v:radioActionGetGroup"
        })


#endif

-- method RadioAction::join_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RadioAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action object" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_source"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RadioAction" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a radio action object whos group we are\n  joining, or %NULL to remove the radio action from its group"
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

foreign import ccall "gtk_radio_action_join_group" gtk_radio_action_join_group :: 
    Ptr RadioAction ->                      -- action : TInterface (Name {namespace = "Gtk", name = "RadioAction"})
    Ptr RadioAction ->                      -- group_source : TInterface (Name {namespace = "Gtk", name = "RadioAction"})
    IO ()

{-# DEPRECATED radioActionJoinGroup ["(Since version 3.10)"] #-}
-- | Joins a radio action object to the group of another radio action object.
-- 
-- Use this in language bindings instead of the 'GI.Gtk.Objects.RadioAction.radioActionGetGroup'
-- and 'GI.Gtk.Objects.RadioAction.radioActionSetGroup' methods
-- 
-- A common way to set up a group of radio actions is the following:
-- 
-- === /C code/
-- >
-- >  GtkRadioAction *action;
-- >  GtkRadioAction *last_action;
-- > 
-- >  while ( ...more actions to add... /)
-- >    {
-- >       action = gtk_radio_action_new (...);
-- >       
-- >       gtk_radio_action_join_group (action, last_action);
-- >       last_action = action;
-- >    }
-- 
-- 
-- /Since: 3.0/
radioActionJoinGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsRadioAction a, IsRadioAction b) =>
    a
    -- ^ /@action@/: the action object
    -> Maybe (b)
    -- ^ /@groupSource@/: a radio action object whos group we are
    --   joining, or 'P.Nothing' to remove the radio action from its group
    -> m ()
radioActionJoinGroup action groupSource = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    maybeGroupSource <- case groupSource of
        Nothing -> return nullPtr
        Just jGroupSource -> do
            jGroupSource' <- unsafeManagedPtrCastPtr jGroupSource
            return jGroupSource'
    gtk_radio_action_join_group action' maybeGroupSource
    touchManagedPtr action
    whenJust groupSource touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data RadioActionJoinGroupMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsRadioAction a, IsRadioAction b) => O.OverloadedMethod RadioActionJoinGroupMethodInfo a signature where
    overloadedMethod = radioActionJoinGroup

instance O.OverloadedMethodInfo RadioActionJoinGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction.radioActionJoinGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#v:radioActionJoinGroup"
        })


#endif

-- method RadioAction::set_current_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RadioAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRadioAction" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "current_value"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_radio_action_set_current_value" gtk_radio_action_set_current_value :: 
    Ptr RadioAction ->                      -- action : TInterface (Name {namespace = "Gtk", name = "RadioAction"})
    Int32 ->                                -- current_value : TBasicType TInt
    IO ()

{-# DEPRECATED radioActionSetCurrentValue ["(Since version 3.10)"] #-}
-- | Sets the currently active group member to the member with value
-- property /@currentValue@/.
-- 
-- /Since: 2.10/
radioActionSetCurrentValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsRadioAction a) =>
    a
    -- ^ /@action@/: a t'GI.Gtk.Objects.RadioAction.RadioAction'
    -> Int32
    -- ^ /@currentValue@/: the new value
    -> m ()
radioActionSetCurrentValue action currentValue = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    gtk_radio_action_set_current_value action' currentValue
    touchManagedPtr action
    return ()

#if defined(ENABLE_OVERLOADING)
data RadioActionSetCurrentValueMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsRadioAction a) => O.OverloadedMethod RadioActionSetCurrentValueMethodInfo a signature where
    overloadedMethod = radioActionSetCurrentValue

instance O.OverloadedMethodInfo RadioActionSetCurrentValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction.radioActionSetCurrentValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#v:radioActionSetCurrentValue"
        })


#endif

-- method RadioAction::set_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RadioAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action object" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group"
--           , argType =
--               TGSList
--                 (TInterface Name { namespace = "Gtk" , name = "RadioAction" })
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a list representing a radio group, or %NULL"
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

foreign import ccall "gtk_radio_action_set_group" gtk_radio_action_set_group :: 
    Ptr RadioAction ->                      -- action : TInterface (Name {namespace = "Gtk", name = "RadioAction"})
    Ptr (GSList (Ptr RadioAction)) ->       -- group : TGSList (TInterface (Name {namespace = "Gtk", name = "RadioAction"}))
    IO ()

{-# DEPRECATED radioActionSetGroup ["(Since version 3.10)"] #-}
-- | Sets the radio group for the radio action object.
-- 
-- /Since: 2.4/
radioActionSetGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsRadioAction a, IsRadioAction b) =>
    a
    -- ^ /@action@/: the action object
    -> [b]
    -- ^ /@group@/: a list representing a radio group, or 'P.Nothing'
    -> m ()
radioActionSetGroup action group = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    group' <- mapM unsafeManagedPtrCastPtr group
    group'' <- packGSList group'
    gtk_radio_action_set_group action' group''
    touchManagedPtr action
    mapM_ touchManagedPtr group
    g_slist_free group''
    return ()

#if defined(ENABLE_OVERLOADING)
data RadioActionSetGroupMethodInfo
instance (signature ~ ([b] -> m ()), MonadIO m, IsRadioAction a, IsRadioAction b) => O.OverloadedMethod RadioActionSetGroupMethodInfo a signature where
    overloadedMethod = radioActionSetGroup

instance O.OverloadedMethodInfo RadioActionSetGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RadioAction.radioActionSetGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RadioAction.html#v:radioActionSetGroup"
        })


#endif


