{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.ToggleAction.ToggleAction' corresponds roughly to a t'GI.Gtk.Objects.CheckMenuItem.CheckMenuItem'. It has an
-- “active” state specifying whether the action has been checked or not.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ToggleAction
    ( 

-- * Exported types
    ToggleAction(..)                        ,
    IsToggleAction                          ,
    toToggleAction                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Action#g:method:activate"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [blockActivate]("GI.Gtk.Objects.Action#g:method:blockActivate"), [connectAccelerator]("GI.Gtk.Objects.Action#g:method:connectAccelerator"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createIcon]("GI.Gtk.Objects.Action#g:method:createIcon"), [createMenu]("GI.Gtk.Objects.Action#g:method:createMenu"), [createMenuItem]("GI.Gtk.Objects.Action#g:method:createMenuItem"), [createToolItem]("GI.Gtk.Objects.Action#g:method:createToolItem"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [disconnectAccelerator]("GI.Gtk.Objects.Action#g:method:disconnectAccelerator"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isSensitive]("GI.Gtk.Objects.Action#g:method:isSensitive"), [isVisible]("GI.Gtk.Objects.Action#g:method:isVisible"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toggled]("GI.Gtk.Objects.ToggleAction#g:method:toggled"), [unblockActivate]("GI.Gtk.Objects.Action#g:method:unblockActivate"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccelClosure]("GI.Gtk.Objects.Action#g:method:getAccelClosure"), [getAccelPath]("GI.Gtk.Objects.Action#g:method:getAccelPath"), [getActive]("GI.Gtk.Objects.ToggleAction#g:method:getActive"), [getAlwaysShowImage]("GI.Gtk.Objects.Action#g:method:getAlwaysShowImage"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDrawAsRadio]("GI.Gtk.Objects.ToggleAction#g:method:getDrawAsRadio"), [getGicon]("GI.Gtk.Objects.Action#g:method:getGicon"), [getIconName]("GI.Gtk.Objects.Action#g:method:getIconName"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getIsImportant]("GI.Gtk.Objects.Action#g:method:getIsImportant"), [getLabel]("GI.Gtk.Objects.Action#g:method:getLabel"), [getName]("GI.Gtk.Objects.Action#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getProxies]("GI.Gtk.Objects.Action#g:method:getProxies"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSensitive]("GI.Gtk.Objects.Action#g:method:getSensitive"), [getShortLabel]("GI.Gtk.Objects.Action#g:method:getShortLabel"), [getStockId]("GI.Gtk.Objects.Action#g:method:getStockId"), [getTooltip]("GI.Gtk.Objects.Action#g:method:getTooltip"), [getVisible]("GI.Gtk.Objects.Action#g:method:getVisible"), [getVisibleHorizontal]("GI.Gtk.Objects.Action#g:method:getVisibleHorizontal"), [getVisibleVertical]("GI.Gtk.Objects.Action#g:method:getVisibleVertical").
-- 
-- ==== Setters
-- [setAccelGroup]("GI.Gtk.Objects.Action#g:method:setAccelGroup"), [setAccelPath]("GI.Gtk.Objects.Action#g:method:setAccelPath"), [setActive]("GI.Gtk.Objects.ToggleAction#g:method:setActive"), [setAlwaysShowImage]("GI.Gtk.Objects.Action#g:method:setAlwaysShowImage"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDrawAsRadio]("GI.Gtk.Objects.ToggleAction#g:method:setDrawAsRadio"), [setGicon]("GI.Gtk.Objects.Action#g:method:setGicon"), [setIconName]("GI.Gtk.Objects.Action#g:method:setIconName"), [setIsImportant]("GI.Gtk.Objects.Action#g:method:setIsImportant"), [setLabel]("GI.Gtk.Objects.Action#g:method:setLabel"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSensitive]("GI.Gtk.Objects.Action#g:method:setSensitive"), [setShortLabel]("GI.Gtk.Objects.Action#g:method:setShortLabel"), [setStockId]("GI.Gtk.Objects.Action#g:method:setStockId"), [setTooltip]("GI.Gtk.Objects.Action#g:method:setTooltip"), [setVisible]("GI.Gtk.Objects.Action#g:method:setVisible"), [setVisibleHorizontal]("GI.Gtk.Objects.Action#g:method:setVisibleHorizontal"), [setVisibleVertical]("GI.Gtk.Objects.Action#g:method:setVisibleVertical").

#if defined(ENABLE_OVERLOADING)
    ResolveToggleActionMethod               ,
#endif

-- ** getActive #method:getActive#

#if defined(ENABLE_OVERLOADING)
    ToggleActionGetActiveMethodInfo         ,
#endif
    toggleActionGetActive                   ,


-- ** getDrawAsRadio #method:getDrawAsRadio#

#if defined(ENABLE_OVERLOADING)
    ToggleActionGetDrawAsRadioMethodInfo    ,
#endif
    toggleActionGetDrawAsRadio              ,


-- ** new #method:new#

    toggleActionNew                         ,


-- ** setActive #method:setActive#

#if defined(ENABLE_OVERLOADING)
    ToggleActionSetActiveMethodInfo         ,
#endif
    toggleActionSetActive                   ,


-- ** setDrawAsRadio #method:setDrawAsRadio#

#if defined(ENABLE_OVERLOADING)
    ToggleActionSetDrawAsRadioMethodInfo    ,
#endif
    toggleActionSetDrawAsRadio              ,


-- ** toggled #method:toggled#

#if defined(ENABLE_OVERLOADING)
    ToggleActionToggledMethodInfo           ,
#endif
    toggleActionToggled                     ,




 -- * Properties


-- ** active #attr:active#
-- | Whether the toggle action should be active.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    ToggleActionActivePropertyInfo          ,
#endif
    constructToggleActionActive             ,
    getToggleActionActive                   ,
    setToggleActionActive                   ,
#if defined(ENABLE_OVERLOADING)
    toggleActionActive                      ,
#endif


-- ** drawAsRadio #attr:drawAsRadio#
-- | Whether the proxies for this action look like radio action proxies.
-- 
-- This is an appearance property and thus only applies if
-- t'GI.Gtk.Interfaces.Activatable.Activatable':@/use-action-appearance/@ is 'P.True'.

#if defined(ENABLE_OVERLOADING)
    ToggleActionDrawAsRadioPropertyInfo     ,
#endif
    constructToggleActionDrawAsRadio        ,
    getToggleActionDrawAsRadio              ,
    setToggleActionDrawAsRadio              ,
#if defined(ENABLE_OVERLOADING)
    toggleActionDrawAsRadio                 ,
#endif




 -- * Signals


-- ** toggled #signal:toggled#

    ToggleActionToggledCallback             ,
#if defined(ENABLE_OVERLOADING)
    ToggleActionToggledSignalInfo           ,
#endif
    afterToggleActionToggled                ,
    onToggleActionToggled                   ,




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

-- | Memory-managed wrapper type.
newtype ToggleAction = ToggleAction (SP.ManagedPtr ToggleAction)
    deriving (Eq)

instance SP.ManagedPtrNewtype ToggleAction where
    toManagedPtr (ToggleAction p) = p

foreign import ccall "gtk_toggle_action_get_type"
    c_gtk_toggle_action_get_type :: IO B.Types.GType

instance B.Types.TypedObject ToggleAction where
    glibType = c_gtk_toggle_action_get_type

instance B.Types.GObject ToggleAction

-- | Type class for types which can be safely cast to `ToggleAction`, for instance with `toToggleAction`.
class (SP.GObject o, O.IsDescendantOf ToggleAction o) => IsToggleAction o
instance (SP.GObject o, O.IsDescendantOf ToggleAction o) => IsToggleAction o

instance O.HasParentTypes ToggleAction
type instance O.ParentTypes ToggleAction = '[Gtk.Action.Action, GObject.Object.Object, Gtk.Buildable.Buildable]

-- | Cast to `ToggleAction`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toToggleAction :: (MIO.MonadIO m, IsToggleAction o) => o -> m ToggleAction
toToggleAction = MIO.liftIO . B.ManagedPtr.unsafeCastTo ToggleAction

-- | Convert 'ToggleAction' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ToggleAction) where
    gvalueGType_ = c_gtk_toggle_action_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ToggleAction)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ToggleAction)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ToggleAction ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveToggleActionMethod (t :: Symbol) (o :: *) :: * where
    ResolveToggleActionMethod "activate" o = Gtk.Action.ActionActivateMethodInfo
    ResolveToggleActionMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveToggleActionMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveToggleActionMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveToggleActionMethod "blockActivate" o = Gtk.Action.ActionBlockActivateMethodInfo
    ResolveToggleActionMethod "connectAccelerator" o = Gtk.Action.ActionConnectAcceleratorMethodInfo
    ResolveToggleActionMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveToggleActionMethod "createIcon" o = Gtk.Action.ActionCreateIconMethodInfo
    ResolveToggleActionMethod "createMenu" o = Gtk.Action.ActionCreateMenuMethodInfo
    ResolveToggleActionMethod "createMenuItem" o = Gtk.Action.ActionCreateMenuItemMethodInfo
    ResolveToggleActionMethod "createToolItem" o = Gtk.Action.ActionCreateToolItemMethodInfo
    ResolveToggleActionMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveToggleActionMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveToggleActionMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveToggleActionMethod "disconnectAccelerator" o = Gtk.Action.ActionDisconnectAcceleratorMethodInfo
    ResolveToggleActionMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveToggleActionMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveToggleActionMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveToggleActionMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveToggleActionMethod "isSensitive" o = Gtk.Action.ActionIsSensitiveMethodInfo
    ResolveToggleActionMethod "isVisible" o = Gtk.Action.ActionIsVisibleMethodInfo
    ResolveToggleActionMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveToggleActionMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveToggleActionMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveToggleActionMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveToggleActionMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveToggleActionMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveToggleActionMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveToggleActionMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveToggleActionMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveToggleActionMethod "toggled" o = ToggleActionToggledMethodInfo
    ResolveToggleActionMethod "unblockActivate" o = Gtk.Action.ActionUnblockActivateMethodInfo
    ResolveToggleActionMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveToggleActionMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveToggleActionMethod "getAccelClosure" o = Gtk.Action.ActionGetAccelClosureMethodInfo
    ResolveToggleActionMethod "getAccelPath" o = Gtk.Action.ActionGetAccelPathMethodInfo
    ResolveToggleActionMethod "getActive" o = ToggleActionGetActiveMethodInfo
    ResolveToggleActionMethod "getAlwaysShowImage" o = Gtk.Action.ActionGetAlwaysShowImageMethodInfo
    ResolveToggleActionMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveToggleActionMethod "getDrawAsRadio" o = ToggleActionGetDrawAsRadioMethodInfo
    ResolveToggleActionMethod "getGicon" o = Gtk.Action.ActionGetGiconMethodInfo
    ResolveToggleActionMethod "getIconName" o = Gtk.Action.ActionGetIconNameMethodInfo
    ResolveToggleActionMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveToggleActionMethod "getIsImportant" o = Gtk.Action.ActionGetIsImportantMethodInfo
    ResolveToggleActionMethod "getLabel" o = Gtk.Action.ActionGetLabelMethodInfo
    ResolveToggleActionMethod "getName" o = Gtk.Action.ActionGetNameMethodInfo
    ResolveToggleActionMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveToggleActionMethod "getProxies" o = Gtk.Action.ActionGetProxiesMethodInfo
    ResolveToggleActionMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveToggleActionMethod "getSensitive" o = Gtk.Action.ActionGetSensitiveMethodInfo
    ResolveToggleActionMethod "getShortLabel" o = Gtk.Action.ActionGetShortLabelMethodInfo
    ResolveToggleActionMethod "getStockId" o = Gtk.Action.ActionGetStockIdMethodInfo
    ResolveToggleActionMethod "getTooltip" o = Gtk.Action.ActionGetTooltipMethodInfo
    ResolveToggleActionMethod "getVisible" o = Gtk.Action.ActionGetVisibleMethodInfo
    ResolveToggleActionMethod "getVisibleHorizontal" o = Gtk.Action.ActionGetVisibleHorizontalMethodInfo
    ResolveToggleActionMethod "getVisibleVertical" o = Gtk.Action.ActionGetVisibleVerticalMethodInfo
    ResolveToggleActionMethod "setAccelGroup" o = Gtk.Action.ActionSetAccelGroupMethodInfo
    ResolveToggleActionMethod "setAccelPath" o = Gtk.Action.ActionSetAccelPathMethodInfo
    ResolveToggleActionMethod "setActive" o = ToggleActionSetActiveMethodInfo
    ResolveToggleActionMethod "setAlwaysShowImage" o = Gtk.Action.ActionSetAlwaysShowImageMethodInfo
    ResolveToggleActionMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveToggleActionMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveToggleActionMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveToggleActionMethod "setDrawAsRadio" o = ToggleActionSetDrawAsRadioMethodInfo
    ResolveToggleActionMethod "setGicon" o = Gtk.Action.ActionSetGiconMethodInfo
    ResolveToggleActionMethod "setIconName" o = Gtk.Action.ActionSetIconNameMethodInfo
    ResolveToggleActionMethod "setIsImportant" o = Gtk.Action.ActionSetIsImportantMethodInfo
    ResolveToggleActionMethod "setLabel" o = Gtk.Action.ActionSetLabelMethodInfo
    ResolveToggleActionMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveToggleActionMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveToggleActionMethod "setSensitive" o = Gtk.Action.ActionSetSensitiveMethodInfo
    ResolveToggleActionMethod "setShortLabel" o = Gtk.Action.ActionSetShortLabelMethodInfo
    ResolveToggleActionMethod "setStockId" o = Gtk.Action.ActionSetStockIdMethodInfo
    ResolveToggleActionMethod "setTooltip" o = Gtk.Action.ActionSetTooltipMethodInfo
    ResolveToggleActionMethod "setVisible" o = Gtk.Action.ActionSetVisibleMethodInfo
    ResolveToggleActionMethod "setVisibleHorizontal" o = Gtk.Action.ActionSetVisibleHorizontalMethodInfo
    ResolveToggleActionMethod "setVisibleVertical" o = Gtk.Action.ActionSetVisibleVerticalMethodInfo
    ResolveToggleActionMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveToggleActionMethod t ToggleAction, O.OverloadedMethod info ToggleAction p) => OL.IsLabel t (ToggleAction -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveToggleActionMethod t ToggleAction, O.OverloadedMethod info ToggleAction p, R.HasField t ToggleAction p) => R.HasField t ToggleAction p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveToggleActionMethod t ToggleAction, O.OverloadedMethodInfo info ToggleAction) => OL.IsLabel t (O.MethodProxy info ToggleAction) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal ToggleAction::toggled
{-# DEPRECATED ToggleActionToggledCallback ["(Since version 3.10)"] #-}
-- | Should be connected if you wish to perform an action
-- whenever the t'GI.Gtk.Objects.ToggleAction.ToggleAction' state is changed.
type ToggleActionToggledCallback =
    IO ()

type C_ToggleActionToggledCallback =
    Ptr ToggleAction ->                     -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ToggleActionToggledCallback`.
foreign import ccall "wrapper"
    mk_ToggleActionToggledCallback :: C_ToggleActionToggledCallback -> IO (FunPtr C_ToggleActionToggledCallback)

wrap_ToggleActionToggledCallback :: 
    GObject a => (a -> ToggleActionToggledCallback) ->
    C_ToggleActionToggledCallback
wrap_ToggleActionToggledCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [toggled](#signal:toggled) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' toggleAction #toggled callback
-- @
-- 
-- 
onToggleActionToggled :: (IsToggleAction a, MonadIO m) => a -> ((?self :: a) => ToggleActionToggledCallback) -> m SignalHandlerId
onToggleActionToggled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ToggleActionToggledCallback wrapped
    wrapped'' <- mk_ToggleActionToggledCallback wrapped'
    connectSignalFunPtr obj "toggled" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggled](#signal:toggled) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' toggleAction #toggled callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterToggleActionToggled :: (IsToggleAction a, MonadIO m) => a -> ((?self :: a) => ToggleActionToggledCallback) -> m SignalHandlerId
afterToggleActionToggled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ToggleActionToggledCallback wrapped
    wrapped'' <- mk_ToggleActionToggledCallback wrapped'
    connectSignalFunPtr obj "toggled" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ToggleActionToggledSignalInfo
instance SignalInfo ToggleActionToggledSignalInfo where
    type HaskellCallbackType ToggleActionToggledSignalInfo = ToggleActionToggledCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ToggleActionToggledCallback cb
        cb'' <- mk_ToggleActionToggledCallback cb'
        connectSignalFunPtr obj "toggled" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleAction::toggled"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleAction.html#g:signal:toggled"})

#endif

-- VVV Prop "active"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleAction #active
-- @
getToggleActionActive :: (MonadIO m, IsToggleAction o) => o -> m Bool
getToggleActionActive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "active"

-- | Set the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleAction [ #active 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionActive :: (MonadIO m, IsToggleAction o) => o -> Bool -> m ()
setToggleActionActive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "active" val

-- | Construct a `GValueConstruct` with valid value for the “@active@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructToggleActionActive :: (IsToggleAction o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructToggleActionActive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "active" val

#if defined(ENABLE_OVERLOADING)
data ToggleActionActivePropertyInfo
instance AttrInfo ToggleActionActivePropertyInfo where
    type AttrAllowedOps ToggleActionActivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ToggleActionActivePropertyInfo = IsToggleAction
    type AttrSetTypeConstraint ToggleActionActivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ToggleActionActivePropertyInfo = (~) Bool
    type AttrTransferType ToggleActionActivePropertyInfo = Bool
    type AttrGetType ToggleActionActivePropertyInfo = Bool
    type AttrLabel ToggleActionActivePropertyInfo = "active"
    type AttrOrigin ToggleActionActivePropertyInfo = ToggleAction
    attrGet = getToggleActionActive
    attrSet = setToggleActionActive
    attrTransfer _ v = do
        return v
    attrConstruct = constructToggleActionActive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleAction.active"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleAction.html#g:attr:active"
        })
#endif

-- VVV Prop "draw-as-radio"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@draw-as-radio@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleAction #drawAsRadio
-- @
getToggleActionDrawAsRadio :: (MonadIO m, IsToggleAction o) => o -> m Bool
getToggleActionDrawAsRadio obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "draw-as-radio"

-- | Set the value of the “@draw-as-radio@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleAction [ #drawAsRadio 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionDrawAsRadio :: (MonadIO m, IsToggleAction o) => o -> Bool -> m ()
setToggleActionDrawAsRadio obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "draw-as-radio" val

-- | Construct a `GValueConstruct` with valid value for the “@draw-as-radio@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructToggleActionDrawAsRadio :: (IsToggleAction o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructToggleActionDrawAsRadio val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "draw-as-radio" val

#if defined(ENABLE_OVERLOADING)
data ToggleActionDrawAsRadioPropertyInfo
instance AttrInfo ToggleActionDrawAsRadioPropertyInfo where
    type AttrAllowedOps ToggleActionDrawAsRadioPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ToggleActionDrawAsRadioPropertyInfo = IsToggleAction
    type AttrSetTypeConstraint ToggleActionDrawAsRadioPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ToggleActionDrawAsRadioPropertyInfo = (~) Bool
    type AttrTransferType ToggleActionDrawAsRadioPropertyInfo = Bool
    type AttrGetType ToggleActionDrawAsRadioPropertyInfo = Bool
    type AttrLabel ToggleActionDrawAsRadioPropertyInfo = "draw-as-radio"
    type AttrOrigin ToggleActionDrawAsRadioPropertyInfo = ToggleAction
    attrGet = getToggleActionDrawAsRadio
    attrSet = setToggleActionDrawAsRadio
    attrTransfer _ v = do
        return v
    attrConstruct = constructToggleActionDrawAsRadio
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleAction.drawAsRadio"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleAction.html#g:attr:drawAsRadio"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ToggleAction
type instance O.AttributeList ToggleAction = ToggleActionAttributeList
type ToggleActionAttributeList = ('[ '("actionGroup", Gtk.Action.ActionActionGroupPropertyInfo), '("active", ToggleActionActivePropertyInfo), '("alwaysShowImage", Gtk.Action.ActionAlwaysShowImagePropertyInfo), '("drawAsRadio", ToggleActionDrawAsRadioPropertyInfo), '("gicon", Gtk.Action.ActionGiconPropertyInfo), '("hideIfEmpty", Gtk.Action.ActionHideIfEmptyPropertyInfo), '("iconName", Gtk.Action.ActionIconNamePropertyInfo), '("isImportant", Gtk.Action.ActionIsImportantPropertyInfo), '("label", Gtk.Action.ActionLabelPropertyInfo), '("name", Gtk.Action.ActionNamePropertyInfo), '("sensitive", Gtk.Action.ActionSensitivePropertyInfo), '("shortLabel", Gtk.Action.ActionShortLabelPropertyInfo), '("stockId", Gtk.Action.ActionStockIdPropertyInfo), '("tooltip", Gtk.Action.ActionTooltipPropertyInfo), '("visible", Gtk.Action.ActionVisiblePropertyInfo), '("visibleHorizontal", Gtk.Action.ActionVisibleHorizontalPropertyInfo), '("visibleOverflown", Gtk.Action.ActionVisibleOverflownPropertyInfo), '("visibleVertical", Gtk.Action.ActionVisibleVerticalPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
toggleActionActive :: AttrLabelProxy "active"
toggleActionActive = AttrLabelProxy

toggleActionDrawAsRadio :: AttrLabelProxy "drawAsRadio"
toggleActionDrawAsRadio = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ToggleAction = ToggleActionSignalList
type ToggleActionSignalList = ('[ '("activate", Gtk.Action.ActionActivateSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("toggled", ToggleActionToggledSignalInfo)] :: [(Symbol, *)])

#endif

-- method ToggleAction::new
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
--                       "The label displayed in menu items and on buttons,\n        or %NULL"
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
--                 { rawDocText = Just "A tooltip for the action, or %NULL"
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
--                       "The stock icon to display in widgets representing\n           the action, or %NULL"
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
--               (TInterface Name { namespace = "Gtk" , name = "ToggleAction" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_toggle_action_new" gtk_toggle_action_new :: 
    CString ->                              -- name : TBasicType TUTF8
    CString ->                              -- label : TBasicType TUTF8
    CString ->                              -- tooltip : TBasicType TUTF8
    CString ->                              -- stock_id : TBasicType TUTF8
    IO (Ptr ToggleAction)

{-# DEPRECATED toggleActionNew ["(Since version 3.10)"] #-}
-- | Creates a new t'GI.Gtk.Objects.ToggleAction.ToggleAction' object. To add the action to
-- a t'GI.Gtk.Objects.ActionGroup.ActionGroup' and set the accelerator for the action,
-- call 'GI.Gtk.Objects.ActionGroup.actionGroupAddActionWithAccel'.
-- 
-- /Since: 2.4/
toggleActionNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@name@/: A unique name for the action
    -> Maybe (T.Text)
    -- ^ /@label@/: The label displayed in menu items and on buttons,
    --         or 'P.Nothing'
    -> Maybe (T.Text)
    -- ^ /@tooltip@/: A tooltip for the action, or 'P.Nothing'
    -> Maybe (T.Text)
    -- ^ /@stockId@/: The stock icon to display in widgets representing
    --            the action, or 'P.Nothing'
    -> m ToggleAction
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.ToggleAction.ToggleAction'
toggleActionNew name label tooltip stockId = liftIO $ do
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
    result <- gtk_toggle_action_new name' maybeLabel maybeTooltip maybeStockId
    checkUnexpectedReturnNULL "toggleActionNew" result
    result' <- (wrapObject ToggleAction) result
    freeMem name'
    freeMem maybeLabel
    freeMem maybeTooltip
    freeMem maybeStockId
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ToggleAction::get_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleAction" }
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_toggle_action_get_active" gtk_toggle_action_get_active :: 
    Ptr ToggleAction ->                     -- action : TInterface (Name {namespace = "Gtk", name = "ToggleAction"})
    IO CInt

{-# DEPRECATED toggleActionGetActive ["(Since version 3.10)"] #-}
-- | Returns the checked state of the toggle action.
-- 
-- /Since: 2.4/
toggleActionGetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleAction a) =>
    a
    -- ^ /@action@/: the action object
    -> m Bool
    -- ^ __Returns:__ the checked state of the toggle action
toggleActionGetActive action = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    result <- gtk_toggle_action_get_active action'
    let result' = (/= 0) result
    touchManagedPtr action
    return result'

#if defined(ENABLE_OVERLOADING)
data ToggleActionGetActiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsToggleAction a) => O.OverloadedMethod ToggleActionGetActiveMethodInfo a signature where
    overloadedMethod = toggleActionGetActive

instance O.OverloadedMethodInfo ToggleActionGetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleAction.toggleActionGetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleAction.html#v:toggleActionGetActive"
        })


#endif

-- method ToggleAction::get_draw_as_radio
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleAction" }
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_toggle_action_get_draw_as_radio" gtk_toggle_action_get_draw_as_radio :: 
    Ptr ToggleAction ->                     -- action : TInterface (Name {namespace = "Gtk", name = "ToggleAction"})
    IO CInt

{-# DEPRECATED toggleActionGetDrawAsRadio ["(Since version 3.10)"] #-}
-- | Returns whether the action should have proxies like a radio action.
-- 
-- /Since: 2.4/
toggleActionGetDrawAsRadio ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleAction a) =>
    a
    -- ^ /@action@/: the action object
    -> m Bool
    -- ^ __Returns:__ whether the action should have proxies like a radio action.
toggleActionGetDrawAsRadio action = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    result <- gtk_toggle_action_get_draw_as_radio action'
    let result' = (/= 0) result
    touchManagedPtr action
    return result'

#if defined(ENABLE_OVERLOADING)
data ToggleActionGetDrawAsRadioMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsToggleAction a) => O.OverloadedMethod ToggleActionGetDrawAsRadioMethodInfo a signature where
    overloadedMethod = toggleActionGetDrawAsRadio

instance O.OverloadedMethodInfo ToggleActionGetDrawAsRadioMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleAction.toggleActionGetDrawAsRadio",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleAction.html#v:toggleActionGetDrawAsRadio"
        })


#endif

-- method ToggleAction::set_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleAction" }
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
--           { argCName = "is_active"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the action should be checked or not"
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

foreign import ccall "gtk_toggle_action_set_active" gtk_toggle_action_set_active :: 
    Ptr ToggleAction ->                     -- action : TInterface (Name {namespace = "Gtk", name = "ToggleAction"})
    CInt ->                                 -- is_active : TBasicType TBoolean
    IO ()

{-# DEPRECATED toggleActionSetActive ["(Since version 3.10)"] #-}
-- | Sets the checked state on the toggle action.
-- 
-- /Since: 2.4/
toggleActionSetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleAction a) =>
    a
    -- ^ /@action@/: the action object
    -> Bool
    -- ^ /@isActive@/: whether the action should be checked or not
    -> m ()
toggleActionSetActive action isActive = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    let isActive' = (fromIntegral . fromEnum) isActive
    gtk_toggle_action_set_active action' isActive'
    touchManagedPtr action
    return ()

#if defined(ENABLE_OVERLOADING)
data ToggleActionSetActiveMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsToggleAction a) => O.OverloadedMethod ToggleActionSetActiveMethodInfo a signature where
    overloadedMethod = toggleActionSetActive

instance O.OverloadedMethodInfo ToggleActionSetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleAction.toggleActionSetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleAction.html#v:toggleActionSetActive"
        })


#endif

-- method ToggleAction::set_draw_as_radio
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleAction" }
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
--           { argCName = "draw_as_radio"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether the action should have proxies like a radio\n   action"
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

foreign import ccall "gtk_toggle_action_set_draw_as_radio" gtk_toggle_action_set_draw_as_radio :: 
    Ptr ToggleAction ->                     -- action : TInterface (Name {namespace = "Gtk", name = "ToggleAction"})
    CInt ->                                 -- draw_as_radio : TBasicType TBoolean
    IO ()

{-# DEPRECATED toggleActionSetDrawAsRadio ["(Since version 3.10)"] #-}
-- | Sets whether the action should have proxies like a radio action.
-- 
-- /Since: 2.4/
toggleActionSetDrawAsRadio ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleAction a) =>
    a
    -- ^ /@action@/: the action object
    -> Bool
    -- ^ /@drawAsRadio@/: whether the action should have proxies like a radio
    --    action
    -> m ()
toggleActionSetDrawAsRadio action drawAsRadio = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    let drawAsRadio' = (fromIntegral . fromEnum) drawAsRadio
    gtk_toggle_action_set_draw_as_radio action' drawAsRadio'
    touchManagedPtr action
    return ()

#if defined(ENABLE_OVERLOADING)
data ToggleActionSetDrawAsRadioMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsToggleAction a) => O.OverloadedMethod ToggleActionSetDrawAsRadioMethodInfo a signature where
    overloadedMethod = toggleActionSetDrawAsRadio

instance O.OverloadedMethodInfo ToggleActionSetDrawAsRadioMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleAction.toggleActionSetDrawAsRadio",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleAction.html#v:toggleActionSetDrawAsRadio"
        })


#endif

-- method ToggleAction::toggled
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleAction" }
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_toggle_action_toggled" gtk_toggle_action_toggled :: 
    Ptr ToggleAction ->                     -- action : TInterface (Name {namespace = "Gtk", name = "ToggleAction"})
    IO ()

{-# DEPRECATED toggleActionToggled ["(Since version 3.10)"] #-}
-- | Emits the “toggled” signal on the toggle action.
-- 
-- /Since: 2.4/
toggleActionToggled ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleAction a) =>
    a
    -- ^ /@action@/: the action object
    -> m ()
toggleActionToggled action = liftIO $ do
    action' <- unsafeManagedPtrCastPtr action
    gtk_toggle_action_toggled action'
    touchManagedPtr action
    return ()

#if defined(ENABLE_OVERLOADING)
data ToggleActionToggledMethodInfo
instance (signature ~ (m ()), MonadIO m, IsToggleAction a) => O.OverloadedMethod ToggleActionToggledMethodInfo a signature where
    overloadedMethod = toggleActionToggled

instance O.OverloadedMethodInfo ToggleActionToggledMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleAction.toggleActionToggled",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleAction.html#v:toggleActionToggled"
        })


#endif


