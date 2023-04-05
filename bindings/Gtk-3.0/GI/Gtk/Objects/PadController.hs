{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.PadController.PadController' is an event controller for the pads found in drawing
-- tablets (The collection of buttons and tactile sensors often found around
-- the stylus-sensitive area).
-- 
-- These buttons and sensors have no implicit meaning, and by default they
-- perform no action, this event controller is provided to map those to
-- t'GI.Gio.Interfaces.Action.Action' objects, thus letting the application give those a more semantic
-- meaning.
-- 
-- Buttons and sensors are not constrained to triggering a single action, some
-- 'GI.Gdk.Enums.InputSourceTabletPad' devices feature multiple \"modes\", all these input
-- elements have one current mode, which may determine the final action
-- being triggered. Pad devices often divide buttons and sensors into groups,
-- all elements in a group share the same current mode, but different groups
-- may have different modes. See 'GI.Gdk.Interfaces.DevicePad.devicePadGetNGroups' and
-- 'GI.Gdk.Interfaces.DevicePad.devicePadGetGroupNModes'.
-- 
-- Each of the actions that a given button\/strip\/ring performs for a given
-- mode is defined by t'GI.Gtk.Structs.PadActionEntry.PadActionEntry', it contains an action name that
-- will be looked up in the given t'GI.Gio.Interfaces.ActionGroup.ActionGroup' and activated whenever the
-- specified input element and mode are triggered.
-- 
-- A simple example of t'GI.Gtk.Objects.PadController.PadController' usage, assigning button 1 in all
-- modes and pad devices to an \"invert-selection\" action:
-- >
-- >  GtkPadActionEntry *pad_actions[] = {
-- >    { GTK_PAD_ACTION_BUTTON, 1, -1, "Invert selection", "pad-actions.invert-selection" },
-- >    …
-- >  };
-- >
-- >  …
-- >  action_group = g_simple_action_group_new ();
-- >  action = g_simple_action_new ("pad-actions.invert-selection", NULL);
-- >  g_signal_connect (action, "activate", on_invert_selection_activated, NULL);
-- >  g_action_map_add_action (G_ACTION_MAP (action_group), action);
-- >  …
-- >  pad_controller = gtk_pad_controller_new (window, action_group, NULL);
-- 
-- 
-- The actions belonging to rings\/strips will be activated with a parameter
-- of type @/G_VARIANT_TYPE_DOUBLE/@ bearing the value of the given axis, it
-- is required that those are made stateful and accepting this t'GI.GLib.Structs.VariantType.VariantType'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.PadController
    ( 

-- * Exported types
    PadController(..)                       ,
    IsPadController                         ,
    toPadController                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget").
-- 
-- ==== Setters
-- [setAction]("GI.Gtk.Objects.PadController#g:method:setAction"), [setActionEntries]("GI.Gtk.Objects.PadController#g:method:setActionEntries"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolvePadControllerMethod              ,
#endif

-- ** new #method:new#

    padControllerNew                        ,


-- ** setAction #method:setAction#

#if defined(ENABLE_OVERLOADING)
    PadControllerSetActionMethodInfo        ,
#endif
    padControllerSetAction                  ,


-- ** setActionEntries #method:setActionEntries#

#if defined(ENABLE_OVERLOADING)
    PadControllerSetActionEntriesMethodInfo ,
#endif
    padControllerSetActionEntries           ,




 -- * Properties


-- ** actionGroup #attr:actionGroup#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    PadControllerActionGroupPropertyInfo    ,
#endif
    constructPadControllerActionGroup       ,
    getPadControllerActionGroup             ,
#if defined(ENABLE_OVERLOADING)
    padControllerActionGroup                ,
#endif


-- ** pad #attr:pad#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    PadControllerPadPropertyInfo            ,
#endif
    constructPadControllerPad               ,
    getPadControllerPad                     ,
#if defined(ENABLE_OVERLOADING)
    padControllerPad                        ,
#endif




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
import qualified GI.Gdk.Objects.Device as Gdk.Device
import qualified GI.Gio.Interfaces.ActionGroup as Gio.ActionGroup
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Objects.EventController as Gtk.EventController
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window
import {-# SOURCE #-} qualified GI.Gtk.Structs.PadActionEntry as Gtk.PadActionEntry

-- | Memory-managed wrapper type.
newtype PadController = PadController (SP.ManagedPtr PadController)
    deriving (Eq)

instance SP.ManagedPtrNewtype PadController where
    toManagedPtr (PadController p) = p

foreign import ccall "gtk_pad_controller_get_type"
    c_gtk_pad_controller_get_type :: IO B.Types.GType

instance B.Types.TypedObject PadController where
    glibType = c_gtk_pad_controller_get_type

instance B.Types.GObject PadController

-- | Type class for types which can be safely cast to `PadController`, for instance with `toPadController`.
class (SP.GObject o, O.IsDescendantOf PadController o) => IsPadController o
instance (SP.GObject o, O.IsDescendantOf PadController o) => IsPadController o

instance O.HasParentTypes PadController
type instance O.ParentTypes PadController = '[Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `PadController`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPadController :: (MIO.MonadIO m, IsPadController o) => o -> m PadController
toPadController = MIO.liftIO . B.ManagedPtr.unsafeCastTo PadController

-- | Convert 'PadController' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PadController) where
    gvalueGType_ = c_gtk_pad_controller_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PadController)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PadController)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PadController ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePadControllerMethod (t :: Symbol) (o :: *) :: * where
    ResolvePadControllerMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePadControllerMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePadControllerMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePadControllerMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePadControllerMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePadControllerMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolvePadControllerMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePadControllerMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePadControllerMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePadControllerMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePadControllerMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePadControllerMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolvePadControllerMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePadControllerMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePadControllerMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePadControllerMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePadControllerMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePadControllerMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePadControllerMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePadControllerMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolvePadControllerMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePadControllerMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePadControllerMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolvePadControllerMethod "setAction" o = PadControllerSetActionMethodInfo
    ResolvePadControllerMethod "setActionEntries" o = PadControllerSetActionEntriesMethodInfo
    ResolvePadControllerMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePadControllerMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePadControllerMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolvePadControllerMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePadControllerMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePadControllerMethod t PadController, O.OverloadedMethod info PadController p) => OL.IsLabel t (PadController -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePadControllerMethod t PadController, O.OverloadedMethod info PadController p, R.HasField t PadController p) => R.HasField t PadController p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePadControllerMethod t PadController, O.OverloadedMethodInfo info PadController) => OL.IsLabel t (O.MethodProxy info PadController) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "action-group"
   -- Type: TInterface (Name {namespace = "Gio", name = "ActionGroup"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@action-group@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' padController #actionGroup
-- @
getPadControllerActionGroup :: (MonadIO m, IsPadController o) => o -> m (Maybe Gio.ActionGroup.ActionGroup)
getPadControllerActionGroup obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "action-group" Gio.ActionGroup.ActionGroup

-- | Construct a `GValueConstruct` with valid value for the “@action-group@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPadControllerActionGroup :: (IsPadController o, MIO.MonadIO m, Gio.ActionGroup.IsActionGroup a) => a -> m (GValueConstruct o)
constructPadControllerActionGroup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "action-group" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data PadControllerActionGroupPropertyInfo
instance AttrInfo PadControllerActionGroupPropertyInfo where
    type AttrAllowedOps PadControllerActionGroupPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint PadControllerActionGroupPropertyInfo = IsPadController
    type AttrSetTypeConstraint PadControllerActionGroupPropertyInfo = Gio.ActionGroup.IsActionGroup
    type AttrTransferTypeConstraint PadControllerActionGroupPropertyInfo = Gio.ActionGroup.IsActionGroup
    type AttrTransferType PadControllerActionGroupPropertyInfo = Gio.ActionGroup.ActionGroup
    type AttrGetType PadControllerActionGroupPropertyInfo = (Maybe Gio.ActionGroup.ActionGroup)
    type AttrLabel PadControllerActionGroupPropertyInfo = "action-group"
    type AttrOrigin PadControllerActionGroupPropertyInfo = PadController
    attrGet = getPadControllerActionGroup
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gio.ActionGroup.ActionGroup v
    attrConstruct = constructPadControllerActionGroup
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PadController.actionGroup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PadController.html#g:attr:actionGroup"
        })
#endif

-- VVV Prop "pad"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Device"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pad@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' padController #pad
-- @
getPadControllerPad :: (MonadIO m, IsPadController o) => o -> m (Maybe Gdk.Device.Device)
getPadControllerPad obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "pad" Gdk.Device.Device

-- | Construct a `GValueConstruct` with valid value for the “@pad@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPadControllerPad :: (IsPadController o, MIO.MonadIO m, Gdk.Device.IsDevice a) => a -> m (GValueConstruct o)
constructPadControllerPad val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "pad" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data PadControllerPadPropertyInfo
instance AttrInfo PadControllerPadPropertyInfo where
    type AttrAllowedOps PadControllerPadPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint PadControllerPadPropertyInfo = IsPadController
    type AttrSetTypeConstraint PadControllerPadPropertyInfo = Gdk.Device.IsDevice
    type AttrTransferTypeConstraint PadControllerPadPropertyInfo = Gdk.Device.IsDevice
    type AttrTransferType PadControllerPadPropertyInfo = Gdk.Device.Device
    type AttrGetType PadControllerPadPropertyInfo = (Maybe Gdk.Device.Device)
    type AttrLabel PadControllerPadPropertyInfo = "pad"
    type AttrOrigin PadControllerPadPropertyInfo = PadController
    attrGet = getPadControllerPad
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gdk.Device.Device v
    attrConstruct = constructPadControllerPad
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PadController.pad"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PadController.html#g:attr:pad"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PadController
type instance O.AttributeList PadController = PadControllerAttributeList
type PadControllerAttributeList = ('[ '("actionGroup", PadControllerActionGroupPropertyInfo), '("pad", PadControllerPadPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
padControllerActionGroup :: AttrLabelProxy "actionGroup"
padControllerActionGroup = AttrLabelProxy

padControllerPad :: AttrLabelProxy "pad"
padControllerPad = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PadController = PadControllerSignalList
type PadControllerSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method PadController::new
-- method type : Constructor
-- Args: [ Arg
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
--       , Arg
--           { argCName = "group"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "ActionGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GActionGroup to trigger actions from"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pad"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Device" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A %GDK_SOURCE_TABLET_PAD device, or %NULL to handle all pads"
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
--               (TInterface Name { namespace = "Gtk" , name = "PadController" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_pad_controller_new" gtk_pad_controller_new :: 
    Ptr Gtk.Window.Window ->                -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gio.ActionGroup.ActionGroup ->      -- group : TInterface (Name {namespace = "Gio", name = "ActionGroup"})
    Ptr Gdk.Device.Device ->                -- pad : TInterface (Name {namespace = "Gdk", name = "Device"})
    IO (Ptr PadController)

-- | Creates a new t'GI.Gtk.Objects.PadController.PadController' that will associate events from /@pad@/ to
-- actions. A 'P.Nothing' pad may be provided so the controller manages all pad devices
-- generically, it is discouraged to mix t'GI.Gtk.Objects.PadController.PadController' objects with 'P.Nothing'
-- and non-'P.Nothing' /@pad@/ argument on the same /@window@/, as execution order is not
-- guaranteed.
-- 
-- The t'GI.Gtk.Objects.PadController.PadController' is created with no mapped actions. In order to map pad
-- events to actions, use 'GI.Gtk.Objects.PadController.padControllerSetActionEntries' or
-- 'GI.Gtk.Objects.PadController.padControllerSetAction'.
-- 
-- /Since: 3.22/
padControllerNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Window.IsWindow a, Gio.ActionGroup.IsActionGroup b, Gdk.Device.IsDevice c) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> b
    -- ^ /@group@/: t'GI.Gio.Interfaces.ActionGroup.ActionGroup' to trigger actions from
    -> Maybe (c)
    -- ^ /@pad@/: A 'GI.Gdk.Enums.InputSourceTabletPad' device, or 'P.Nothing' to handle all pads
    -> m PadController
    -- ^ __Returns:__ A newly created t'GI.Gtk.Objects.PadController.PadController'
padControllerNew window group pad = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    group' <- unsafeManagedPtrCastPtr group
    maybePad <- case pad of
        Nothing -> return nullPtr
        Just jPad -> do
            jPad' <- unsafeManagedPtrCastPtr jPad
            return jPad'
    result <- gtk_pad_controller_new window' group' maybePad
    checkUnexpectedReturnNULL "padControllerNew" result
    result' <- (wrapObject PadController) result
    touchManagedPtr window
    touchManagedPtr group
    whenJust pad touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method PadController::set_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "controller"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PadController" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPadController"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PadActionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the type of pad feature that will trigger this action"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "index"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the 0-indexed button/ring/strip number that will trigger this action"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mode"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the mode that will trigger this action, or -1 for all modes."
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Human readable description of this action, this string should\n  be deemed user-visible."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText =
--                     Just "action name that will be activated in the #GActionGroup"
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

foreign import ccall "gtk_pad_controller_set_action" gtk_pad_controller_set_action :: 
    Ptr PadController ->                    -- controller : TInterface (Name {namespace = "Gtk", name = "PadController"})
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "PadActionType"})
    Int32 ->                                -- index : TBasicType TInt
    Int32 ->                                -- mode : TBasicType TInt
    CString ->                              -- label : TBasicType TUTF8
    CString ->                              -- action_name : TBasicType TUTF8
    IO ()

-- | Adds an individual action to /@controller@/. This action will only be activated
-- if the given button\/ring\/strip number in /@index@/ is interacted while
-- the current mode is /@mode@/. -1 may be used for simple cases, so the action
-- is triggered on all modes.
-- 
-- The given /@label@/ should be considered user-visible, so internationalization
-- rules apply. Some windowing systems may be able to use those for user
-- feedback.
-- 
-- /Since: 3.22/
padControllerSetAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsPadController a) =>
    a
    -- ^ /@controller@/: a t'GI.Gtk.Objects.PadController.PadController'
    -> Gtk.Enums.PadActionType
    -- ^ /@type@/: the type of pad feature that will trigger this action
    -> Int32
    -- ^ /@index@/: the 0-indexed button\/ring\/strip number that will trigger this action
    -> Int32
    -- ^ /@mode@/: the mode that will trigger this action, or -1 for all modes.
    -> T.Text
    -- ^ /@label@/: Human readable description of this action, this string should
    --   be deemed user-visible.
    -> T.Text
    -- ^ /@actionName@/: action name that will be activated in the t'GI.Gio.Interfaces.ActionGroup.ActionGroup'
    -> m ()
padControllerSetAction controller type_ index mode label actionName = liftIO $ do
    controller' <- unsafeManagedPtrCastPtr controller
    let type_' = (fromIntegral . fromEnum) type_
    label' <- textToCString label
    actionName' <- textToCString actionName
    gtk_pad_controller_set_action controller' type_' index mode label' actionName'
    touchManagedPtr controller
    freeMem label'
    freeMem actionName'
    return ()

#if defined(ENABLE_OVERLOADING)
data PadControllerSetActionMethodInfo
instance (signature ~ (Gtk.Enums.PadActionType -> Int32 -> Int32 -> T.Text -> T.Text -> m ()), MonadIO m, IsPadController a) => O.OverloadedMethod PadControllerSetActionMethodInfo a signature where
    overloadedMethod = padControllerSetAction

instance O.OverloadedMethodInfo PadControllerSetActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PadController.padControllerSetAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PadController.html#v:padControllerSetAction"
        })


#endif

-- method PadController::set_action_entries
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "controller"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PadController" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPadController"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "entries"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 2
--                 (TInterface Name { namespace = "Gtk" , name = "PadActionEntry" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the action entries to set on @controller"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_entries"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of elements in @entries"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_entries"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the number of elements in @entries"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_pad_controller_set_action_entries" gtk_pad_controller_set_action_entries :: 
    Ptr PadController ->                    -- controller : TInterface (Name {namespace = "Gtk", name = "PadController"})
    Ptr Gtk.PadActionEntry.PadActionEntry -> -- entries : TCArray False (-1) 2 (TInterface (Name {namespace = "Gtk", name = "PadActionEntry"}))
    Int32 ->                                -- n_entries : TBasicType TInt
    IO ()

-- | This is a convenience function to add a group of action entries on
-- /@controller@/. See t'GI.Gtk.Structs.PadActionEntry.PadActionEntry' and 'GI.Gtk.Objects.PadController.padControllerSetAction'.
-- 
-- /Since: 3.22/
padControllerSetActionEntries ::
    (B.CallStack.HasCallStack, MonadIO m, IsPadController a) =>
    a
    -- ^ /@controller@/: a t'GI.Gtk.Objects.PadController.PadController'
    -> [Gtk.PadActionEntry.PadActionEntry]
    -- ^ /@entries@/: the action entries to set on /@controller@/
    -> m ()
padControllerSetActionEntries controller entries = liftIO $ do
    let nEntries = fromIntegral $ P.length entries
    controller' <- unsafeManagedPtrCastPtr controller
    entries' <- mapM unsafeManagedPtrGetPtr entries
    entries'' <- packBlockArray 32 entries'
    gtk_pad_controller_set_action_entries controller' entries'' nEntries
    touchManagedPtr controller
    mapM_ touchManagedPtr entries
    freeMem entries''
    return ()

#if defined(ENABLE_OVERLOADING)
data PadControllerSetActionEntriesMethodInfo
instance (signature ~ ([Gtk.PadActionEntry.PadActionEntry] -> m ()), MonadIO m, IsPadController a) => O.OverloadedMethod PadControllerSetActionEntriesMethodInfo a signature where
    overloadedMethod = padControllerSetActionEntries

instance O.OverloadedMethodInfo PadControllerSetActionEntriesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PadController.padControllerSetActionEntries",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PadController.html#v:padControllerSetActionEntries"
        })


#endif


