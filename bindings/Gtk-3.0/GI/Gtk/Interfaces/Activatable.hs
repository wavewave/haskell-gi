{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Activatable widgets can be connected to a t'GI.Gtk.Objects.Action.Action' and reflects
-- the state of its action. A t'GI.Gtk.Interfaces.Activatable.Activatable' can also provide feedback
-- through its action, as they are responsible for activating their
-- related actions.
-- 
-- = Implementing GtkActivatable
-- 
-- When extending a class that is already t'GI.Gtk.Interfaces.Activatable.Activatable'; it is only
-- necessary to implement the t'GI.Gtk.Interfaces.Activatable.Activatable'->@/sync_action_properties()/@
-- and t'GI.Gtk.Interfaces.Activatable.Activatable'->@/update()/@ methods and chain up to the parent
-- implementation, however when introducing
-- a new t'GI.Gtk.Interfaces.Activatable.Activatable' class; the t'GI.Gtk.Interfaces.Activatable.Activatable':@/related-action/@ and
-- t'GI.Gtk.Interfaces.Activatable.Activatable':@/use-action-appearance/@ properties need to be handled by
-- the implementor. Handling these properties is mostly a matter of installing
-- the action pointer and boolean flag on your instance, and calling
-- 'GI.Gtk.Interfaces.Activatable.activatableDoSetRelatedAction' and
-- 'GI.Gtk.Interfaces.Activatable.activatableSyncActionProperties' at the appropriate times.
-- 
-- ## A class fragment implementing t'GI.Gtk.Interfaces.Activatable.Activatable'
-- 
-- 
-- === /C code/
-- >
-- >
-- >enum {
-- >...
-- >
-- >PROP_ACTIVATABLE_RELATED_ACTION,
-- >PROP_ACTIVATABLE_USE_ACTION_APPEARANCE
-- >}
-- >
-- >struct _FooBarPrivate
-- >{
-- >
-- >  ...
-- >
-- >  GtkAction      *action;
-- >  gboolean        use_action_appearance;
-- >};
-- >
-- >...
-- >
-- >static void foo_bar_activatable_interface_init         (GtkActivatableIface  *iface);
-- >static void foo_bar_activatable_update                 (GtkActivatable       *activatable,
-- >						           GtkAction            *action,
-- >						           const gchar          *property_name);
-- >static void foo_bar_activatable_sync_action_properties (GtkActivatable       *activatable,
-- >						           GtkAction            *action);
-- >...
-- >
-- >
-- >static void
-- >foo_bar_class_init (FooBarClass *klass)
-- >{
-- >
-- >  ...
-- >
-- >  g_object_class_override_property (gobject_class, PROP_ACTIVATABLE_RELATED_ACTION, "related-action");
-- >  g_object_class_override_property (gobject_class, PROP_ACTIVATABLE_USE_ACTION_APPEARANCE, "use-action-appearance");
-- >
-- >  ...
-- >}
-- >
-- >
-- >static void
-- >foo_bar_activatable_interface_init (GtkActivatableIface  *iface)
-- >{
-- >  iface->update = foo_bar_activatable_update;
-- >  iface->sync_action_properties = foo_bar_activatable_sync_action_properties;
-- >}
-- >
-- >... Break the reference using gtk_activatable_do_set_related_action()...
-- >
-- >static void
-- >foo_bar_dispose (GObject *object)
-- >{
-- >  FooBar *bar = FOO_BAR (object);
-- >  FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
-- >
-- >  ...
-- >
-- >  if (priv->action)
-- >    {
-- >      gtk_activatable_do_set_related_action (GTK_ACTIVATABLE (bar), NULL);
-- >      priv->action = NULL;
-- >    }
-- >  G_OBJECT_CLASS (foo_bar_parent_class)->dispose (object);
-- >}
-- >
-- >... Handle the “related-action” and “use-action-appearance” properties ...
-- >
-- >static void
-- >foo_bar_set_property (GObject         *object,
-- >                      guint            prop_id,
-- >                      const GValue    *value,
-- >                      GParamSpec      *pspec)
-- >{
-- >  FooBar *bar = FOO_BAR (object);
-- >  FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
-- >
-- >  switch (prop_id)
-- >    {
-- >
-- >      ...
-- >
-- >    case PROP_ACTIVATABLE_RELATED_ACTION:
-- >      foo_bar_set_related_action (bar, g_value_get_object (value));
-- >      break;
-- >    case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
-- >      foo_bar_set_use_action_appearance (bar, g_value_get_boolean (value));
-- >      break;
-- >    default:
-- >      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
-- >      break;
-- >    }
-- >}
-- >
-- >static void
-- >foo_bar_get_property (GObject         *object,
-- >                         guint            prop_id,
-- >                         GValue          *value,
-- >                         GParamSpec      *pspec)
-- >{
-- >  FooBar *bar = FOO_BAR (object);
-- >  FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
-- >
-- >  switch (prop_id)
-- >    {
-- >
-- >      ...
-- >
-- >    case PROP_ACTIVATABLE_RELATED_ACTION:
-- >      g_value_set_object (value, priv->action);
-- >      break;
-- >    case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
-- >      g_value_set_boolean (value, priv->use_action_appearance);
-- >      break;
-- >    default:
-- >      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
-- >      break;
-- >    }
-- >}
-- >
-- >
-- >static void
-- >foo_bar_set_use_action_appearance (FooBar   *bar,
-- >				   gboolean  use_appearance)
-- >{
-- >  FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
-- >
-- >  if (priv->use_action_appearance != use_appearance)
-- >    {
-- >      priv->use_action_appearance = use_appearance;
-- >      
-- >      gtk_activatable_sync_action_properties (GTK_ACTIVATABLE (bar), priv->action);
-- >    }
-- >}
-- >
-- >... call gtk_activatable_do_set_related_action() and then assign the action pointer,
-- >no need to reference the action here since gtk_activatable_do_set_related_action() already
-- >holds a reference here for you...
-- >static void
-- >foo_bar_set_related_action (FooBar    *bar,
-- >			    GtkAction *action)
-- >{
-- >  FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
-- >
-- >  if (priv->action == action)
-- >    return;
-- >
-- >  gtk_activatable_do_set_related_action (GTK_ACTIVATABLE (bar), action);
-- >
-- >  priv->action = action;
-- >}
-- >
-- >... Selectively reset and update activatable depending on the use-action-appearance property ...
-- >static void
-- >gtk_button_activatable_sync_action_properties (GtkActivatable       *activatable,
-- >		                                  GtkAction            *action)
-- >{
-- >  GtkButtonPrivate *priv = GTK_BUTTON_GET_PRIVATE (activatable);
-- >
-- >  if (!action)
-- >    return;
-- >
-- >  if (gtk_action_is_visible (action))
-- >    gtk_widget_show (GTK_WIDGET (activatable));
-- >  else
-- >    gtk_widget_hide (GTK_WIDGET (activatable));
-- >  
-- >  gtk_widget_set_sensitive (GTK_WIDGET (activatable), gtk_action_is_sensitive (action));
-- >
-- >  ...
-- >  
-- >  if (priv->use_action_appearance)
-- >    {
-- >      if (gtk_action_get_stock_id (action))
-- >	foo_bar_set_stock (button, gtk_action_get_stock_id (action));
-- >      else if (gtk_action_get_label (action))
-- >	foo_bar_set_label (button, gtk_action_get_label (action));
-- >
-- >      ...
-- >
-- >    }
-- >}
-- >
-- >static void
-- >foo_bar_activatable_update (GtkActivatable       *activatable,
-- >			       GtkAction            *action,
-- >			       const gchar          *property_name)
-- >{
-- >  FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (activatable);
-- >
-- >  if (strcmp (property_name, "visible") == 0)
-- >    {
-- >      if (gtk_action_is_visible (action))
-- >	gtk_widget_show (GTK_WIDGET (activatable));
-- >      else
-- >	gtk_widget_hide (GTK_WIDGET (activatable));
-- >    }
-- >  else if (strcmp (property_name, "sensitive") == 0)
-- >    gtk_widget_set_sensitive (GTK_WIDGET (activatable), gtk_action_is_sensitive (action));
-- >
-- >  ...
-- >
-- >  if (!priv->use_action_appearance)
-- >    return;
-- >
-- >  if (strcmp (property_name, "stock-id") == 0)
-- >    foo_bar_set_stock (button, gtk_action_get_stock_id (action));
-- >  else if (strcmp (property_name, "label") == 0)
-- >    foo_bar_set_label (button, gtk_action_get_label (action));
-- >
-- >  ...
-- >}
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.Activatable
    ( 

-- * Exported types
    Activatable(..)                         ,
    IsActivatable                           ,
    toActivatable                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance").

#if defined(ENABLE_OVERLOADING)
    ResolveActivatableMethod                ,
#endif

-- ** doSetRelatedAction #method:doSetRelatedAction#

#if defined(ENABLE_OVERLOADING)
    ActivatableDoSetRelatedActionMethodInfo ,
#endif
    activatableDoSetRelatedAction           ,


-- ** getRelatedAction #method:getRelatedAction#

#if defined(ENABLE_OVERLOADING)
    ActivatableGetRelatedActionMethodInfo   ,
#endif
    activatableGetRelatedAction             ,


-- ** getUseActionAppearance #method:getUseActionAppearance#

#if defined(ENABLE_OVERLOADING)
    ActivatableGetUseActionAppearanceMethodInfo,
#endif
    activatableGetUseActionAppearance       ,


-- ** setRelatedAction #method:setRelatedAction#

#if defined(ENABLE_OVERLOADING)
    ActivatableSetRelatedActionMethodInfo   ,
#endif
    activatableSetRelatedAction             ,


-- ** setUseActionAppearance #method:setUseActionAppearance#

#if defined(ENABLE_OVERLOADING)
    ActivatableSetUseActionAppearanceMethodInfo,
#endif
    activatableSetUseActionAppearance       ,


-- ** syncActionProperties #method:syncActionProperties#

#if defined(ENABLE_OVERLOADING)
    ActivatableSyncActionPropertiesMethodInfo,
#endif
    activatableSyncActionProperties         ,




 -- * Properties


-- ** relatedAction #attr:relatedAction#
-- | The action that this activatable will activate and receive
-- updates from for various states and possibly appearance.
-- 
-- > t'GI.Gtk.Interfaces.Activatable.Activatable' implementors need to handle the this property and
-- > call 'GI.Gtk.Interfaces.Activatable.activatableDoSetRelatedAction' when it changes.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    ActivatableRelatedActionPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    activatableRelatedAction                ,
#endif
    constructActivatableRelatedAction       ,
    getActivatableRelatedAction             ,
    setActivatableRelatedAction             ,


-- ** useActionAppearance #attr:useActionAppearance#
-- | Whether this activatable should reset its layout
-- and appearance when setting the related action or when
-- the action changes appearance.
-- 
-- See the t'GI.Gtk.Objects.Action.Action' documentation directly to find which properties
-- should be ignored by the t'GI.Gtk.Interfaces.Activatable.Activatable' when this property is 'P.False'.
-- 
-- > t'GI.Gtk.Interfaces.Activatable.Activatable' implementors need to handle this property
-- > and call 'GI.Gtk.Interfaces.Activatable.activatableSyncActionProperties' on the activatable
-- > widget when it changes.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    ActivatableUseActionAppearancePropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    activatableUseActionAppearance          ,
#endif
    constructActivatableUseActionAppearance ,
    getActivatableUseActionAppearance       ,
    setActivatableUseActionAppearance       ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Action as Gtk.Action

-- interface Activatable 
-- | Memory-managed wrapper type.
newtype Activatable = Activatable (SP.ManagedPtr Activatable)
    deriving (Eq)

instance SP.ManagedPtrNewtype Activatable where
    toManagedPtr (Activatable p) = p

foreign import ccall "gtk_activatable_get_type"
    c_gtk_activatable_get_type :: IO B.Types.GType

instance B.Types.TypedObject Activatable where
    glibType = c_gtk_activatable_get_type

instance B.Types.GObject Activatable

-- | Type class for types which can be safely cast to `Activatable`, for instance with `toActivatable`.
class (SP.GObject o, O.IsDescendantOf Activatable o) => IsActivatable o
instance (SP.GObject o, O.IsDescendantOf Activatable o) => IsActivatable o

instance O.HasParentTypes Activatable
type instance O.ParentTypes Activatable = '[GObject.Object.Object]

-- | Cast to `Activatable`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toActivatable :: (MIO.MonadIO m, IsActivatable o) => o -> m Activatable
toActivatable = MIO.liftIO . B.ManagedPtr.unsafeCastTo Activatable

-- | Convert 'Activatable' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Activatable) where
    gvalueGType_ = c_gtk_activatable_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Activatable)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Activatable)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Activatable ptr
        else return P.Nothing
        
    

-- VVV Prop "related-action"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Action"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@related-action@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' activatable #relatedAction
-- @
getActivatableRelatedAction :: (MonadIO m, IsActivatable o) => o -> m Gtk.Action.Action
getActivatableRelatedAction obj = MIO.liftIO $ checkUnexpectedNothing "getActivatableRelatedAction" $ B.Properties.getObjectPropertyObject obj "related-action" Gtk.Action.Action

-- | Set the value of the “@related-action@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' activatable [ #relatedAction 'Data.GI.Base.Attributes.:=' value ]
-- @
setActivatableRelatedAction :: (MonadIO m, IsActivatable o, Gtk.Action.IsAction a) => o -> a -> m ()
setActivatableRelatedAction obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "related-action" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@related-action@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructActivatableRelatedAction :: (IsActivatable o, MIO.MonadIO m, Gtk.Action.IsAction a) => a -> m (GValueConstruct o)
constructActivatableRelatedAction val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "related-action" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ActivatableRelatedActionPropertyInfo
instance AttrInfo ActivatableRelatedActionPropertyInfo where
    type AttrAllowedOps ActivatableRelatedActionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ActivatableRelatedActionPropertyInfo = IsActivatable
    type AttrSetTypeConstraint ActivatableRelatedActionPropertyInfo = Gtk.Action.IsAction
    type AttrTransferTypeConstraint ActivatableRelatedActionPropertyInfo = Gtk.Action.IsAction
    type AttrTransferType ActivatableRelatedActionPropertyInfo = Gtk.Action.Action
    type AttrGetType ActivatableRelatedActionPropertyInfo = Gtk.Action.Action
    type AttrLabel ActivatableRelatedActionPropertyInfo = "related-action"
    type AttrOrigin ActivatableRelatedActionPropertyInfo = Activatable
    attrGet = getActivatableRelatedAction
    attrSet = setActivatableRelatedAction
    attrTransfer _ v = do
        unsafeCastTo Gtk.Action.Action v
    attrConstruct = constructActivatableRelatedAction
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Activatable.relatedAction"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Activatable.html#g:attr:relatedAction"
        })
#endif

-- VVV Prop "use-action-appearance"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-action-appearance@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' activatable #useActionAppearance
-- @
getActivatableUseActionAppearance :: (MonadIO m, IsActivatable o) => o -> m Bool
getActivatableUseActionAppearance obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-action-appearance"

-- | Set the value of the “@use-action-appearance@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' activatable [ #useActionAppearance 'Data.GI.Base.Attributes.:=' value ]
-- @
setActivatableUseActionAppearance :: (MonadIO m, IsActivatable o) => o -> Bool -> m ()
setActivatableUseActionAppearance obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-action-appearance" val

-- | Construct a `GValueConstruct` with valid value for the “@use-action-appearance@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructActivatableUseActionAppearance :: (IsActivatable o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructActivatableUseActionAppearance val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-action-appearance" val

#if defined(ENABLE_OVERLOADING)
data ActivatableUseActionAppearancePropertyInfo
instance AttrInfo ActivatableUseActionAppearancePropertyInfo where
    type AttrAllowedOps ActivatableUseActionAppearancePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ActivatableUseActionAppearancePropertyInfo = IsActivatable
    type AttrSetTypeConstraint ActivatableUseActionAppearancePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ActivatableUseActionAppearancePropertyInfo = (~) Bool
    type AttrTransferType ActivatableUseActionAppearancePropertyInfo = Bool
    type AttrGetType ActivatableUseActionAppearancePropertyInfo = Bool
    type AttrLabel ActivatableUseActionAppearancePropertyInfo = "use-action-appearance"
    type AttrOrigin ActivatableUseActionAppearancePropertyInfo = Activatable
    attrGet = getActivatableUseActionAppearance
    attrSet = setActivatableUseActionAppearance
    attrTransfer _ v = do
        return v
    attrConstruct = constructActivatableUseActionAppearance
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Activatable.useActionAppearance"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Activatable.html#g:attr:useActionAppearance"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Activatable
type instance O.AttributeList Activatable = ActivatableAttributeList
type ActivatableAttributeList = ('[ '("relatedAction", ActivatableRelatedActionPropertyInfo), '("useActionAppearance", ActivatableUseActionAppearancePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
activatableRelatedAction :: AttrLabelProxy "relatedAction"
activatableRelatedAction = AttrLabelProxy

activatableUseActionAppearance :: AttrLabelProxy "useActionAppearance"
activatableUseActionAppearance = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveActivatableMethod (t :: Symbol) (o :: *) :: * where
    ResolveActivatableMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveActivatableMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveActivatableMethod "doSetRelatedAction" o = ActivatableDoSetRelatedActionMethodInfo
    ResolveActivatableMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveActivatableMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveActivatableMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveActivatableMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveActivatableMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveActivatableMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveActivatableMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveActivatableMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveActivatableMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveActivatableMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveActivatableMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveActivatableMethod "syncActionProperties" o = ActivatableSyncActionPropertiesMethodInfo
    ResolveActivatableMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveActivatableMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveActivatableMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveActivatableMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveActivatableMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveActivatableMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveActivatableMethod "getRelatedAction" o = ActivatableGetRelatedActionMethodInfo
    ResolveActivatableMethod "getUseActionAppearance" o = ActivatableGetUseActionAppearanceMethodInfo
    ResolveActivatableMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveActivatableMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveActivatableMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveActivatableMethod "setRelatedAction" o = ActivatableSetRelatedActionMethodInfo
    ResolveActivatableMethod "setUseActionAppearance" o = ActivatableSetUseActionAppearanceMethodInfo
    ResolveActivatableMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveActivatableMethod t Activatable, O.OverloadedMethod info Activatable p) => OL.IsLabel t (Activatable -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveActivatableMethod t Activatable, O.OverloadedMethod info Activatable p, R.HasField t Activatable p) => R.HasField t Activatable p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveActivatableMethod t Activatable, O.OverloadedMethodInfo info Activatable) => OL.IsLabel t (O.MethodProxy info Activatable) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method Activatable::do_set_related_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "activatable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Activatable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActivatable" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkAction to set"
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

foreign import ccall "gtk_activatable_do_set_related_action" gtk_activatable_do_set_related_action :: 
    Ptr Activatable ->                      -- activatable : TInterface (Name {namespace = "Gtk", name = "Activatable"})
    Ptr Gtk.Action.Action ->                -- action : TInterface (Name {namespace = "Gtk", name = "Action"})
    IO ()

{-# DEPRECATED activatableDoSetRelatedAction ["(Since version 3.10)"] #-}
-- | This is a utility function for t'GI.Gtk.Interfaces.Activatable.Activatable' implementors.
-- 
-- When implementing t'GI.Gtk.Interfaces.Activatable.Activatable' you must call this when
-- handling changes of the t'GI.Gtk.Interfaces.Activatable.Activatable':@/related-action/@, and
-- you must also use this to break references in t'GI.GObject.Objects.Object.Object'->@/dispose()/@.
-- 
-- This function adds a reference to the currently set related
-- action for you, it also makes sure the t'GI.Gtk.Interfaces.Activatable.Activatable'->@/update()/@
-- method is called when the related t'GI.Gtk.Objects.Action.Action' properties change
-- and registers to the action’s proxy list.
-- 
-- > Be careful to call this before setting the local
-- > copy of the t'GI.Gtk.Objects.Action.Action' property, since this function uses
-- > 'GI.Gtk.Interfaces.Activatable.activatableGetRelatedAction' to retrieve the
-- > previous action.
-- 
-- /Since: 2.16/
activatableDoSetRelatedAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsActivatable a, Gtk.Action.IsAction b) =>
    a
    -- ^ /@activatable@/: a t'GI.Gtk.Interfaces.Activatable.Activatable'
    -> b
    -- ^ /@action@/: the t'GI.Gtk.Objects.Action.Action' to set
    -> m ()
activatableDoSetRelatedAction activatable action = liftIO $ do
    activatable' <- unsafeManagedPtrCastPtr activatable
    action' <- unsafeManagedPtrCastPtr action
    gtk_activatable_do_set_related_action activatable' action'
    touchManagedPtr activatable
    touchManagedPtr action
    return ()

#if defined(ENABLE_OVERLOADING)
data ActivatableDoSetRelatedActionMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsActivatable a, Gtk.Action.IsAction b) => O.OverloadedMethod ActivatableDoSetRelatedActionMethodInfo a signature where
    overloadedMethod = activatableDoSetRelatedAction

instance O.OverloadedMethodInfo ActivatableDoSetRelatedActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Activatable.activatableDoSetRelatedAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Activatable.html#v:activatableDoSetRelatedAction"
        })


#endif

-- method Activatable::get_related_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "activatable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Activatable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActivatable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_activatable_get_related_action" gtk_activatable_get_related_action :: 
    Ptr Activatable ->                      -- activatable : TInterface (Name {namespace = "Gtk", name = "Activatable"})
    IO (Ptr Gtk.Action.Action)

{-# DEPRECATED activatableGetRelatedAction ["(Since version 3.10)"] #-}
-- | Gets the related t'GI.Gtk.Objects.Action.Action' for /@activatable@/.
-- 
-- /Since: 2.16/
activatableGetRelatedAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsActivatable a) =>
    a
    -- ^ /@activatable@/: a t'GI.Gtk.Interfaces.Activatable.Activatable'
    -> m Gtk.Action.Action
    -- ^ __Returns:__ the related t'GI.Gtk.Objects.Action.Action' if one is set.
activatableGetRelatedAction activatable = liftIO $ do
    activatable' <- unsafeManagedPtrCastPtr activatable
    result <- gtk_activatable_get_related_action activatable'
    checkUnexpectedReturnNULL "activatableGetRelatedAction" result
    result' <- (newObject Gtk.Action.Action) result
    touchManagedPtr activatable
    return result'

#if defined(ENABLE_OVERLOADING)
data ActivatableGetRelatedActionMethodInfo
instance (signature ~ (m Gtk.Action.Action), MonadIO m, IsActivatable a) => O.OverloadedMethod ActivatableGetRelatedActionMethodInfo a signature where
    overloadedMethod = activatableGetRelatedAction

instance O.OverloadedMethodInfo ActivatableGetRelatedActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Activatable.activatableGetRelatedAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Activatable.html#v:activatableGetRelatedAction"
        })


#endif

-- method Activatable::get_use_action_appearance
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "activatable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Activatable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActivatable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_activatable_get_use_action_appearance" gtk_activatable_get_use_action_appearance :: 
    Ptr Activatable ->                      -- activatable : TInterface (Name {namespace = "Gtk", name = "Activatable"})
    IO CInt

{-# DEPRECATED activatableGetUseActionAppearance ["(Since version 3.10)"] #-}
-- | Gets whether this activatable should reset its layout
-- and appearance when setting the related action or when
-- the action changes appearance.
-- 
-- /Since: 2.16/
activatableGetUseActionAppearance ::
    (B.CallStack.HasCallStack, MonadIO m, IsActivatable a) =>
    a
    -- ^ /@activatable@/: a t'GI.Gtk.Interfaces.Activatable.Activatable'
    -> m Bool
    -- ^ __Returns:__ whether /@activatable@/ uses its actions appearance.
activatableGetUseActionAppearance activatable = liftIO $ do
    activatable' <- unsafeManagedPtrCastPtr activatable
    result <- gtk_activatable_get_use_action_appearance activatable'
    let result' = (/= 0) result
    touchManagedPtr activatable
    return result'

#if defined(ENABLE_OVERLOADING)
data ActivatableGetUseActionAppearanceMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsActivatable a) => O.OverloadedMethod ActivatableGetUseActionAppearanceMethodInfo a signature where
    overloadedMethod = activatableGetUseActionAppearance

instance O.OverloadedMethodInfo ActivatableGetUseActionAppearanceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Activatable.activatableGetUseActionAppearance",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Activatable.html#v:activatableGetUseActionAppearance"
        })


#endif

-- method Activatable::set_related_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "activatable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Activatable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActivatable" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkAction to set"
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

foreign import ccall "gtk_activatable_set_related_action" gtk_activatable_set_related_action :: 
    Ptr Activatable ->                      -- activatable : TInterface (Name {namespace = "Gtk", name = "Activatable"})
    Ptr Gtk.Action.Action ->                -- action : TInterface (Name {namespace = "Gtk", name = "Action"})
    IO ()

{-# DEPRECATED activatableSetRelatedAction ["(Since version 3.10)"] #-}
-- | Sets the related action on the /@activatable@/ object.
-- 
-- > t'GI.Gtk.Interfaces.Activatable.Activatable' implementors need to handle the t'GI.Gtk.Interfaces.Activatable.Activatable':@/related-action/@
-- > property and call 'GI.Gtk.Interfaces.Activatable.activatableDoSetRelatedAction' when it changes.
-- 
-- /Since: 2.16/
activatableSetRelatedAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsActivatable a, Gtk.Action.IsAction b) =>
    a
    -- ^ /@activatable@/: a t'GI.Gtk.Interfaces.Activatable.Activatable'
    -> b
    -- ^ /@action@/: the t'GI.Gtk.Objects.Action.Action' to set
    -> m ()
activatableSetRelatedAction activatable action = liftIO $ do
    activatable' <- unsafeManagedPtrCastPtr activatable
    action' <- unsafeManagedPtrCastPtr action
    gtk_activatable_set_related_action activatable' action'
    touchManagedPtr activatable
    touchManagedPtr action
    return ()

#if defined(ENABLE_OVERLOADING)
data ActivatableSetRelatedActionMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsActivatable a, Gtk.Action.IsAction b) => O.OverloadedMethod ActivatableSetRelatedActionMethodInfo a signature where
    overloadedMethod = activatableSetRelatedAction

instance O.OverloadedMethodInfo ActivatableSetRelatedActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Activatable.activatableSetRelatedAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Activatable.html#v:activatableSetRelatedAction"
        })


#endif

-- method Activatable::set_use_action_appearance
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "activatable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Activatable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActivatable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_appearance"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to use the actions appearance"
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

foreign import ccall "gtk_activatable_set_use_action_appearance" gtk_activatable_set_use_action_appearance :: 
    Ptr Activatable ->                      -- activatable : TInterface (Name {namespace = "Gtk", name = "Activatable"})
    CInt ->                                 -- use_appearance : TBasicType TBoolean
    IO ()

{-# DEPRECATED activatableSetUseActionAppearance ["(Since version 3.10)"] #-}
-- | Sets whether this activatable should reset its layout and appearance
-- when setting the related action or when the action changes appearance
-- 
-- > t'GI.Gtk.Interfaces.Activatable.Activatable' implementors need to handle the
-- > t'GI.Gtk.Interfaces.Activatable.Activatable':@/use-action-appearance/@ property and call
-- > 'GI.Gtk.Interfaces.Activatable.activatableSyncActionProperties' to update /@activatable@/
-- > if needed.
-- 
-- /Since: 2.16/
activatableSetUseActionAppearance ::
    (B.CallStack.HasCallStack, MonadIO m, IsActivatable a) =>
    a
    -- ^ /@activatable@/: a t'GI.Gtk.Interfaces.Activatable.Activatable'
    -> Bool
    -- ^ /@useAppearance@/: whether to use the actions appearance
    -> m ()
activatableSetUseActionAppearance activatable useAppearance = liftIO $ do
    activatable' <- unsafeManagedPtrCastPtr activatable
    let useAppearance' = (fromIntegral . fromEnum) useAppearance
    gtk_activatable_set_use_action_appearance activatable' useAppearance'
    touchManagedPtr activatable
    return ()

#if defined(ENABLE_OVERLOADING)
data ActivatableSetUseActionAppearanceMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsActivatable a) => O.OverloadedMethod ActivatableSetUseActionAppearanceMethodInfo a signature where
    overloadedMethod = activatableSetUseActionAppearance

instance O.OverloadedMethodInfo ActivatableSetUseActionAppearanceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Activatable.activatableSetUseActionAppearance",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Activatable.html#v:activatableSetUseActionAppearance"
        })


#endif

-- method Activatable::sync_action_properties
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "activatable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Activatable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActivatable" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the related #GtkAction or %NULL"
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

foreign import ccall "gtk_activatable_sync_action_properties" gtk_activatable_sync_action_properties :: 
    Ptr Activatable ->                      -- activatable : TInterface (Name {namespace = "Gtk", name = "Activatable"})
    Ptr Gtk.Action.Action ->                -- action : TInterface (Name {namespace = "Gtk", name = "Action"})
    IO ()

{-# DEPRECATED activatableSyncActionProperties ["(Since version 3.10)"] #-}
-- | This is called to update the activatable completely, this is called
-- internally when the t'GI.Gtk.Interfaces.Activatable.Activatable':@/related-action/@ property is set
-- or unset and by the implementing class when
-- t'GI.Gtk.Interfaces.Activatable.Activatable':@/use-action-appearance/@ changes.
-- 
-- /Since: 2.16/
activatableSyncActionProperties ::
    (B.CallStack.HasCallStack, MonadIO m, IsActivatable a, Gtk.Action.IsAction b) =>
    a
    -- ^ /@activatable@/: a t'GI.Gtk.Interfaces.Activatable.Activatable'
    -> Maybe (b)
    -- ^ /@action@/: the related t'GI.Gtk.Objects.Action.Action' or 'P.Nothing'
    -> m ()
activatableSyncActionProperties activatable action = liftIO $ do
    activatable' <- unsafeManagedPtrCastPtr activatable
    maybeAction <- case action of
        Nothing -> return nullPtr
        Just jAction -> do
            jAction' <- unsafeManagedPtrCastPtr jAction
            return jAction'
    gtk_activatable_sync_action_properties activatable' maybeAction
    touchManagedPtr activatable
    whenJust action touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ActivatableSyncActionPropertiesMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsActivatable a, Gtk.Action.IsAction b) => O.OverloadedMethod ActivatableSyncActionPropertiesMethodInfo a signature where
    overloadedMethod = activatableSyncActionProperties

instance O.OverloadedMethodInfo ActivatableSyncActionPropertiesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Activatable.activatableSyncActionProperties",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Activatable.html#v:activatableSyncActionProperties"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Activatable = ActivatableSignalList
type ActivatableSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif


