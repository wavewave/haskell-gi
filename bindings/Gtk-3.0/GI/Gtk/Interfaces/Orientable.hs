{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Interfaces.Orientable.Orientable' interface is implemented by all widgets that can be
-- oriented horizontally or vertically. Historically, such widgets have been
-- realized as subclasses of a common base class (e.g t'GI.Gtk.Objects.Box.Box'\/t'GI.Gtk.Objects.HBox.HBox'\/t'GI.Gtk.Objects.VBox.VBox'
-- or t'GI.Gtk.Objects.Scale.Scale'\/t'GI.Gtk.Objects.HScale.HScale'\/t'GI.Gtk.Objects.VScale.VScale'). t'GI.Gtk.Interfaces.Orientable.Orientable' is more flexible in that
-- it allows the orientation to be changed at runtime, allowing the widgets
-- to “flip”.
-- 
-- t'GI.Gtk.Interfaces.Orientable.Orientable' was introduced in GTK+ 2.16.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.Orientable
    ( 

-- * Exported types
    Orientable(..)                          ,
    IsOrientable                            ,
    toOrientable                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveOrientableMethod                 ,
#endif

-- ** getOrientation #method:getOrientation#

#if defined(ENABLE_OVERLOADING)
    OrientableGetOrientationMethodInfo      ,
#endif
    orientableGetOrientation                ,


-- ** setOrientation #method:setOrientation#

#if defined(ENABLE_OVERLOADING)
    OrientableSetOrientationMethodInfo      ,
#endif
    orientableSetOrientation                ,




 -- * Properties


-- ** orientation #attr:orientation#
-- | The orientation of the orientable.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    OrientableOrientationPropertyInfo       ,
#endif
    constructOrientableOrientation          ,
    getOrientableOrientation                ,
#if defined(ENABLE_OVERLOADING)
    orientableOrientation                   ,
#endif
    setOrientableOrientation                ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums

-- interface Orientable 
-- | Memory-managed wrapper type.
newtype Orientable = Orientable (SP.ManagedPtr Orientable)
    deriving (Eq)

instance SP.ManagedPtrNewtype Orientable where
    toManagedPtr (Orientable p) = p

foreign import ccall "gtk_orientable_get_type"
    c_gtk_orientable_get_type :: IO B.Types.GType

instance B.Types.TypedObject Orientable where
    glibType = c_gtk_orientable_get_type

instance B.Types.GObject Orientable

-- | Type class for types which can be safely cast to `Orientable`, for instance with `toOrientable`.
class (SP.GObject o, O.IsDescendantOf Orientable o) => IsOrientable o
instance (SP.GObject o, O.IsDescendantOf Orientable o) => IsOrientable o

instance O.HasParentTypes Orientable
type instance O.ParentTypes Orientable = '[GObject.Object.Object]

-- | Cast to `Orientable`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toOrientable :: (MIO.MonadIO m, IsOrientable o) => o -> m Orientable
toOrientable = MIO.liftIO . B.ManagedPtr.unsafeCastTo Orientable

-- | Convert 'Orientable' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Orientable) where
    gvalueGType_ = c_gtk_orientable_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Orientable)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Orientable)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Orientable ptr
        else return P.Nothing
        
    

-- VVV Prop "orientation"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Orientation"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@orientation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' orientable #orientation
-- @
getOrientableOrientation :: (MonadIO m, IsOrientable o) => o -> m Gtk.Enums.Orientation
getOrientableOrientation obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "orientation"

-- | Set the value of the “@orientation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' orientable [ #orientation 'Data.GI.Base.Attributes.:=' value ]
-- @
setOrientableOrientation :: (MonadIO m, IsOrientable o) => o -> Gtk.Enums.Orientation -> m ()
setOrientableOrientation obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "orientation" val

-- | Construct a `GValueConstruct` with valid value for the “@orientation@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructOrientableOrientation :: (IsOrientable o, MIO.MonadIO m) => Gtk.Enums.Orientation -> m (GValueConstruct o)
constructOrientableOrientation val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "orientation" val

#if defined(ENABLE_OVERLOADING)
data OrientableOrientationPropertyInfo
instance AttrInfo OrientableOrientationPropertyInfo where
    type AttrAllowedOps OrientableOrientationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint OrientableOrientationPropertyInfo = IsOrientable
    type AttrSetTypeConstraint OrientableOrientationPropertyInfo = (~) Gtk.Enums.Orientation
    type AttrTransferTypeConstraint OrientableOrientationPropertyInfo = (~) Gtk.Enums.Orientation
    type AttrTransferType OrientableOrientationPropertyInfo = Gtk.Enums.Orientation
    type AttrGetType OrientableOrientationPropertyInfo = Gtk.Enums.Orientation
    type AttrLabel OrientableOrientationPropertyInfo = "orientation"
    type AttrOrigin OrientableOrientationPropertyInfo = Orientable
    attrGet = getOrientableOrientation
    attrSet = setOrientableOrientation
    attrTransfer _ v = do
        return v
    attrConstruct = constructOrientableOrientation
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Orientable.orientation"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Orientable.html#g:attr:orientation"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Orientable
type instance O.AttributeList Orientable = OrientableAttributeList
type OrientableAttributeList = ('[ '("orientation", OrientableOrientationPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
orientableOrientation :: AttrLabelProxy "orientation"
orientableOrientation = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveOrientableMethod (t :: Symbol) (o :: *) :: * where
    ResolveOrientableMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveOrientableMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveOrientableMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveOrientableMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveOrientableMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveOrientableMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveOrientableMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveOrientableMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveOrientableMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveOrientableMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveOrientableMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveOrientableMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveOrientableMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveOrientableMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveOrientableMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveOrientableMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveOrientableMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveOrientableMethod "getOrientation" o = OrientableGetOrientationMethodInfo
    ResolveOrientableMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveOrientableMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveOrientableMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveOrientableMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveOrientableMethod "setOrientation" o = OrientableSetOrientationMethodInfo
    ResolveOrientableMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveOrientableMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveOrientableMethod t Orientable, O.OverloadedMethod info Orientable p) => OL.IsLabel t (Orientable -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveOrientableMethod t Orientable, O.OverloadedMethod info Orientable p, R.HasField t Orientable p) => R.HasField t Orientable p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveOrientableMethod t Orientable, O.OverloadedMethodInfo info Orientable) => OL.IsLabel t (O.MethodProxy info Orientable) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method Orientable::get_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "orientable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkOrientable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Orientation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_orientable_get_orientation" gtk_orientable_get_orientation :: 
    Ptr Orientable ->                       -- orientable : TInterface (Name {namespace = "Gtk", name = "Orientable"})
    IO CUInt

-- | Retrieves the orientation of the /@orientable@/.
-- 
-- /Since: 2.16/
orientableGetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsOrientable a) =>
    a
    -- ^ /@orientable@/: a t'GI.Gtk.Interfaces.Orientable.Orientable'
    -> m Gtk.Enums.Orientation
    -- ^ __Returns:__ the orientation of the /@orientable@/.
orientableGetOrientation orientable = liftIO $ do
    orientable' <- unsafeManagedPtrCastPtr orientable
    result <- gtk_orientable_get_orientation orientable'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr orientable
    return result'

#if defined(ENABLE_OVERLOADING)
data OrientableGetOrientationMethodInfo
instance (signature ~ (m Gtk.Enums.Orientation), MonadIO m, IsOrientable a) => O.OverloadedMethod OrientableGetOrientationMethodInfo a signature where
    overloadedMethod = orientableGetOrientation

instance O.OverloadedMethodInfo OrientableGetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Orientable.orientableGetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Orientable.html#v:orientableGetOrientation"
        })


#endif

-- method Orientable::set_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "orientable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkOrientable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the orientable\8217s new orientation."
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

foreign import ccall "gtk_orientable_set_orientation" gtk_orientable_set_orientation :: 
    Ptr Orientable ->                       -- orientable : TInterface (Name {namespace = "Gtk", name = "Orientable"})
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    IO ()

-- | Sets the orientation of the /@orientable@/.
-- 
-- /Since: 2.16/
orientableSetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsOrientable a) =>
    a
    -- ^ /@orientable@/: a t'GI.Gtk.Interfaces.Orientable.Orientable'
    -> Gtk.Enums.Orientation
    -- ^ /@orientation@/: the orientable’s new orientation.
    -> m ()
orientableSetOrientation orientable orientation = liftIO $ do
    orientable' <- unsafeManagedPtrCastPtr orientable
    let orientation' = (fromIntegral . fromEnum) orientation
    gtk_orientable_set_orientation orientable' orientation'
    touchManagedPtr orientable
    return ()

#if defined(ENABLE_OVERLOADING)
data OrientableSetOrientationMethodInfo
instance (signature ~ (Gtk.Enums.Orientation -> m ()), MonadIO m, IsOrientable a) => O.OverloadedMethod OrientableSetOrientationMethodInfo a signature where
    overloadedMethod = orientableSetOrientation

instance O.OverloadedMethodInfo OrientableSetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Orientable.orientableSetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Orientable.html#v:orientableSetOrientation"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Orientable = OrientableSignalList
type OrientableSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif


