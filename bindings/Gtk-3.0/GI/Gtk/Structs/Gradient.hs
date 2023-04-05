{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkGradient is a boxed type that represents a gradient.
-- It is the result of parsing a
-- [gradient expression][gtkcssprovider-gradients].
-- To obtain the gradient represented by a GtkGradient, it has to
-- be resolved with 'GI.Gtk.Structs.Gradient.gradientResolve', which replaces all
-- symbolic color references by the colors they refer to (in a given
-- context) and constructs a t'GI.Cairo.Structs.Pattern.Pattern' value.
-- 
-- It is not normally necessary to deal directly with @/GtkGradients/@,
-- since they are mostly used behind the scenes by t'GI.Gtk.Objects.StyleContext.StyleContext' and
-- t'GI.Gtk.Objects.CssProvider.CssProvider'.
-- 
-- t'GI.Gtk.Structs.Gradient.Gradient' is deprecated. It was used internally by GTK’s CSS engine
-- to represent gradients. As its handling is not conforming to modern
-- web standards, it is not used anymore. If you want to use gradients in
-- your own code, please use Cairo directly.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.Gradient
    ( 

-- * Exported types
    Gradient(..)                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addColorStop]("GI.Gtk.Structs.Gradient#g:method:addColorStop"), [ref]("GI.Gtk.Structs.Gradient#g:method:ref"), [resolve]("GI.Gtk.Structs.Gradient#g:method:resolve"), [resolveForContext]("GI.Gtk.Structs.Gradient#g:method:resolveForContext"), [toString]("GI.Gtk.Structs.Gradient#g:method:toString"), [unref]("GI.Gtk.Structs.Gradient#g:method:unref").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveGradientMethod                   ,
#endif

-- ** addColorStop #method:addColorStop#

#if defined(ENABLE_OVERLOADING)
    GradientAddColorStopMethodInfo          ,
#endif
    gradientAddColorStop                    ,


-- ** newLinear #method:newLinear#

    gradientNewLinear                       ,


-- ** newRadial #method:newRadial#

    gradientNewRadial                       ,


-- ** ref #method:ref#

#if defined(ENABLE_OVERLOADING)
    GradientRefMethodInfo                   ,
#endif
    gradientRef                             ,


-- ** resolve #method:resolve#

#if defined(ENABLE_OVERLOADING)
    GradientResolveMethodInfo               ,
#endif
    gradientResolve                         ,


-- ** resolveForContext #method:resolveForContext#

#if defined(ENABLE_OVERLOADING)
    GradientResolveForContextMethodInfo     ,
#endif
    gradientResolveForContext               ,


-- ** toString #method:toString#

#if defined(ENABLE_OVERLOADING)
    GradientToStringMethodInfo              ,
#endif
    gradientToString                        ,


-- ** unref #method:unref#

#if defined(ENABLE_OVERLOADING)
    GradientUnrefMethodInfo                 ,
#endif
    gradientUnref                           ,




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

import qualified GI.Cairo.Structs.Pattern as Cairo.Pattern
import {-# SOURCE #-} qualified GI.Gtk.Objects.StyleContext as Gtk.StyleContext
import {-# SOURCE #-} qualified GI.Gtk.Objects.StyleProperties as Gtk.StyleProperties
import {-# SOURCE #-} qualified GI.Gtk.Structs.SymbolicColor as Gtk.SymbolicColor

-- | Memory-managed wrapper type.
newtype Gradient = Gradient (SP.ManagedPtr Gradient)
    deriving (Eq)

instance SP.ManagedPtrNewtype Gradient where
    toManagedPtr (Gradient p) = p

foreign import ccall "gtk_gradient_get_type" c_gtk_gradient_get_type :: 
    IO GType

type instance O.ParentTypes Gradient = '[]
instance O.HasParentTypes Gradient

instance B.Types.TypedObject Gradient where
    glibType = c_gtk_gradient_get_type

instance B.Types.GBoxed Gradient

-- | Convert 'Gradient' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Gradient) where
    gvalueGType_ = c_gtk_gradient_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr Gradient)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr Gradient)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed Gradient ptr
        else return P.Nothing
        
    


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Gradient
type instance O.AttributeList Gradient = GradientAttributeList
type GradientAttributeList = ('[ ] :: [(Symbol, *)])
#endif

-- method Gradient::new_linear
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "x0"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate of the starting point"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y0"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate of the starting point"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x1"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate of the end point"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y1"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate of the end point"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Gradient" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gradient_new_linear" gtk_gradient_new_linear :: 
    CDouble ->                              -- x0 : TBasicType TDouble
    CDouble ->                              -- y0 : TBasicType TDouble
    CDouble ->                              -- x1 : TBasicType TDouble
    CDouble ->                              -- y1 : TBasicType TDouble
    IO (Ptr Gradient)

{-# DEPRECATED gradientNewLinear ["(Since version 3.8)","t'GI.Gtk.Structs.Gradient.Gradient' is deprecated."] #-}
-- | Creates a new linear gradient along the line defined by (x0, y0) and (x1, y1). Before using the gradient
-- a number of stop colors must be added through 'GI.Gtk.Structs.Gradient.gradientAddColorStop'.
-- 
-- /Since: 3.0/
gradientNewLinear ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Double
    -- ^ /@x0@/: X coordinate of the starting point
    -> Double
    -- ^ /@y0@/: Y coordinate of the starting point
    -> Double
    -- ^ /@x1@/: X coordinate of the end point
    -> Double
    -- ^ /@y1@/: Y coordinate of the end point
    -> m Gradient
    -- ^ __Returns:__ A newly created t'GI.Gtk.Structs.Gradient.Gradient'
gradientNewLinear x0 y0 x1 y1 = liftIO $ do
    let x0' = realToFrac x0
    let y0' = realToFrac y0
    let x1' = realToFrac x1
    let y1' = realToFrac y1
    result <- gtk_gradient_new_linear x0' y0' x1' y1'
    checkUnexpectedReturnNULL "gradientNewLinear" result
    result' <- (wrapBoxed Gradient) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Gradient::new_radial
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "x0"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate of the start circle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y0"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate of the start circle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "radius0"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "radius of the start circle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x1"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate of the end circle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y1"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate of the end circle"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "radius1"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "radius of the end circle"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Gradient" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gradient_new_radial" gtk_gradient_new_radial :: 
    CDouble ->                              -- x0 : TBasicType TDouble
    CDouble ->                              -- y0 : TBasicType TDouble
    CDouble ->                              -- radius0 : TBasicType TDouble
    CDouble ->                              -- x1 : TBasicType TDouble
    CDouble ->                              -- y1 : TBasicType TDouble
    CDouble ->                              -- radius1 : TBasicType TDouble
    IO (Ptr Gradient)

{-# DEPRECATED gradientNewRadial ["(Since version 3.8)","t'GI.Gtk.Structs.Gradient.Gradient' is deprecated."] #-}
-- | Creates a new radial gradient along the two circles defined by (x0, y0, radius0) and
-- (x1, y1, radius1). Before using the gradient a number of stop colors must be added
-- through 'GI.Gtk.Structs.Gradient.gradientAddColorStop'.
-- 
-- /Since: 3.0/
gradientNewRadial ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Double
    -- ^ /@x0@/: X coordinate of the start circle
    -> Double
    -- ^ /@y0@/: Y coordinate of the start circle
    -> Double
    -- ^ /@radius0@/: radius of the start circle
    -> Double
    -- ^ /@x1@/: X coordinate of the end circle
    -> Double
    -- ^ /@y1@/: Y coordinate of the end circle
    -> Double
    -- ^ /@radius1@/: radius of the end circle
    -> m Gradient
    -- ^ __Returns:__ A newly created t'GI.Gtk.Structs.Gradient.Gradient'
gradientNewRadial x0 y0 radius0 x1 y1 radius1 = liftIO $ do
    let x0' = realToFrac x0
    let y0' = realToFrac y0
    let radius0' = realToFrac radius0
    let x1' = realToFrac x1
    let y1' = realToFrac y1
    let radius1' = realToFrac radius1
    result <- gtk_gradient_new_radial x0' y0' radius0' x1' y1' radius1'
    checkUnexpectedReturnNULL "gradientNewRadial" result
    result' <- (wrapBoxed Gradient) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Gradient::add_color_stop
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gradient"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gradient" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGradient" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "offset"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "offset for the color stop"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SymbolicColor" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "color to use" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gradient_add_color_stop" gtk_gradient_add_color_stop :: 
    Ptr Gradient ->                         -- gradient : TInterface (Name {namespace = "Gtk", name = "Gradient"})
    CDouble ->                              -- offset : TBasicType TDouble
    Ptr Gtk.SymbolicColor.SymbolicColor ->  -- color : TInterface (Name {namespace = "Gtk", name = "SymbolicColor"})
    IO ()

{-# DEPRECATED gradientAddColorStop ["(Since version 3.8)","t'GI.Gtk.Structs.Gradient.Gradient' is deprecated."] #-}
-- | Adds a stop color to /@gradient@/.
-- 
-- /Since: 3.0/
gradientAddColorStop ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gradient
    -- ^ /@gradient@/: a t'GI.Gtk.Structs.Gradient.Gradient'
    -> Double
    -- ^ /@offset@/: offset for the color stop
    -> Gtk.SymbolicColor.SymbolicColor
    -- ^ /@color@/: color to use
    -> m ()
gradientAddColorStop gradient offset color = liftIO $ do
    gradient' <- unsafeManagedPtrGetPtr gradient
    let offset' = realToFrac offset
    color' <- unsafeManagedPtrGetPtr color
    gtk_gradient_add_color_stop gradient' offset' color'
    touchManagedPtr gradient
    touchManagedPtr color
    return ()

#if defined(ENABLE_OVERLOADING)
data GradientAddColorStopMethodInfo
instance (signature ~ (Double -> Gtk.SymbolicColor.SymbolicColor -> m ()), MonadIO m) => O.OverloadedMethod GradientAddColorStopMethodInfo Gradient signature where
    overloadedMethod = gradientAddColorStop

instance O.OverloadedMethodInfo GradientAddColorStopMethodInfo Gradient where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Gradient.gradientAddColorStop",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Gradient.html#v:gradientAddColorStop"
        })


#endif

-- method Gradient::ref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gradient"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gradient" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGradient" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Gradient" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gradient_ref" gtk_gradient_ref :: 
    Ptr Gradient ->                         -- gradient : TInterface (Name {namespace = "Gtk", name = "Gradient"})
    IO (Ptr Gradient)

{-# DEPRECATED gradientRef ["(Since version 3.8)","t'GI.Gtk.Structs.Gradient.Gradient' is deprecated."] #-}
-- | Increases the reference count of /@gradient@/.
-- 
-- /Since: 3.0/
gradientRef ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gradient
    -- ^ /@gradient@/: a t'GI.Gtk.Structs.Gradient.Gradient'
    -> m Gradient
    -- ^ __Returns:__ The same /@gradient@/
gradientRef gradient = liftIO $ do
    gradient' <- unsafeManagedPtrGetPtr gradient
    result <- gtk_gradient_ref gradient'
    checkUnexpectedReturnNULL "gradientRef" result
    result' <- (wrapBoxed Gradient) result
    touchManagedPtr gradient
    return result'

#if defined(ENABLE_OVERLOADING)
data GradientRefMethodInfo
instance (signature ~ (m Gradient), MonadIO m) => O.OverloadedMethod GradientRefMethodInfo Gradient signature where
    overloadedMethod = gradientRef

instance O.OverloadedMethodInfo GradientRefMethodInfo Gradient where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Gradient.gradientRef",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Gradient.html#v:gradientRef"
        })


#endif

-- method Gradient::resolve
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gradient"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gradient" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGradient" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "props"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleProperties" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "#GtkStyleProperties to use when resolving named colors"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resolved_gradient"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Pattern" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the resolved pattern"
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

foreign import ccall "gtk_gradient_resolve" gtk_gradient_resolve :: 
    Ptr Gradient ->                         -- gradient : TInterface (Name {namespace = "Gtk", name = "Gradient"})
    Ptr Gtk.StyleProperties.StyleProperties -> -- props : TInterface (Name {namespace = "Gtk", name = "StyleProperties"})
    Ptr (Ptr Cairo.Pattern.Pattern) ->      -- resolved_gradient : TInterface (Name {namespace = "cairo", name = "Pattern"})
    IO CInt

{-# DEPRECATED gradientResolve ["(Since version 3.8)","t'GI.Gtk.Structs.Gradient.Gradient' is deprecated."] #-}
-- | If /@gradient@/ is resolvable, /@resolvedGradient@/ will be filled in
-- with the resolved gradient as a cairo_pattern_t, and 'P.True' will
-- be returned. Generally, if /@gradient@/ can’t be resolved, it is
-- due to it being defined on top of a named color that doesn\'t
-- exist in /@props@/.
-- 
-- /Since: 3.0/
gradientResolve ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleProperties.IsStyleProperties a) =>
    Gradient
    -- ^ /@gradient@/: a t'GI.Gtk.Structs.Gradient.Gradient'
    -> a
    -- ^ /@props@/: t'GI.Gtk.Objects.StyleProperties.StyleProperties' to use when resolving named colors
    -> m ((Bool, Cairo.Pattern.Pattern))
    -- ^ __Returns:__ 'P.True' if the gradient has been resolved
gradientResolve gradient props = liftIO $ do
    gradient' <- unsafeManagedPtrGetPtr gradient
    props' <- unsafeManagedPtrCastPtr props
    resolvedGradient <- callocMem :: IO (Ptr (Ptr Cairo.Pattern.Pattern))
    result <- gtk_gradient_resolve gradient' props' resolvedGradient
    let result' = (/= 0) result
    resolvedGradient' <- peek resolvedGradient
    resolvedGradient'' <- (wrapBoxed Cairo.Pattern.Pattern) resolvedGradient'
    touchManagedPtr gradient
    touchManagedPtr props
    freeMem resolvedGradient
    return (result', resolvedGradient'')

#if defined(ENABLE_OVERLOADING)
data GradientResolveMethodInfo
instance (signature ~ (a -> m ((Bool, Cairo.Pattern.Pattern))), MonadIO m, Gtk.StyleProperties.IsStyleProperties a) => O.OverloadedMethod GradientResolveMethodInfo Gradient signature where
    overloadedMethod = gradientResolve

instance O.OverloadedMethodInfo GradientResolveMethodInfo Gradient where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Gradient.gradientResolve",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Gradient.html#v:gradientResolve"
        })


#endif

-- method Gradient::resolve_for_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gradient"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gradient" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "cairo" , name = "Pattern" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gradient_resolve_for_context" gtk_gradient_resolve_for_context :: 
    Ptr Gradient ->                         -- gradient : TInterface (Name {namespace = "Gtk", name = "Gradient"})
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    IO (Ptr Cairo.Pattern.Pattern)

-- | /No description available in the introspection data./
gradientResolveForContext ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.StyleContext.IsStyleContext a) =>
    Gradient
    -> a
    -> m Cairo.Pattern.Pattern
gradientResolveForContext gradient context = liftIO $ do
    gradient' <- unsafeManagedPtrGetPtr gradient
    context' <- unsafeManagedPtrCastPtr context
    result <- gtk_gradient_resolve_for_context gradient' context'
    checkUnexpectedReturnNULL "gradientResolveForContext" result
    result' <- (wrapBoxed Cairo.Pattern.Pattern) result
    touchManagedPtr gradient
    touchManagedPtr context
    return result'

#if defined(ENABLE_OVERLOADING)
data GradientResolveForContextMethodInfo
instance (signature ~ (a -> m Cairo.Pattern.Pattern), MonadIO m, Gtk.StyleContext.IsStyleContext a) => O.OverloadedMethod GradientResolveForContextMethodInfo Gradient signature where
    overloadedMethod = gradientResolveForContext

instance O.OverloadedMethodInfo GradientResolveForContextMethodInfo Gradient where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Gradient.gradientResolveForContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Gradient.html#v:gradientResolveForContext"
        })


#endif

-- method Gradient::to_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gradient"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gradient" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the gradient to print"
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

foreign import ccall "gtk_gradient_to_string" gtk_gradient_to_string :: 
    Ptr Gradient ->                         -- gradient : TInterface (Name {namespace = "Gtk", name = "Gradient"})
    IO CString

{-# DEPRECATED gradientToString ["(Since version 3.8)","t'GI.Gtk.Structs.Gradient.Gradient' is deprecated."] #-}
-- | Creates a string representation for /@gradient@/ that is suitable
-- for using in GTK CSS files.
gradientToString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gradient
    -- ^ /@gradient@/: the gradient to print
    -> m T.Text
    -- ^ __Returns:__ A string representation for /@gradient@/
gradientToString gradient = liftIO $ do
    gradient' <- unsafeManagedPtrGetPtr gradient
    result <- gtk_gradient_to_string gradient'
    checkUnexpectedReturnNULL "gradientToString" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr gradient
    return result'

#if defined(ENABLE_OVERLOADING)
data GradientToStringMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod GradientToStringMethodInfo Gradient signature where
    overloadedMethod = gradientToString

instance O.OverloadedMethodInfo GradientToStringMethodInfo Gradient where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Gradient.gradientToString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Gradient.html#v:gradientToString"
        })


#endif

-- method Gradient::unref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "gradient"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Gradient" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGradient" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gradient_unref" gtk_gradient_unref :: 
    Ptr Gradient ->                         -- gradient : TInterface (Name {namespace = "Gtk", name = "Gradient"})
    IO ()

{-# DEPRECATED gradientUnref ["(Since version 3.8)","t'GI.Gtk.Structs.Gradient.Gradient' is deprecated."] #-}
-- | Decreases the reference count of /@gradient@/, freeing its memory
-- if the reference count reaches 0.
-- 
-- /Since: 3.0/
gradientUnref ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gradient
    -- ^ /@gradient@/: a t'GI.Gtk.Structs.Gradient.Gradient'
    -> m ()
gradientUnref gradient = liftIO $ do
    gradient' <- unsafeManagedPtrGetPtr gradient
    gtk_gradient_unref gradient'
    touchManagedPtr gradient
    return ()

#if defined(ENABLE_OVERLOADING)
data GradientUnrefMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod GradientUnrefMethodInfo Gradient signature where
    overloadedMethod = gradientUnref

instance O.OverloadedMethodInfo GradientUnrefMethodInfo Gradient where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Gradient.gradientUnref",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Gradient.html#v:gradientUnref"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveGradientMethod (t :: Symbol) (o :: *) :: * where
    ResolveGradientMethod "addColorStop" o = GradientAddColorStopMethodInfo
    ResolveGradientMethod "ref" o = GradientRefMethodInfo
    ResolveGradientMethod "resolve" o = GradientResolveMethodInfo
    ResolveGradientMethod "resolveForContext" o = GradientResolveForContextMethodInfo
    ResolveGradientMethod "toString" o = GradientToStringMethodInfo
    ResolveGradientMethod "unref" o = GradientUnrefMethodInfo
    ResolveGradientMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGradientMethod t Gradient, O.OverloadedMethod info Gradient p) => OL.IsLabel t (Gradient -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGradientMethod t Gradient, O.OverloadedMethod info Gradient p, R.HasField t Gradient p) => R.HasField t Gradient p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGradientMethod t Gradient, O.OverloadedMethodInfo info Gradient) => OL.IsLabel t (O.MethodProxy info Gradient) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


