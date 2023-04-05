{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.WindowGeometryInfo
    ( 

-- * Exported types
    WindowGeometryInfo(..)                  ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveWindowGeometryInfoMethod         ,
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


-- | Memory-managed wrapper type.
newtype WindowGeometryInfo = WindowGeometryInfo (SP.ManagedPtr WindowGeometryInfo)
    deriving (Eq)

instance SP.ManagedPtrNewtype WindowGeometryInfo where
    toManagedPtr (WindowGeometryInfo p) = p

-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?
instance BoxedPtr WindowGeometryInfo where
    boxedPtrCopy = return
    boxedPtrFree = \_x -> return ()


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList WindowGeometryInfo
type instance O.AttributeList WindowGeometryInfo = WindowGeometryInfoAttributeList
type WindowGeometryInfoAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveWindowGeometryInfoMethod (t :: Symbol) (o :: *) :: * where
    ResolveWindowGeometryInfoMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveWindowGeometryInfoMethod t WindowGeometryInfo, O.OverloadedMethod info WindowGeometryInfo p) => OL.IsLabel t (WindowGeometryInfo -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveWindowGeometryInfoMethod t WindowGeometryInfo, O.OverloadedMethod info WindowGeometryInfo p, R.HasField t WindowGeometryInfo p) => R.HasField t WindowGeometryInfo p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveWindowGeometryInfoMethod t WindowGeometryInfo, O.OverloadedMethodInfo info WindowGeometryInfo) => OL.IsLabel t (O.MethodProxy info WindowGeometryInfo) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


