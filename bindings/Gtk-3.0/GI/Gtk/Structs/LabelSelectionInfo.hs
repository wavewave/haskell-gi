{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.LabelSelectionInfo
    ( 

-- * Exported types
    LabelSelectionInfo(..)                  ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveLabelSelectionInfoMethod         ,
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
newtype LabelSelectionInfo = LabelSelectionInfo (SP.ManagedPtr LabelSelectionInfo)
    deriving (Eq)

instance SP.ManagedPtrNewtype LabelSelectionInfo where
    toManagedPtr (LabelSelectionInfo p) = p

-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?
instance BoxedPtr LabelSelectionInfo where
    boxedPtrCopy = return
    boxedPtrFree = \_x -> return ()


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList LabelSelectionInfo
type instance O.AttributeList LabelSelectionInfo = LabelSelectionInfoAttributeList
type LabelSelectionInfoAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveLabelSelectionInfoMethod (t :: Symbol) (o :: *) :: * where
    ResolveLabelSelectionInfoMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveLabelSelectionInfoMethod t LabelSelectionInfo, O.OverloadedMethod info LabelSelectionInfo p) => OL.IsLabel t (LabelSelectionInfo -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveLabelSelectionInfoMethod t LabelSelectionInfo, O.OverloadedMethod info LabelSelectionInfo p, R.HasField t LabelSelectionInfo p) => R.HasField t LabelSelectionInfo p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveLabelSelectionInfoMethod t LabelSelectionInfo, O.OverloadedMethodInfo info LabelSelectionInfo) => OL.IsLabel t (O.MethodProxy info LabelSelectionInfo) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


