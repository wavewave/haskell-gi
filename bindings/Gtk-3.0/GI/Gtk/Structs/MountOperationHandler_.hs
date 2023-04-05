{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Abstract interface type for the D-Bus interface \<link linkend=\"gdbus-interface-org-Gtk-MountOperationHandler.top_of_page\">org.Gtk.MountOperationHandler\<\/link>.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.MountOperationHandler_
    ( 

-- * Exported types
    MountOperationHandler_(..)              ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveMountOperationHandler_Method     ,
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
newtype MountOperationHandler_ = MountOperationHandler_ (SP.ManagedPtr MountOperationHandler_)
    deriving (Eq)

instance SP.ManagedPtrNewtype MountOperationHandler_ where
    toManagedPtr (MountOperationHandler_ p) = p

-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?
instance BoxedPtr MountOperationHandler_ where
    boxedPtrCopy = return
    boxedPtrFree = \_x -> return ()


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList MountOperationHandler_
type instance O.AttributeList MountOperationHandler_ = MountOperationHandler_AttributeList
type MountOperationHandler_AttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveMountOperationHandler_Method (t :: Symbol) (o :: *) :: * where
    ResolveMountOperationHandler_Method l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMountOperationHandler_Method t MountOperationHandler_, O.OverloadedMethod info MountOperationHandler_ p) => OL.IsLabel t (MountOperationHandler_ -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMountOperationHandler_Method t MountOperationHandler_, O.OverloadedMethod info MountOperationHandler_ p, R.HasField t MountOperationHandler_ p) => R.HasField t MountOperationHandler_ p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMountOperationHandler_Method t MountOperationHandler_, O.OverloadedMethodInfo info MountOperationHandler_) => OL.IsLabel t (O.MethodProxy info MountOperationHandler_) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


