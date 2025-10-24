{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}

module System.OsString.Internal.Types.Compat
  ( WindowsString(..)
  , pattern WS
  , unWS
  , PosixString(..)
  , unPS
  , pattern PS
  , PlatformString
  , WindowsChar(..)
  , unWW
  , pattern WW
  , PosixChar(..)
  , unPW
  , pattern PW
  , PlatformChar
  , OsString(..)
  , OsChar(..)
#if MIN_VERSION_os_string(2,0,2)
  , coercionToPlatformTypes
#endif
  ) where

#if MIN_VERSION_filepath(1,5,0)

import "os-string" System.OsString.Internal.Types

#else

import "filepath" System.OsString.Internal.Types

import Data.Type.Coercion (Coercion(..))

-- | This is a type-level evidence that 'OsChar' is a newtype wrapper
-- over 'WindowsChar' or 'PosixChar' and 'OsString' is a newtype wrapper
-- over 'WindowsString' or 'PosixString'. If you pattern match on
-- 'coercionToPlatformTypes', GHC will know that relevant types
-- are coercible to each other. This helps to avoid CPP in certain scenarios.
coercionToPlatformTypes
  :: Either
  (Coercion OsChar WindowsChar, Coercion OsString WindowsString)
  (Coercion OsChar PosixChar, Coercion OsString PosixString)
#if defined(mingw32_HOST_OS)
coercionToPlatformTypes = Left (Coercion, Coercion)
#else
coercionToPlatformTypes = Right (Coercion, Coercion)
#endif

#endif