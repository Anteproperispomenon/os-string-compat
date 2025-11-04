{-# LANGUAGE CPP #-}

module System.OsString.Encoding.Internal.Compat
  ( ucs2le
  , mkUcs2le
  , ucs2le_DF
  , ucs2le_EF
  , ucs2le_decode
  , ucs2le_encode
  , utf16le_b
  , mkUTF16le_b
  , utf16le_b_DF
  , utf16le_b_EF
  , utf16le_b_decode
  , utf16le_b_encode
  , cWcharsToChars_UCS2
  , cWcharsToChars
  , charsToCWchars
  , withWindowsString
  , withFilePathWin
  , peekWindowsString
  , peekFilePathWin
  , withPosixString
  , withFilePathPosix
#if MIN_VERSION_os_string(2,0,5)
  , withPosixString'
  , withFilePathPosix'
#endif
  , peekPosixString
#if MIN_VERSION_os_string(2,0,5)
  , peekPosixString'
#endif
  , peekFilePathPosix
#if MIN_VERSION_os_string(2,0,5)
  , peekFilePathPosix'
#endif
  , decodeWithTE
  , encodeWithTE
  , decodeWithBasePosix
#if MIN_VERSION_os_string(2,0,5)
  , decodeWithBasePosix'
#endif
  , encodeWithBasePosix
#if MIN_VERSION_os_string(2,0,5)
  , encodeWithBasePosix'
#endif
  , decodeWithBaseWindows
  , encodeWithBaseWindows
  , EncodingException(..)
  , showEncodingException
  , wNUL
  ) where

import GHC.Ptr
import Data.Word
import Foreign.C.String

#if MIN_VERSION_filepath(1,5,0)

import "os-string" System.OsString.Encoding.Internal

-- | Synonym of `withWindowsString`
withFilePathWin :: FilePath -> (Int -> Ptr Word16 -> IO a) -> IO a
withFilePathWin = withWindowsString
{-# INLINE withFilePathWin #-}

-- | Synonym of `peekWindowsString`
peekFilePathWin :: (Ptr Word16, Int) -> IO FilePath
peekFilePathWin = peekWindowsString
{-# INLINE peekFilePathWin #-}

-- | Synonym of `withPosixString`
withFilePathPosix :: FilePath -> (CStringLen -> IO a) -> IO a
withFilePathPosix = withPosixString
{-# INLINE withFilePathPosix #-}

#  if MIN_VERSION_os_string(2,0,5)
-- | Synonym of `withPosixString'`
withFilePathPosix' :: FilePath -> (CStringLen -> IO a) -> IO a
withFilePathPosix' = withPosixString'
{-# INLINE withFilePathPosix' #-}

peekFilePathPosix' :: CStringLen -> IO String
peekFilePathPosix' = peekPosixString'
{-# INLINE peekFilePathPosix' #-}
#  endif

peekFilePathPosix :: CStringLen -> IO String
peekFilePathPosix = peekPosixString
{-# INLINE peekFilePathPosix #-}

-- ---------------------------------------------------------------- --
-- ---------------------------------------------------------------- --
-- ---------------------------------------------------------------- --

#else

import "filepath" System.OsPath.Encoding.Internal

import "os-string" System.OsString.Encoding.Internal qualified as New

import Data.ByteString.Short (ShortByteString)

-- import GHC.IO.Encoding (getFileSystemEncoding, getLocaleEncoding)
-- import GHC.Foreign qualified as GHC

-- | Synonym of `withFilePathWin`
withWindowsString :: String -> (Int -> Ptr Word16 -> IO a) -> IO a
withWindowsString = withFilePathWin
{-# INLINE withWindowsString #-}

-- | Synonym of `peekFilePathWin`.
peekWindowsString :: (Ptr Word16, Int) -> IO FilePath
peekWindowsString = peekFilePathWin
{-# INLINE peekWindowsString #-}

-- | Synonym of `withFilePathPosix`
withPosixString :: FilePath -> (CStringLen -> IO a) -> IO a
withPosixString = withFilePathPosix
{-# INLINE withPosixString #-}

#  if MIN_VERSION_os_string(2,0,5)
-- | Alternate version of `withPosixString` from
--   newer versions of 
withPosixString' :: String -> (CStringLen -> IO a) -> IO a
withPosixString'= New.withPosixString'

-- | Synonym of `withPosixString'`.
withFilePathPosix' :: String -> (CStringLen -> IO a) -> IO a
withFilePathPosix' = withPosixString'
{-# INLINE withFilePathPosix' #-}
#  endif

peekPosixString :: CStringLen -> IO String
peekPosixString = peekFilePathPosix
{-# INLINE peekPosixString #-}

#  if MIN_VERSION_os_string(2,0,5)
-- may need to re-write this and other
-- -' functions to be based on the original
-- code.
peekFilePathPosix' :: CStringLen -> IO String
peekFilePathPosix' = New.peekPosixString'

peekPosixString' :: CStringLen -> IO String
peekPosixString' = New.peekPosixString'
{-# INLINE peekPosixString' #-}

decodeWithBasePosix' :: ShortByteString -> IO String
decodeWithBasePosix' = New.decodeWithBasePosix'
{-# INLINE decodeWithBasePosix' #-}

encodeWithBasePosix' :: String -> IO ShortByteString
encodeWithBasePosix' = New.encodeWithBasePosix'
{-# INLINE encodeWithBasePosix' #-}
#  endif

#endif