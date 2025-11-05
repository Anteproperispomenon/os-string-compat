{- HLINT ignore "Unused LANGUAGE pragma" -}

{-# LANGUAGE CPP #-}

-- To allow this file's source code
-- to be included when using Haddock.
#ifndef MODULE_NAME
{-# OPTIONS_HADDOCK hide #-}

#define MODULE_NAME Posix
#define IS_WINDOWS False
#define PLATFORM_STRING     PosixString
#define PLATFORM_STR_PLURAL PosixStrings
#define PLATFORM_WORD       PosixChar
#define IS_WINDOWS          False
#define IN_HADDOCK
#endif

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskellQuotes #-}
-- {-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}




-- This is essentially copied from os-string.
-- It's located in a folder that starts with
-- a lower-case character so that it doesn't
-- get compiled by default. Instead, it is
-- just to be CPP-included by other modules.

-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True
--
#if defined(WINDOWS)
#define WINDOWS_DOC
#else
#define POSIX_DOC
#endif

#ifdef IN_HADDOCK
module System.OsString.Common
#else
module System.OsString.MODULE_NAME.Compat
#endif
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsChar
#else
    PosixString
  , PosixChar
#endif

  -- * String construction
  , encodeUtf
  , unsafeEncodeUtf
  , encodeWith
  , encodeFS
#if MIN_VERSION_os_string(2,0,5)
  , encodeLE
#endif
#ifdef WINDOWS
#  if MIN_VERSION_os_string(2,0,6)
  , fromString
#  endif
#endif
  , fromBytes
#if MIN_VERSION_os_string(2,0,8)
  , fromShortBytes
#endif
#ifndef WINDOWS
#  if MIN_VERSION_os_string(2,0,6)
  , fromBytestring
#  endif
#  if MIN_VERSION_os_string(2,0,8)
  , fromShortBytestring
#  endif
#endif
  , pstr
  , singleton
  , empty
  , pack

  -- * String deconstruction
  , decodeUtf
  , decodeWith
  , decodeFS
#if MIN_VERSION_os_string(2,0,5)
  , decodeLE
#endif
  , unpack

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar

  -- * Basic interface
  , snoc
  , cons
  , last
  , tail
  , uncons
  , head
  , init
  , unsnoc
  , null
  , length
  , lengthBytes

  -- * Transforming PLATFORM_STR_PLURAL
  , map
  , reverse
  , intercalate

  -- * Reducing PLATFORM_STR_PLURAL (folds)
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr'
  , foldr1
  , foldr1'

  -- ** Special folds
  , all
  , any
  , concat

  -- ** Generating and unfolding PLATFORM_STR_PLURAL
  , replicate
  , unfoldr
  , unfoldrN

  -- * Substrings
  -- ** Breaking strings
  , take
  , takeEnd
  , takeWhileEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhileEnd
  , dropWhile
  , break
  , breakEnd
  , span
  , spanEnd
  , splitAt
  , split
  , splitWith
  , stripSuffix
  , stripPrefix

  -- * Predicates
  , isInfixOf
  , isPrefixOf
  , isSuffixOf
  -- ** Search for arbitrary susbstrings
  , breakSubstring

  -- * Searching PLATFORM_STR_PLURAL
  -- ** Searching by equality
  , elem
  , find
  , filter
  , partition

  -- * Indexing PLATFORM_STR_PLURAL
  , index
  , indexMaybe
  , (!?)
  , elemIndex
  , elemIndices
  , count
  , findIndex
  , findIndices
  )
where

#if MIN_VERSION_filepath(1,5,0)

import Prelude (Int)

import "os-string" System.OsString.Internal.Types qualified as NewT (PLATFORM_STRING(..), PLATFORM_WORD(..))
import System.OsString.Internal.Types.Compat (PLATFORM_STRING(..), PLATFORM_WORD(..))

import "os-string" System.OsString.MODULE_NAME hiding (length)

import Data.Coerce

#  ifdef WINDOWS
import "os-string" System.OsString.Data.ByteString.Short.Word16 qualified as B16
#  else
import "os-string" System.OsString.Data.ByteString.Short        qualified as B8
#  endif

-- | /O(1)/ The length of a `PLATFORM_STRING`.
--
-- This returns the number of code units
-- (@Word8@ on unix and @Word16@ on windows), not
-- bytes.
--
-- >>> length "abc"
-- 3
--
-- Note: older versions of os-string return the
-- length in bytes, rather than the length in
-- code units. This will return the length in
-- code units, regardless of the version of 
-- os-string. For checking the length in Bytes,
-- use `lengthBytes`.
length :: PLATFORM_STRING -> Int
-- length = coerce New.length
#ifdef WINDOWS
length = coerce B16.numWord16
#else
length = coerce B8.length
#endif

-- | /O(1)/ The length in bytes of a `PLATFORM_STRING`.
--
-- If you want the number of code units, just
-- use `length` instead.
lengthBytes :: PLATFORM_STRING -> Int
#ifdef WINDOWS
lengthBytes = coerce B16.length
#else
lengthBytes = coerce B8.length
#endif

-- End of (filepath >= 1.5.0) section
#else

import "os-string" System.OsString.MODULE_NAME qualified as New

#  ifdef WINDOWS
import "os-string" System.OsString.Data.ByteString.Short.Word16 qualified as B16
#  else
import "os-string" System.OsString.Data.ByteString.Short        qualified as B8
#  endif

import "filepath" System.OsString.MODULE_NAME (pstr, encodeWith, decodeWith)

import "os-string" System.OsString.Internal.Types qualified as NewT (PLATFORM_STRING(..), PLATFORM_WORD(..))
import System.OsString.Internal.Types.Compat (PLATFORM_STRING(..), PLATFORM_WORD(..))

import System.OsString.Internal.Exception.Compat
import System.OsString.Internal.Types.Compat (
#ifdef WINDOWS
  WindowsString(..), WindowsChar(..)
#else
  PosixString(..), PosixChar(..)
#endif
  )

import Data.Coerce
import Data.Char
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Data.ByteString.Internal
    ( ByteString )
import Data.ByteString.Short.Internal
    ( ShortByteString )
import Control.Exception
    ( SomeException, try, displayException )
-- import Control.DeepSeq ( force )
-- import Data.Bifunctor ( first )
-- import GHC.IO
--     ( evaluate, unsafePerformIO )
import qualified GHC.Foreign as GHC
-- import Language.Haskell.TH.Quote
--     ( QuasiQuoter (..) )
-- import Language.Haskell.TH.Syntax
--     ( Lift (..), lift )


import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
#ifdef WINDOWS
import System.OsString.Encoding.Compat
import System.IO
    ( TextEncoding, utf16le )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import System.OsString.Data.ByteString.Short.Word16 qualified as BSP
#else
import System.OsString.Encoding.Compat
import System.IO
    ( TextEncoding, utf8 )
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import qualified System.OsString.Data.ByteString.Short as BSP
#endif
import GHC.Stack (HasCallStack)
import Prelude (Bool(..), Int, Maybe(..), IO, String, Either(..), fmap, ($), (.), mconcat, fromEnum, fromInteger, mempty, fromIntegral, fail, (<$>), show, either, pure, const, flip, error, id)
-- import Data.Bifunctor ( bimap )
-- import qualified System.OsString.Data.ByteString.Short.Word16 as BS16
-- import qualified System.OsString.Data.ByteString.Short as BS8



#ifdef WINDOWS_DOC
-- | Partial unicode friendly encoding.
--
-- This encodes as UTF16-LE (strictly), which is a pretty good guess.
--
-- Throws an 'EncodingException' if encoding fails. If the input does not
-- contain surrogate chars, you can use @unsafeEncodeUtf@.
#else
-- | Partial unicode friendly encoding.
--
-- This encodes as UTF8 (strictly), which is a good guess.
--
-- Throws an 'EncodingException' if encoding fails. If the input does not
-- contain surrogate chars, you can use 'unsafeEncodeUtf'.
#endif
encodeUtf :: MonadThrow m => String -> m PLATFORM_STRING
encodeUtf str = coerce <$> New.encodeUtf str
-- #ifdef WINDOWS
-- encodeUtf = either throwM pure . encodeWith utf16le
-- #else
-- encodeUtf = either throwM pure . encodeWith utf8
-- #endif

-- | Unsafe unicode friendly encoding.
--
-- Like 'encodeUtf', except it crashes when the input contains
-- surrogate chars. For sanitized input, this can be useful.
unsafeEncodeUtf :: HasCallStack => String -> PLATFORM_STRING
#ifdef WINDOWS
unsafeEncodeUtf = coerce New.unsafeEncodeUtf -- either (error . displayException) id . encodeWith utf16le
#else
unsafeEncodeUtf = coerce New.unsafeEncodeUtf -- either (error . displayException) id . encodeWith utf8
#endif

{-
#ifdef WINDOWS
-- | Encode a 'String' with the specified encoding.
--
-- Note: We expect a "wide char" encoding (e.g. UCS-2 or UTF-16). Anything
-- that works with @Word16@ boundaries. Picking an incompatible encoding may crash
-- filepath operations.
encodeWith :: TextEncoding  -- ^ text encoding (wide char)
           -> String
           -> Either EncodingException PLATFORM_STRING
encodeWith = coerce New.encodeWith
#else
-- | Encode a 'String' with the specified encoding.
encodeWith :: TextEncoding
           -> String
           -> Either EncodingException PLATFORM_STRING
encodeWith = coerce New.encodeWith
#endif
-}

#ifdef WINDOWS_DOC
-- | This mimics the behavior of the base library when doing filesystem
-- operations (usually filepaths), which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. This is safe to 'unsafePerformIO'/'unsafeDupablePerformIO'.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations (usually filepaths), which uses shady PEP 383 style encoding (based on the current locale,
-- but PEP 383 only works properly on UTF-8 encodings, so good luck).
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
encodeFS :: String -> IO PLATFORM_STRING
#ifdef WINDOWS
{-# DEPRECATED encodeFS "Use System.OsPath.Windows.encodeFS from filepath" #-}
encodeFS = coerce New.encodeFS
#else
{-# DEPRECATED encodeFS "Use System.OsPath.Posix.encodeFS from filepath" #-}
encodeFS = coerce New.encodeFS
#endif

#if MIN_VERSION_os_string(2,0,5)
#  ifdef WINDOWS_DOC
-- | This mimics the behavior of the base library when doing string
-- operations, which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. This is safe to 'unsafePerformIO'/'unsafeDupablePerformIO'.
#  else
-- | This mimics the behavior of the base library when doing string
-- operations, which uses 'getLocaleEncoding'.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#  endif
encodeLE :: String -> IO PLATFORM_STRING
#  ifdef WINDOWS
encodeLE = coerce New.encodeLE
#  else
encodeLE = coerce New.encodeLE
#  endif
#endif

#ifdef WINDOWS
#  if MIN_VERSION_os_string(2,0,6)
-- | Like 'encodeLE but not in IO.
--
-- 'encodeLE' was designed to have a symmetric type signature
-- on unix and windows, but morally the function has no IO effects on windows,
-- so we provide this variant without breaking existing API.
--
-- On windows, 'encodeLE' is equivalent to 'encodeFS'.
--
-- This function does not exist on unix.
--
-- @since 2.0.6
fromString :: String -> WindowsString
fromString = coerce New.fromString
#  endif
#endif

#ifdef WINDOWS_DOC
-- | Partial unicode friendly decoding.
--
-- This decodes as UTF16-LE (strictly), which is a pretty good.
--
-- Throws a 'EncodingException' if decoding fails.
#else
-- | Partial unicode friendly decoding.
--
-- This decodes as UTF8 (strictly), which is a good guess. Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
#endif
decodeUtf :: MonadThrow m => PLATFORM_STRING -> m String
#ifdef WINDOWS
decodeUtf str = New.decodeUtf (coerce str)
#else
decodeUtf str = New.decodeUtf (coerce str)
#endif

{-
#ifdef WINDOWS
-- | Decode a 'WindowsString' with the specified encoding.
--
-- The String is forced into memory to catch all exceptions.
decodeWith :: TextEncoding
           -> PLATFORM_STRING
           -> Either EncodingException String
decodeWith = coerce New.decodeWith
-- decodeWith winEnc (WindowsString ba) = unsafePerformIO $ do
--   r <- trySafe @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen winEnc fp
--   evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
-- | Decode a 'PosixString' with the specified encoding.
--
-- The String is forced into memory to catch all exceptions.
decodeWith :: TextEncoding
       -> PLATFORM_STRING
       -> Either EncodingException String
decodeWith = coerce New.decodeWith
-- decodeWith unixEnc (PosixString ba) = unsafePerformIO $ do
--   r <- trySafe @SomeException $ BSP.useAsCStringLen ba $ \fp -> GHC.peekCStringLen unixEnc fp
--   evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif
-}

#if MIN_VERSION_os_string(2,0,5)
#ifdef WINDOWS_DOC
-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. 'unsafePerformIO'/'unsafeDupablePerformIO' are safe, however.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which uses 'getLocaleEncoding'.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
decodeLE :: PLATFORM_STRING -> IO String
decodeLE = coerce New.decodeLE
#endif

#ifdef WINDOWS_DOC
-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations (usually filepaths), which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. 'unsafePerformIO'/'unsafeDupablePerformIO' are safe, however.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations (usually filepaths), which uses shady PEP 383 style encoding (based on the current locale,
-- but PEP 383 only works properly on UTF-8 encodings, so good luck).
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
decodeFS :: PLATFORM_STRING -> IO String
#ifdef WINDOWS
{-# DEPRECATED decodeFS "Use System.OsPath.Windows.decodeFS from filepath" #-}
decodeFS = coerce New.decodeFS
-- decodeFS (WindowsString ba) = decodeWithBaseWindows ba
#else
{-# DEPRECATED decodeFS "Use System.OsPath.Posix.decodeFS from filepath" #-}
decodeFS = coerce New.decodeFS
-- decodeFS (PosixString ba) = decodeWithBasePosix ba
#endif


#ifdef WINDOWS_DOC
-- | Constructs a platform string from a ByteString.
--
-- This ensures valid UCS-2LE.
-- Note that this doesn't expand Word8 to Word16 on windows, so you may get invalid UTF-16.
--
-- Throws 'EncodingException' on invalid UCS-2LE (although unlikely).
#else
-- | Constructs a platform string from a ByteString.
--
-- This is a no-op.
#endif
fromBytes :: MonadThrow m
          => ByteString
          -> m PLATFORM_STRING
fromBytes bs = coerce <$> New.fromBytes bs
-- fromBytes = fromShortBytes . BS16.toShort

#if MIN_VERSION_os_string(2,0,8)
#ifdef WINDOWS_DOC
-- | Constructs a platform string from a ShortByteString.
--
-- This ensures valid UCS-2LE.
-- Note that this doesn't expand Word8 to Word16 on windows, so you may get invalid UTF-16.
--
-- Throws 'EncodingException' on invalid UCS-2LE (although unlikely).
--
-- @since 2.0.8
#else
-- | Constructs a platform string from a ShortByteString.
--
-- This is a no-op.
--
-- @since 2.0.8
#endif
fromShortBytes :: MonadThrow m
               => ShortByteString
               -> m PLATFORM_STRING
fromShortBytes sbs = coerce <$> New.fromShortBytes sbs
#endif

-- #ifdef WINDOWS
-- fromShortBytes bs =
--   let ws = WindowsString bs
--   in either throwM (const . pure $ ws) $ decodeWith ucs2le ws
-- #else
-- fromShortBytes = pure . PosixString
-- #endif

#ifndef WINDOWS
#  if MIN_VERSION_os_string(2,0,6)
-- | Like 'fromBytes', but not in IO.
--
-- 'fromBytes' was designed to have a symmetric type signature
-- on unix and windows, but morally the function has no IO effects on unix,
-- so we provide this variant without breaking existing API.
--
-- This function does not exist on windows.
--
-- @since 2.0.6
fromBytestring :: ByteString -> PosixString
fromBytestring = coerce New.fromBytestring
-- fromBytestring = PosixString . BSP.toShort
#  endif

#  if MIN_VERSION_os_string(2,0,8)
-- | Like 'fromShortBytes', but not in IO, similarly to 'fromBytestring'
--
-- @since 2.0.8
fromShortBytestring :: ShortByteString -> PosixString
fromShortBytestring = coerce New.fromShortBytestring
-- fromShortBytestring = PosixString
#  endif
#endif


-- pstr is included in the old version
{-
#ifdef WINDOWS_DOC
-- | QuasiQuote a 'WindowsString'. This accepts Unicode characters
-- and encodes as UTF-16LE on windows.
#else
-- | QuasiQuote a 'PosixString'. This accepts Unicode characters
-- and encodes as UTF-8 on unix.
#endif
pstr :: QuasiQuoter
pstr =
  QuasiQuoter
#ifdef WINDOWS
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF16le ErrorOnCodingFailure) s
      lift ps
  , quotePat = \s -> do
      osp' <- either (fail . show) pure . encodeWith (mkUTF16le ErrorOnCodingFailure) $ s
      [p|((==) osp' -> True)|]
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a declaration)"
  }
#else
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF8 ErrorOnCodingFailure) s
      lift ps
  , quotePat = \s -> do
      osp' <- either (fail . show) pure . encodeWith (mkUTF8 ErrorOnCodingFailure) $ s
      [p|((==) osp' -> True)|]
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a declaration)"
  }
#endif
-}

-- | Unpack a platform string to a list of platform words.
unpack :: PLATFORM_STRING -> [PLATFORM_WORD]
unpack = coerce New.unpack


-- | Pack a list of platform words to a platform string.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to platform string is probably not what
-- you want, because it will truncate unicode code points.
pack :: [PLATFORM_WORD] -> PLATFORM_STRING
pack = coerce New.pack

singleton :: PLATFORM_WORD -> PLATFORM_STRING
singleton = coerce New.singleton

empty :: PLATFORM_STRING
empty = mempty


#ifdef WINDOWS
-- | Truncates to 2 octets.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = coerce New.unsafeFromChar
#else
-- | Truncates to 1 octet.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = coerce New.unsafeFromChar
#endif

-- | Converts back to a unicode codepoint (total).
toChar :: PLATFORM_WORD -> Char
#ifdef WINDOWS
toChar = coerce New.toChar
#else
toChar = coerce New.toChar
#endif

-- | /O(n)/ Append a byte to the end of a `PLATFORM_STRING`
--
snoc :: PLATFORM_STRING -> PLATFORM_WORD -> PLATFORM_STRING
snoc = coerce New.snoc

-- | /O(n)/ 'cons' is analogous to (:) for lists.
--
cons :: PLATFORM_WORD -> PLATFORM_STRING -> PLATFORM_STRING
cons = coerce New.cons


-- | /O(1)/ Extract the last element of a `PLATFORM_STRING`, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty `PLATFORM_STRING`.
--
-- This is a partial function, consider using 'unsnoc' instead.
--
last :: HasCallStack => PLATFORM_STRING -> PLATFORM_WORD
last = coerce New.last

-- | /O(n)/ Extract the elements after the head of a `PLATFORM_STRING`, which must be non-empty.
-- An exception will be thrown in the case of an empty `PLATFORM_STRING`.
--
-- This is a partial function, consider using 'uncons' instead.
--
tail :: HasCallStack => PLATFORM_STRING -> PLATFORM_STRING
tail = coerce New.tail

-- | /O(n)/ Extract the 'head' and 'tail' of a `PLATFORM_STRING`, returning 'Nothing'
-- if it is empty.
--
uncons :: PLATFORM_STRING -> Maybe (PLATFORM_WORD, PLATFORM_STRING)
uncons = coerce New.uncons

-- | /O(1)/ Extract the first element of a `PLATFORM_STRING`, which must be non-empty.
-- An exception will be thrown in the case of an empty `PLATFORM_STRING`.
--
-- This is a partial function, consider using 'uncons' instead.
--
head :: HasCallStack => PLATFORM_STRING -> PLATFORM_WORD
head = coerce New.head

-- | /O(n)/ Return all the elements of a `PLATFORM_STRING` except the last one.
-- An exception will be thrown in the case of an empty `PLATFORM_STRING`.
--
-- This is a partial function, consider using 'unsnoc' instead.
--
init :: HasCallStack => PLATFORM_STRING -> PLATFORM_STRING
init = coerce New.init

-- | /O(n)/ Extract the 'init' and 'last' of a `PLATFORM_STRING`, returning 'Nothing'
-- if it is empty.
--
unsnoc :: PLATFORM_STRING -> Maybe (PLATFORM_STRING, PLATFORM_WORD)
unsnoc = coerce New.unsnoc

-- | /O(1)/. The empty `PLATFORM_STRING`.
--
null :: PLATFORM_STRING -> Bool
null = coerce New.null

-- | /O(1)/ The length of a `PLATFORM_STRING`.
--
-- This returns the number of code units
-- (@Word8@ on unix and @Word16@ on windows), not
-- bytes.
--
-- >>> length "abc"
-- 3
--
-- Note: older versions of os-string return the
-- length in bytes, rather than the length in
-- code units. This will return the length in
-- code units, regardless of the version of 
-- os-string. For checking the length in Bytes,
-- use `lengthBytes`.
length :: PLATFORM_STRING -> Int
-- length = coerce New.length
#ifdef WINDOWS
length = coerce B16.numWord16
#else
length = coerce B8.length
#endif

-- | /O(1)/ The length in bytes of a `PLATFORM_STRING`.
--
-- If you want the number of code units, just
-- use `length` instead.
lengthBytes :: PLATFORM_STRING -> Int
#ifdef WINDOWS
lengthBytes = coerce B16.length
#else
lengthBytes = coerce B8.length
#endif


-- | /O(n)/ 'map' @f xs@ is the `PLATFORM_STRING` obtained by applying @f@ to each
-- element of @xs@.
--
map :: (PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_STRING
map = coerce New.map

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
--
reverse :: PLATFORM_STRING -> PLATFORM_STRING
reverse = coerce New.reverse

-- | /O(n)/ The 'intercalate' function takes a `PLATFORM_STRING` and a list of
-- `PLATFORM_STRING`s and concatenates the list after interspersing the first
-- argument between each element of the list.
--
intercalate :: PLATFORM_STRING -> [PLATFORM_STRING] -> PLATFORM_STRING
intercalate = coerce New.intercalate

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a `PLATFORM_STRING`, reduces the
-- `PLATFORM_STRING` using the binary operator, from left to right.
--
foldl :: forall a. (a -> PLATFORM_WORD -> a) -> a -> PLATFORM_STRING -> a
foldl = coerce (New.foldl @a)

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
foldl'
  :: forall a. (a -> PLATFORM_WORD -> a) -> a -> PLATFORM_STRING -> a
foldl' = coerce (New.foldl' @a)

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty `PLATFORM_STRING`s.
-- An exception will be thrown in the case of an empty `PLATFORM_STRING`.
--
foldl1 :: (PLATFORM_WORD -> PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_WORD
foldl1 = coerce New.foldl1

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty `PLATFORM_STRING`.
--
foldl1'
  :: (PLATFORM_WORD -> PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_WORD
foldl1' = coerce New.foldl1'

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a `PLATFORM_STRING`,
-- reduces the `PLATFORM_STRING` using the binary operator, from right to left.
--
foldr :: forall a. (PLATFORM_WORD -> a -> a) -> a -> PLATFORM_STRING -> a
foldr = coerce (New.foldr @a)

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
--
foldr'
  :: forall a. (PLATFORM_WORD -> a -> a) -> a -> PLATFORM_STRING -> a
foldr' = coerce (New.foldr' @a)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty `PLATFORM_STRING`s
-- An exception will be thrown in the case of an empty `PLATFORM_STRING`.
--
foldr1 :: (PLATFORM_WORD -> PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_WORD
foldr1 = coerce New.foldr1

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
--
foldr1'
  :: (PLATFORM_WORD -> PLATFORM_WORD -> PLATFORM_WORD) -> PLATFORM_STRING -> PLATFORM_WORD
foldr1' = coerce New.foldr1'

-- | /O(n)/ Applied to a predicate and a 'PLATFString', 'all' determines
-- if all elements of the `PLATFORM_STRING` satisfy the predicate.
--
all :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> Bool
all = coerce New.all

-- | /O(n)/ Applied to a predicate and a `PLATFORM_STRING`, 'any' determines if
-- any element of the `PLATFORM_STRING` satisfies the predicate.
--
any :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> Bool
any = coerce New.any

-- /O(n)/ Concatenate a list of OsStrings.
--
concat :: [PLATFORM_STRING] -> PLATFORM_STRING
concat = mconcat

-- | /O(n)/ 'replicate' @n x@ is a `PLATFORM_STRING` of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
replicate :: Int -> PLATFORM_WORD -> PLATFORM_STRING
replicate = coerce New.replicate

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- `PLATFORM_STRING` from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the `PLATFORM_STRING` or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- This function is not efficient/safe. It will build a list of @[Word8]@
-- and run the generator until it returns `Nothing`, otherwise recurse infinitely,
-- then finally create a `PLATFORM_STRING`.
--
-- If you know the maximum length, consider using 'unfoldrN'.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: forall a. (a -> Maybe (PLATFORM_WORD, a)) -> a -> PLATFORM_STRING
unfoldr = coerce (New.unfoldr @a)

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a `PLATFORM_STRING` from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: forall a. Int -> (a -> Maybe (PLATFORM_WORD, a)) -> a -> (PLATFORM_STRING, Maybe a)
unfoldrN = coerce (New.unfoldrN @a)

-- | /O(n)/ 'take' @n@, applied to a `PLATFORM_STRING` @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
take :: Int -> PLATFORM_STRING -> PLATFORM_STRING
take = coerce New.take

-- | /O(n)/ @'takeEnd' n xs@ is equivalent to @'drop' ('length' xs - n) xs@.
-- Takes @n@ elements from end of bytestring.
--
-- >>> takeEnd 3 "abcdefg"
-- "efg"
-- >>> takeEnd 0 "abcdefg"
-- ""
-- >>> takeEnd 4 "abc"
-- "abc"
--
takeEnd :: Int -> PLATFORM_STRING -> PLATFORM_STRING
takeEnd = coerce New.takeEnd

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
takeWhileEnd :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
takeWhileEnd = coerce New.takeWhileEnd

-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
--
takeWhile :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
takeWhile = coerce New.takeWhile

-- | /O(n)/ 'drop' @n@ @xs@ returns the suffix of @xs@ after the first n elements, or 'empty' if @n > 'length' xs@.
--
drop :: Int -> PLATFORM_STRING -> PLATFORM_STRING
drop = coerce New.drop

-- | /O(n)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "abcdefg"
-- "abcd"
-- >>> dropEnd 0 "abcdefg"
-- "abcdefg"
-- >>> dropEnd 4 "abc"
-- ""
dropEnd :: Int -> PLATFORM_STRING -> PLATFORM_STRING
dropEnd = coerce New.dropEnd

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
dropWhile :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
dropWhile = coerce New.dropWhile

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
dropWhileEnd :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
dropWhileEnd = coerce New.dropWhileEnd

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
breakEnd :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
breakEnd = coerce New.breakEnd

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
break :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
break = coerce New.break

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
span :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
span = coerce New.span

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'spanEnd' @p@ is equivalent to @'breakEnd' (not . p)@ and to @('takeWhileEnd' p &&& 'dropWhileEnd' p)@.
--
-- We have
--
-- > spanEnd (not . isSpace) "x y z" == ("x y ", "z")
--
-- and
--
-- > spanEnd (not . isSpace) sbs
-- >    ==
-- > let (x, y) = span (not . isSpace) (reverse sbs) in (reverse y, reverse x)
--
spanEnd :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
spanEnd = coerce New.spanEnd

-- | /O(n)/ 'splitAt' @n sbs@ is equivalent to @('take' n sbs, 'drop' n sbs)@.
splitAt :: Int -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
splitAt = coerce New.splitAt

-- | /O(n)/ Break a `PLATFORM_STRING` into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split 10  "a\nb\nd\ne" == ["a","b","d","e"]   -- fromEnum '\n' == 10
-- > split 97  "aXaXaXa"    == ["","X","X","X",""] -- fromEnum 'a' == 97
-- > split 120 "x"          == ["",""]             -- fromEnum 'x' == 120
-- > split undefined ""     == []                  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
split :: PLATFORM_WORD -> PLATFORM_STRING -> [PLATFORM_STRING]
split = coerce New.split

-- | /O(n)/ Splits a `PLATFORM_STRING` into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> [PLATFORM_STRING]
splitWith = coerce New.splitWith

-- | /O(n)/ The 'stripSuffix' function takes two OsStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
stripSuffix :: PLATFORM_STRING -> PLATFORM_STRING -> Maybe PLATFORM_STRING
stripSuffix = coerce New.stripSuffix

-- | /O(n)/ The 'stripPrefix' function takes two OsStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
stripPrefix :: PLATFORM_STRING -> PLATFORM_STRING -> Maybe PLATFORM_STRING
stripPrefix = coerce New.stripPrefix


-- | Check whether one string is a substring of another.
isInfixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
isInfixOf = coerce New.isInfixOf

-- |/O(n)/ The 'isPrefixOf' function takes two OsStrings and returns 'True'
isPrefixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
isPrefixOf = coerce New.isPrefixOf

-- | /O(n)/ The 'isSuffixOf' function takes two OsStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
isSuffixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
isSuffixOf = coerce New.isSuffixOf


-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurrence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
-- Note that calling `breakSubstring x` does some preprocessing work, so
-- you should avoid unnecessarily duplicating breakSubstring calls with the same
-- pattern.
breakSubstring :: PLATFORM_STRING -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
breakSubstring = coerce New.breakSubstring

-- | /O(n)/ 'elem' is the `PLATFORM_STRING` membership predicate.
elem :: PLATFORM_WORD -> PLATFORM_STRING -> Bool
elem = coerce New.elem

-- | /O(n)/ The 'find' function takes a predicate and a `PLATFORM_STRING`,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> Maybe PLATFORM_WORD
find = coerce New.find

-- | /O(n)/ 'filter', applied to a predicate and a `PLATFORM_STRING`,
-- returns a `PLATFORM_STRING` containing those characters that satisfy the
-- predicate.
filter :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
filter = coerce New.filter

-- | /O(n)/ The 'partition' function takes a predicate a `PLATFORM_STRING` and returns
-- the pair of OsStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p sbs, filter (not . p) sbs)
--
partition :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> (PLATFORM_STRING, PLATFORM_STRING)
partition = coerce New.partition

-- | /O(1)/ `PLATFORM_STRING` index (subscript) operator, starting from 0.
index :: HasCallStack => PLATFORM_STRING -> Int -> PLATFORM_WORD
index = coerce New.index

-- | /O(1)/ `PLATFORM_STRING` index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
indexMaybe :: PLATFORM_STRING -> Int -> Maybe PLATFORM_WORD
indexMaybe = coerce New.indexMaybe

-- | /O(1)/ `PLATFORM_STRING` index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
(!?) :: PLATFORM_STRING -> Int -> Maybe PLATFORM_WORD
(!?) = indexMaybe

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given `PLATFORM_STRING` which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: PLATFORM_WORD -> PLATFORM_STRING -> Maybe Int
elemIndex = coerce New.elemIndex

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: PLATFORM_WORD -> PLATFORM_STRING -> [Int]
elemIndices = coerce New.elemIndices

-- | count returns the number of times its argument appears in the `PLATFORM_STRING`
count :: PLATFORM_WORD -> PLATFORM_STRING -> Int
count = coerce New.count

-- | /O(n)/ The 'findIndex' function takes a predicate and a `PLATFORM_STRING` and
-- returns the index of the first element in the `PLATFORM_STRING`
-- satisfying the predicate.
findIndex :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> Maybe Int
findIndex = coerce New.findIndex

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
--
findIndices :: (PLATFORM_WORD -> Bool) -> PLATFORM_STRING -> [Int]
findIndices = coerce New.findIndices

#endif
