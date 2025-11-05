{-# OPTIONS_GHC -Wno-orphans #-}

-- Taken from https://github.com/haskell/os-string

module Arbitrary where

import Data.Char
import Data.Maybe
import System.OsString.Compat
import System.OsString.Internal.Types.Compat
import System.OsString.Posix.Compat   qualified as Posix
import System.OsString.Windows.Compat qualified as Windows
import Data.ByteString ( ByteString )
import Data.ByteString qualified as ByteString
import Test.QuickCheck


instance Arbitrary OsString where
  arbitrary = fmap fromJust $ encodeUtf <$> listOf filepathChar

instance Arbitrary PosixString where
  arbitrary = fmap fromJust $ Posix.encodeUtf <$> listOf filepathChar

instance Arbitrary WindowsString where
  arbitrary = fmap fromJust $ Windows.encodeUtf <$> listOf filepathChar


newtype NonNullString = NonNullString { nonNullString :: String }
  deriving Show

instance Arbitrary NonNullString where
  arbitrary = NonNullString <$> listOf filepathChar

filepathChar :: Gen Char
filepathChar = arbitraryUnicodeChar `suchThat` (\c -> not (isNull c) && isValidUnicode c)
 where
  isNull = (== '\NUL')
  isValidUnicode c = case generalCategory c of
      Surrogate -> False
      NotAssigned -> False
      _ -> True


newtype NonNullAsciiString = NonNullAsciiString { nonNullAsciiString :: String }
  deriving Show

instance Arbitrary NonNullAsciiString where
  arbitrary = NonNullAsciiString <$> listOf filepathAsciiChar

filepathAsciiChar :: Gen Char
filepathAsciiChar = arbitraryASCIIChar `suchThat` (\c -> not (isNull c))
 where
  isNull = (== '\NUL')

newtype NonNullSurrogateString = NonNullSurrogateString { nonNullSurrogateString :: String }
  deriving Show

instance Arbitrary NonNullSurrogateString where
  arbitrary = NonNullSurrogateString <$> listOf filepathWithSurrogates

filepathWithSurrogates :: Gen Char
filepathWithSurrogates =
  frequency
    [(3, arbitraryASCIIChar),
     (1, arbitraryUnicodeChar),
     (1, arbitraryBoundedEnum)
    ]


instance Arbitrary   ByteString where arbitrary   = ByteString.pack <$> arbitrary
instance CoArbitrary ByteString where coarbitrary = coarbitrary . ByteString.unpack
