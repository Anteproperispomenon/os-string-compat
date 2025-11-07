-- taken from os-string

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeApplications    #-}

#define OSSTR pstr
#define OS_STRING PosixString
#define OS_CHAR PosixChar

module BenchPosixString (benchMark) where

import System.OsString.Posix.Compat          (PosixString, pstr)
import System.OsString.Posix.Compat          qualified as S
import System.OsString.Internal.Types.Compat (PosixChar(..))

#include "include/Common.hs"

benchStr :: String
benchStr = "PosixString"

w :: Int -> PosixChar
w i = PosixChar (fromIntegral i)

hashWord8 :: PosixChar -> PosixChar
hashWord8 (PosixChar w) = PosixChar . fromIntegral . hashInt . fromIntegral $ w

iw :: PosixChar -> Int
iw (PosixChar w) = fromIntegral w
