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
#define OS_STRING WindowsString
#define OS_CHAR WindowsChar

module BenchWindowsString (benchMark) where

import System.OsString.Windows.Compat        (WindowsString, WindowsChar, pstr)
import System.OsString.Windows.Compat        qualified as S
import System.OsString.Internal.Types.Compat (WindowsChar(..))

#include "include/Common.hs"

benchStr :: String
benchStr = "WindowsString"

w :: Int -> WindowsChar
w i = WindowsChar (fromIntegral i)

hashWord8 :: WindowsChar -> WindowsChar
hashWord8 (WindowsChar w) = WindowsChar . fromIntegral . hashInt . fromIntegral $ w

iw :: WindowsChar -> Int
iw (WindowsChar w) = fromIntegral w
