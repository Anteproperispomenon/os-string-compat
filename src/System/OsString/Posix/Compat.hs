{-# LANGUAGE CPP #-}

-- Note: to find the actual source code for
-- this module, change the last part of the
-- URL to "System.OsString.Common.html"

#undef WINDOWS
#define MODULE_NAME         Posix
#define PLATFORM_STRING     PosixString
#define PLATFORM_STR_PLURAL PosixStrings
#define PLATFORM_WORD       PosixChar
#define IS_WINDOWS          False
#include "../include/Common.hs"
