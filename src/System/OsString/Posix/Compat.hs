{-# LANGUAGE CPP #-}
#undef WINDOWS
#define MODULE_NAME         Posix
#define PLATFORM_STRING     PosixString
#define PLATFORM_STR_PLURAL PosixStrings
#define PLATFORM_WORD       PosixChar
#define IS_WINDOWS          False
#include "../include/Common.hs"
