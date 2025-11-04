{-# LANGUAGE CPP #-}

#undef POSIX
#define MODULE_NAME         Windows
#define PLATFORM_STRING     WindowsString
#define PLATFORM_STR_PLURAL WindowsStrings
#define PLATFORM_WORD       WindowsChar
#define IS_WINDOWS          True
#define WINDOWS
#include "../include/Common.hs"
#undef MODULE_NAME
#undef FILEPATH_NAME
#undef OSSTRING_NAME
#undef IS_WINDOWS
#undef WINDOWS
