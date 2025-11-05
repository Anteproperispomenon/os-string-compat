{-# LANGUAGE CPP #-}

module System.OsString.Internal.Exception.Compat
  ( trySafe
  , isAsyncException
  ) where

#if MIN_VERSION_os_string(2,0,7)
import "os-string" System.OsString.Internal.Exception
  ( isAsyncException, trySafe )
#else

import Control.Exception ( catch, fromException, toException, throwIO, Exception, SomeAsyncException(..) )

-- Again, taken from os-string.

-- | Like 'try', but rethrows async exceptions.
trySafe :: Exception e => IO a -> IO (Either e a)
trySafe ioA = catch action eHandler
 where
  action = do
    v <- ioA
    return (Right v)
  eHandler e
    | isAsyncException e = throwIO e
    | otherwise = return (Left e)

isAsyncException :: Exception e => e -> Bool
isAsyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> True
        Nothing -> False

#endif