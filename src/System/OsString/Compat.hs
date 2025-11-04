{-|
Module      : System.OsString.Compat
Copyright   : (c) 2025 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

Compatibility layer for versions of filepath that
don't import os-string. For versions that do, it
just re-exports the corresponding functions from
"System.OsString".

Note: All documentation is taken from the os-string
documentation, except for a few functions/types that 
are found in the older versions of "filepath".

-}

module System.OsString.Compat
  -- * String types
  ( OsString

  -- * OsString construction
  , encodeUtf
  , unsafeEncodeUtf
  , encodeWith
  , encodeFS
  , osstr
  , empty
  , singleton
  , pack

  -- * OsString deconstruction
  , decodeUtf
  , decodeWith
  , decodeFS
  , unpack

  -- * Word types
  , OsChar

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

  -- * Transforming OsString
  , map
  , reverse
  , intercalate

  -- * Reducing OsStrings (folds)
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr'
  , foldr1
  , foldr1'

  -- * Special folds
  , all
  , any
  , concat

  -- * Generating and unfolding OsStrings
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

  -- * Searching OsStrings
  -- ** Searching by equality
  , elem
  , find
  , filter
  , partition

  -- * Indexing OsStrings
  , index
  , indexMaybe
  , (!?)
  , elemIndex
  , elemIndices
  , count
  , findIndex
  , findIndices

  -- * Coercions
  , coercionToPlatformTypes
  ) where

import Prelude ()

import System.OsString.Internal.Compat
import System.OsString.Internal.Types.Compat

