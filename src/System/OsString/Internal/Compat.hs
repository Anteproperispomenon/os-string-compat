{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}

module System.OsString.Internal.Compat
  -- * OsString construction
  ( encodeUtf
  , unsafeEncodeUtf
  , encodeWith
  , encodeFS
#if MIN_VERSION_os_string(2,0,5)
  , encodeLE
#endif
  , osstr
  , empty
  , singleton
  , pack
  , fromBytes
#if MIN_VERSION_os_string(2,0,8)
  , fromShortBytes
#endif

  -- * OsString deconstruction
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
  ) where

import System.OsString.Internal.Types.Compat

#if MIN_VERSION_filepath(1,5,0)

import Prelude ()

-- These should have the same types as in
-- System.OsString.Internal.Types.Compat, so
-- it shouldn't be a problem to use this directly.
import "os-string" System.OsString.Internal

#else

import Data.Kind (Type)

import Data.ByteString.Short (ShortByteString(..))

import Data.Coerce


import "filepath" System.OsString.Internal
  ( encodeUtf
  , encodeWith
  , fromBytes
  , osstr
  , pack
  , decodeUtf
  , decodeWith
  , decodeFS
  , unpack
  , unsafeFromChar
  -- , toChar

  )

-- Needed to allow coercions.
import "os-string" System.OsString.Internal.Types qualified as OST

import "os-string" System.OsString.Internal qualified as OS
import "filepath"  System.OsString.Internal qualified as Old

import System.OsString.Internal.Types.Compat

#if defined(mingw32_HOST_OS)
import System.OsString.Windows.Compat qualified as PF
#else
import System.OsString.Posix.Compat qualified as PF
#endif

import Control.Monad.Catch (MonadThrow)

import GHC.Stack (HasCallStack)
import Prelude (Bool, Int, Maybe, String, IO, Char, (<$>))

-- | Unsafe unicode friendly encoding.
--
-- Like 'encodeUtf', except it crashes when the input contains
-- surrogate chars. For sanitized input, this can be useful.
unsafeEncodeUtf :: HasCallStack => String -> OsString
unsafeEncodeUtf = coerce OS.unsafeEncodeUtf

-- | Like 'encodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations (usually filepaths), which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
--
encodeFS :: String -> IO OsString
encodeFS = coerce OS.encodeFS

#if MIN_VERSION_os_string(2,0,5)
-- | Like 'encodeUtf', except this mimics the behavior of the base library when doing string
-- operations, which is:
--
-- 1. on unix this uses 'getLocaleEncoding'
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
encodeLE :: String -> IO OsString
encodeLE = coerce OS.encodeLE
#endif

-- Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations (usually filepaths), which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
--
-- Note: this is different from `decodeFS` provided by older versions
-- of filepath. That function is equivalent to `decodeLE` in "os-string".
-- decodeFS :: OsString -> IO String
-- decodeFS = coerce OS.encodeFS

#if MIN_VERSION_os_string(2,0,5)
-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing string operations,
-- which is:
--
-- 1. on unix this uses 'getLocaleEncoding'
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
decodeLE :: OsString -> IO String
decodeLE = coerce OS.decodeLE
#endif

empty :: OsString
empty = coerce OS.empty

singleton :: OsChar -> OsString
singleton = coerce OS.singleton

-- | Converts back to a unicode codepoint (total).
--
--   Note that this uses the version from os-string, not filepath
toChar :: OsChar -> Char
toChar = coerce OS.toChar

-- | /O(n)/ Append a byte/word to the end of an `OsString`
snoc :: OsString -> OsChar -> OsString
snoc = coerce OS.snoc

-- | /O(n)/ 'cons' is analogous to (:) for lists.
cons :: OsChar -> OsString -> OsString
cons = coerce OS.cons

-- | /O(1)/ Extract the last element of an `OsString`, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => OsString -> OsChar
last = coerce OS.last

-- | /O(n)/ Extract the elements after the head of an `OsString`, which must be non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'uncons' instead.
tail :: HasCallStack => OsString -> OsString
tail = coerce OS.tail

-- | /O(n)/ Extract the 'head' and 'tail' of an `OsString`, returning 'Nothing'
-- if it is empty.
uncons :: OsString -> Maybe (OsChar, OsString)
uncons = coerce OS.uncons

-- | /O(1)/ Extract the first element of an `OsString`, which must be non-empty.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => OsString -> OsChar
head = coerce OS.head

-- | /O(n)/ Return all the elements of an `OsString` except the last one.
-- An exception will be thrown in the case of an empty OsString.
--
-- This is a partial function, consider using 'unsnoc' instead.
init :: HasCallStack => OsString -> OsString
init = coerce OS.init

-- | /O(n)/ Extract the 'init' and 'last' of an `OsString`, returning 'Nothing'
-- if it is empty.
unsnoc :: OsString -> Maybe (OsString, OsChar)
unsnoc = coerce OS.unsnoc

-- | /O(1)/ Test whether an `OsString` is empty.
null :: OsString -> Bool
null = coerce OS.null

-- | /O(1)/ The length of an `OsString`.
--
-- This returns the number of code units
-- (@Word8@ on unix and @Word16@ on windows), not
-- bytes.
length :: OsString -> Int
length = coerce PF.length

-- | /O(1)/ The length in bytes of an `OsString`.
--
-- This always returns the number of bytes,
-- regardless of which platform you're on.
lengthBytes :: OsString -> Int
lengthBytes = coerce PF.lengthBytes

-- | /O(n)/ 'map' @f xs@ is the OsString obtained by applying @f@ to each
-- element of @xs@.
map :: (OsChar -> OsChar) -> OsString -> OsString
map = coerce OS.map

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: OsString -> OsString
reverse = coerce OS.reverse

-- | /O(n)/ The 'intercalate' function takes an `OsString` and a list of
-- 'OsString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: OsString -> [OsString] -> OsString
intercalate = coerce OS.intercalate

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and an `OsString`, reduces the
-- OsString using the binary operator, from left to right.
foldl :: forall a. (a -> OsChar -> a) -> a -> OsString -> a
foldl = coerce (OS.foldl @a)

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
foldl' :: forall a. (a -> OsChar -> a) -> a -> OsString -> a
foldl' = coerce (OS.foldl' @a)

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'OsString's.
-- An exception will be thrown in the case of an empty OsString.
foldl1 :: (OsChar -> OsChar -> OsChar) -> OsString -> OsChar
foldl1 = coerce OS.foldl1

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty OsString.
foldl1' :: (OsChar -> OsChar -> OsChar) -> OsString -> OsChar
foldl1' = coerce OS.foldl1'


-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and an `OsString`,
-- reduces the OsString using the binary operator, from right to left.
foldr :: forall a. (OsChar -> a -> a) -> a -> OsString -> a
foldr = coerce (OS.foldr @a)

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
foldr' :: forall a. (OsChar -> a -> a) -> a -> OsString -> a
foldr' = coerce (OS.foldr' @a)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'OsString's
-- An exception will be thrown in the case of an empty OsString.
foldr1 :: (OsChar -> OsChar -> OsChar) -> OsString -> OsChar
foldr1 = coerce OS.foldr1

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
foldr1' :: (OsChar -> OsChar -> OsChar) -> OsString -> OsChar
foldr1' = coerce OS.foldr1'

-- | /O(n)/ Applied to a predicate and an `OsString`, 'all' determines
-- if all elements of the 'OsString' satisfy the predicate.
all :: (OsChar -> Bool) -> OsString -> Bool
all = coerce OS.all

-- | /O(n)/ Applied to a predicate and an `OsString`, 'any' determines if
-- any element of the 'OsString' satisfies the predicate.
any :: (OsChar -> Bool) -> OsString -> Bool
any = coerce OS.any

-- /O(n)/ Concatenate a list of OsStrings.
--
concat :: [OsString] -> OsString
concat = coerce OS.concat

-- | /O(n)/ 'replicate' @n x@ is an `OsString` of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
replicate :: Int -> OsChar -> OsString
replicate = coerce OS.replicate

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- OsString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the OsString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- This function is not efficient/safe. It will build a list of @[Word8]@
-- and run the generator until it returns `Nothing`, otherwise recurse infinitely,
-- then finally create an `OsString`.
--
-- If you know the maximum length, consider using 'unfoldrN'.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: forall a. (a -> Maybe (OsChar, a)) -> a -> OsString
unfoldr = coerce (OS.unfoldr @a)

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds an `OsString` from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: forall a. Int -> (a -> Maybe (OsChar, a)) -> a -> (OsString, Maybe a)
unfoldrN = coerce (OS.unfoldrN @a)

-- | /O(n)/ 'take' @n@, applied to an `OsString` @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> OsString -> OsString
take = coerce OS.take

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
takeEnd :: Int -> OsString -> OsString
takeEnd = coerce OS.takeEnd

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
takeWhileEnd :: (OsChar -> Bool) -> OsString -> OsString
takeWhileEnd = coerce OS.takeWhileEnd

-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
takeWhile :: (OsChar -> Bool) -> OsString -> OsString
takeWhile = coerce OS.takeWhile

-- | /O(n)/ 'drop' @n@ @xs@ returns the suffix of @xs@ after the first n elements, or 'empty' if @n > 'length' xs@.
drop :: Int -> OsString -> OsString
drop = coerce OS.drop

-- | /O(n)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "abcdefg"
-- "abcd"
-- >>> dropEnd 0 "abcdefg"
-- "abcdefg"
-- >>> dropEnd 4 "abc"
-- ""
dropEnd :: Int -> OsString -> OsString
dropEnd = coerce OS.dropEnd

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
dropWhile :: (OsChar -> Bool) -> OsString -> OsString
dropWhile = coerce OS.dropWhile

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
dropWhileEnd :: (OsChar -> Bool) -> OsString -> OsString
dropWhileEnd = coerce OS.dropWhileEnd

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
breakEnd :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
breakEnd = coerce OS.breakEnd

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
break :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
break = coerce OS.break

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
span = coerce OS.span

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
spanEnd :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
spanEnd = coerce OS.spanEnd

-- | /O(n)/ 'splitAt' @n sbs@ is equivalent to @('take' n sbs, 'drop' n sbs)@.
splitAt :: Int -> OsString -> (OsString, OsString)
splitAt = coerce OS.splitAt

-- | /O(n)/ Break an `OsString` into pieces separated by the byte
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
split :: OsChar -> OsString -> [OsString]
split = coerce OS.split

-- | /O(n)/ Splits an `OsString` into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (OsChar -> Bool) -> OsString -> [OsString]
splitWith = coerce OS.splitWith

-- | /O(n)/ The 'stripSuffix' function takes two OsStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
stripSuffix :: OsString -> OsString -> Maybe OsString
stripSuffix = coerce OS.stripSuffix

-- | /O(n)/ The 'stripPrefix' function takes two OsStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
stripPrefix :: OsString -> OsString -> Maybe OsString
stripPrefix = coerce OS.stripPrefix


-- | Check whether one string is a substring of another.
isInfixOf :: OsString -> OsString -> Bool
isInfixOf = coerce OS.isInfixOf

-- |/O(n)/ The 'isPrefixOf' function takes two OsStrings and returns 'True'
isPrefixOf :: OsString -> OsString -> Bool
isPrefixOf = coerce OS.isPrefixOf

-- | /O(n)/ The 'isSuffixOf' function takes two OsStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
isSuffixOf :: OsString -> OsString -> Bool
isSuffixOf = coerce OS.isSuffixOf

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
breakSubstring :: OsString -> OsString -> (OsString, OsString)
breakSubstring = coerce OS.breakSubstring

-- | /O(n)/ 'elem' is the 'OsString' membership predicate.
elem :: OsChar -> OsString -> Bool
elem = coerce OS.elem

-- | /O(n)/ The 'find' function takes a predicate and an `OsString`,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (OsChar -> Bool) -> OsString -> Maybe OsChar
find = coerce OS.find

-- | /O(n)/ 'filter', applied to a predicate and an `OsString`,
-- returns an `OsString` containing those characters that satisfy the
-- predicate.
filter :: (OsChar -> Bool) -> OsString -> OsString
filter = coerce OS.filter

-- | /O(n)/ The 'partition' function takes a predicate an `OsString` and returns
-- the pair of OsStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p sbs, filter (not . p) sbs)
--
partition :: (OsChar -> Bool) -> OsString -> (OsString, OsString)
partition = coerce OS.partition

-- | /O(1)/ 'OsString' index (subscript) operator, starting from 0.
index :: HasCallStack => OsString -> Int -> OsChar
index = coerce OS.index

-- | /O(1)/ 'OsString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
indexMaybe :: OsString -> Int -> Maybe OsChar
indexMaybe = coerce OS.indexMaybe

-- | /O(1)/ 'OsString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
(!?) :: OsString -> Int -> Maybe OsChar
(!?) = indexMaybe

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'OsString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: OsChar -> OsString -> Maybe Int
elemIndex = coerce OS.elemIndex

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: OsChar -> OsString -> [Int]
elemIndices = coerce OS.elemIndices

-- | count returns the number of times its argument appears in the OsString
count :: OsChar -> OsString -> Int
count = coerce OS.count

-- | /O(n)/ The 'findIndex' function takes a predicate and an `OsString` and
-- returns the index of the first element in the OsString
-- satisfying the predicate.
findIndex :: (OsChar -> Bool) -> OsString -> Maybe Int
findIndex = coerce OS.findIndex

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (OsChar -> Bool) -> OsString -> [Int]
findIndices = coerce OS.findIndices

#endif

#if MIN_VERSION_os_string(2,0,8) && !(MIN_VERSION_filepath(1,5,0))

-- | Constructs an @OsString@ from a ShortByteString.
--
-- On windows, this ensures valid UCS-2LE, on unix it is passed unchanged/unchecked.
--
-- Throws 'EncodingException' on invalid UCS-2LE on windows (although unlikely).
fromShortBytes :: forall (m :: Type -> Type). MonadThrow m => ShortByteString -> m OsString
fromShortBytes sbs = coerce <$> (OS.fromShortBytes @m sbs)

#endif

