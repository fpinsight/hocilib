-- |
-- Module:      Database.Ocilib.Internal
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Internal where

import Foreign.Ptr

toMaybePtr :: Ptr a -> Maybe (Ptr a)
toMaybePtr a
    | a == nullPtr = Nothing
    | otherwise    = Just a
