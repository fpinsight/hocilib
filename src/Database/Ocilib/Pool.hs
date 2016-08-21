{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Pool
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Pool
    (
    -- * Oracle Pools
      Pool
    , ociPoolCreate
    , ociPoolFree
    , ociPoolGetConnection
    , ociPoolGetTimeout
    , ociPoolSetTimeout
    , ociPoolGetNoWait
    , ociPoolSetNoWait
    , ociPoolGetBusyCount
    , ociPoolGetOpenedCount
    , ociPoolGetMin
    , ociPoolGetMax
    , ociPoolGetIncrement
    , ociPoolGetStatementCacheSize
    , ociPoolSetStatementCacheSize
    ) where

import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci
import           Database.Ocilib.Enums
import           Database.Ocilib.Internal

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- Oracle Pools

type Pool = Ptr OCI_Pool

-- | Create an Oracle pool of connections or sessions.
ociPoolCreate :: String -> String -> String -> PoolType -> SessionMode -> CUInt -> CUInt -> CUInt -> IO (Maybe Pool)
ociPoolCreate db user pwd pType mode minCon maxCon incrCon = do
    let pt = fromIntegral $ fromEnum pType
        m  = fromIntegral $ fromEnum mode
    withCString db (\d ->
        withCString user (\u ->
            withCString pwd (\p ->
                fmap toMaybePtr [C.exp| OCI_Pool* { OCI_PoolCreate( $(char *d)
                                                                  , $(char *u)
                                                                  , $(char *p)
                                                                  , $(unsigned int pt)
                                                                  , $(unsigned int m)
                                                                  , $(unsigned int minCon)
                                                                  , $(unsigned int maxCon)
                                                                  , $(unsigned int incrCon)) }|]
                )
            )
        )

-- | Destroy a pool object.
ociPoolFree :: Ptr OCI_Pool -> IO Bool
ociPoolFree p = fmap toBool [C.exp| int { OCI_PoolFree($(OCI_Pool *p))} |]

-- | Get a connection from the pool.
ociPoolGetConnection :: Ptr OCI_Pool -> String -> IO (Maybe (Ptr OCI_Connection))
ociPoolGetConnection p tag =
    withCString tag (\t ->
        fmap toMaybePtr [C.exp| OCI_Connection* { OCI_PoolGetConnection($(OCI_Pool *p), $(char *t)) } |]
    )

-- | Get the idle timeout for connections/sessions in the pool.
ociPoolGetTimeout :: Ptr OCI_Pool -> IO CUInt
ociPoolGetTimeout p = [C.exp| unsigned int { OCI_PoolGetTimeout($(OCI_Pool *p))} |]

-- | Set the connections/sessions idle timeout.
ociPoolSetTimeout :: Ptr OCI_Pool -> CUInt -> IO Bool
ociPoolSetTimeout p t = fmap toBool [C.exp| int { OCI_PoolSetTimeout($(OCI_Pool *p), $(unsigned int t)) } |]

-- | Get the waiting mode used when no more connections/sessions are available from the pool.
ociPoolGetNoWait :: Ptr OCI_Pool -> IO Bool
ociPoolGetNoWait p = fmap toBool [C.exp| int { OCI_PoolGetNoWait($(OCI_Pool *p)) } |]

-- | Set the waiting mode used when no more connections/sessions are available from the pool.
ociPoolSetNoWait :: Ptr OCI_Pool -> Bool -> IO Bool
ociPoolSetNoWait p value = do
    let v = fromIntegral $ fromEnum value
    fmap toBool [C.exp| int { OCI_PoolSetNoWait($(OCI_Pool *p), $(int v)) }|]

-- | Return the current number of busy connections/sessions.
ociPoolGetBusyCount :: Ptr OCI_Pool -> IO CUInt
ociPoolGetBusyCount p = [C.exp| unsigned int { OCI_PoolGetBusyCount($(OCI_Pool *p)) } |]

-- | Return the current number of opened connections/sessions.
ociPoolGetOpenedCount :: Ptr OCI_Pool -> IO CUInt
ociPoolGetOpenedCount p = [C.exp| unsigned int { OCI_PoolGetOpenedCount($(OCI_Pool *p)) } |]

-- | Return the minimum number of connections/sessions that can be opened to the database.
ociPoolGetMin :: Ptr OCI_Pool -> IO CUInt
ociPoolGetMin p = [C.exp| unsigned int { OCI_PoolGetMin($(OCI_Pool *p)) } |]

-- | Return the maximum number of connections/sessions that can be opened to the database.
ociPoolGetMax :: Ptr OCI_Pool -> IO CUInt
ociPoolGetMax p = [C.exp| unsigned int { OCI_PoolGetMax($(OCI_Pool *p)) } |]

-- | Return the increment for connections/sessions to be opened to the database when the pool is not full.
ociPoolGetIncrement :: Ptr OCI_Pool -> IO CUInt
ociPoolGetIncrement p = [C.exp| unsigned int { OCI_PoolGetIncrement($(OCI_Pool *p)) }|]

-- | Return the maximum number of statements to keep in the pool statement cache.
ociPoolGetStatementCacheSize :: Ptr OCI_Pool -> IO CUInt
ociPoolGetStatementCacheSize p = [C.exp| unsigned int { OCI_PoolGetStatementCacheSize($(OCI_Pool *p)) } |]

-- | Set the maximum number of statements to keep in the pool statement cache.
ociPoolSetStatementCacheSize :: Ptr OCI_Pool -> CUInt -> IO Bool
ociPoolSetStatementCacheSize p value = fmap toBool [C.exp| int { OCI_PoolSetStatementCacheSize($(OCI_Pool *p), $(unsigned int value)) } |]
