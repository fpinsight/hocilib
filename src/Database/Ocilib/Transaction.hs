{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Transaction
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Transaction where

import           Data.Monoid ((<>))
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- | Commit current pending changes.
ociCommit :: Ptr OCI_Connection -> IO Bool
ociCommit c = fmap toBool [C.exp| int { OCI_Commit($(OCI_Connection *c)) }|]

-- | Cancel current pending changes.
ociRollback :: Ptr OCI_Connection -> IO Bool
ociRollback c = fmap toBool [C.exp| int { OCI_Rollback($(OCI_Connection *c)) } |]

-- | Enable / disable auto commit mode.
ociSetAutoCommit :: Ptr OCI_Connection -> Bool -> IO Bool
ociSetAutoCommit c b = do
    let b' = fromBool b
    fmap toBool [C.exp| int { OCI_SetAutoCommit($(OCI_Connection *c), $(int b'))} |]

-- | Get current auto commit mode status.
ociGetAutoCommit :: Ptr OCI_Connection -> IO Bool
ociGetAutoCommit c = fmap toBool [C.exp| int { OCI_GetAutoCommit($(OCI_Connection *c)) } |]

{-
-- | Create a new global transaction or a serializable/read-only local transaction.
-- OCI_Transaction *OCI_TransactionCreate (OCI_Connection *con, unsigned int timeout, unsigned int mode, OCI_XID *pxid)

-- | Free current transaction.
-- boolean OCI_TransactionFree (OCI_Transaction *trans)

-- | Start global transaction.
-- boolean OCI_TransactionStart (OCI_Transaction *trans)

-- | Stop current global transaction.
-- boolean OCI_TransactionStop (OCI_Transaction *trans)

-- | Resume a stopped global transaction.
-- boolean OCI_TransactionResume (OCI_Transaction *trans)

-- | Prepare a global transaction validation. More...
-- boolean OCI_TransactionPrepare (OCI_Transaction *trans)

-- | Cancel the prepared global transaction validation.
-- boolean OCI_TransactionForget (OCI_Transaction *trans)

-- | Return global transaction mode.
-- unsigned int OCI_TransactionGetMode (OCI_Transaction *trans)

-- | Return global transaction Timeout.
-- unsigned int OCI_TransactionGetTimeout (OCI_Transaction *trans)

-}
