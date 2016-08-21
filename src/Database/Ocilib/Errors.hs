{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Errors
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Errors
    ( ociGetLastError
    , ociErrorGetString
    , ociErrorGetType
    , ociErrorGetOCICode
    , ociErrorGetInternalCode
    , ociErrorGetConnection
    , ociErrorGetStatement
    , ociErrorGetRow
    ) where

import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci
import           Database.Ocilib.Enums
import           Database.Ocilib.Internal

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- error handling

-- | Retrieve the last error or warning occurred within the last OCILIB call.
ociGetLastError :: IO (Maybe (Ptr OCI_Error))
ociGetLastError = fmap toMaybePtr [C.exp| OCI_Error* { OCI_GetLastError() } |]

-- | Retrieve error message from error handle.
ociErrorGetString :: Ptr OCI_Error -> IO String
ociErrorGetString err = do
    s <- [C.exp| const char* { OCI_ErrorGetString($(OCI_Error *err)) } |]
    peekCString s

-- | Retrieve the type of error from error handle.
ociErrorGetType :: Ptr OCI_Error -> IO ErrorType
ociErrorGetType err = fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_ErrorGetType($(OCI_Error *err)) } |]

-- | Retrieve Oracle Error code from error handle.
ociErrorGetOCICode :: Ptr OCI_Error -> IO ErrorCode
ociErrorGetOCICode err = fmap (toEnum . fromIntegral) [C.exp| int { OCI_ErrorGetOCICode($(OCI_Error *err)) } |]

-- | Retrieve Internal Error code from error handle.
ociErrorGetInternalCode :: Ptr OCI_Error -> IO CInt
ociErrorGetInternalCode err = [C.exp| int { OCI_ErrorGetInternalCode($(OCI_Error *err)) } |]

-- | Retrieve connection handle within the error occurred.
ociErrorGetConnection :: Ptr OCI_Error -> IO (Ptr OCI_Connection)
ociErrorGetConnection err = [C.exp| OCI_Connection* { OCI_ErrorGetConnection($(OCI_Error *err)) }|]

-- | Retrieve statement handle within the error occurred.
ociErrorGetStatement :: Ptr OCI_Error -> IO (Maybe (Ptr OCI_Statement))
ociErrorGetStatement err = fmap toMaybePtr [C.exp| OCI_Statement* { OCI_ErrorGetStatement($(OCI_Error *err)) }|]

-- | Return the row index which caused an error during statement execution.
ociErrorGetRow :: Ptr OCI_Error -> IO CUInt
ociErrorGetRow err = [C.exp| unsigned int {OCI_ErrorGetRow($(OCI_Error *err)) }|] -- FIXME should be Maybe CUInt
