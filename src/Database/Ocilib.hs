{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
-- FFI wrapper to Ocilib

module Database.Ocilib
    (
    -- * Initializing library
      ociInitialize
    , ociCleanup
    , ociGetOCICompileVersion
    , ociGetOCIRuntimeVersion
    , ociGetImportMode
    , ociGetCharset
    , ociGetAllocatedBytes
    , ociEnableWarnings
    , ociSetErrorHandler

    -- * Describing Schema Meta data and Objects
    , ociTypeInfoGet
    , ociTypeInfoGetType
    , ociTypeInfoGetConnection
    , ociTypeInfoFree
    , ociTypeInfoGetColumnCount
    , ociTypeInfoGetColumn
    , ociTypeInfoGetName

    -- * Types
    , ImportMode(..)
    , CharsetMode(..)
    , ErrorType(..)
    , ErrorCode(..)
    , AllocatedBytesType(..)
    , BindMode(..)
    , ColumnType(..)
    , DataTypeCode(..)
    , StatementType(..)
    , EnvironmentMode(..)
    , SessionMode(..)
    , ChangeNotificationType(..)
    , EventNotificationType(..)
    , EventObjectNotificationType(..)
    , DatabaseStartupMode(..)
    , DatabaseStartupFlag(..)
    , DatabaseShutdownMode(..)
    , DatabaseShutdownFlag(..)
    , CharsetFormType(..)
    , StatementFetchMode(..)
    , StatementFetchDirection(..)
    , BindAllocationMode(..)
    , BindDirectionMode(..)
    , ColumnPropertyFlag(..)
    , TimestampType(..)
    , IntervalType(..)
    , LongType(..)
    , LobType(..)
    , LobOpeningMode(..)
    , FileType(..)
    , LobBrowsingMode(..)
    , TypeInfoType(..)
    , ObjectType(..)
    , CollectionType(..)
    , PoolType(..)
    , AQMessageState(..)
    , AQSequenceDeviation(..)
    , OCI_Error
    ) where

import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.BitMask
-- import           Database.Ocilib.Bindings
import           Database.Ocilib.Oci
import           Database.Ocilib.Enums
-- import           Database.Ocilib.Errors
-- import           Database.Ocilib.Pool
-- import           Database.Ocilib.Fetch
import           Database.Ocilib.Schemas
-- import           Database.Ocilib.Statement
-- import           Database.Ocilib.Connection
-- import           Database.Ocilib.Collections
-- import           Database.Ocilib.Transaction
-- import           Database.Ocilib.DateTime
-- import           Database.Ocilib.Timestamp

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

type ErrorHandler = Ptr OCI_Error -> IO ()

-- Initializing the library

-- | Initialize the library.
ociInitialize :: Maybe ErrorHandler -> Maybe FilePath -> [EnvironmentMode] -> IO Bool
ociInitialize Nothing fp mode = do
    let m = fromIntegral $ toBitMask mode
    case fp of
        Nothing ->
            fmap toBool[C.exp| int { OCI_Initialize(NULL, NULL, $(unsigned int m)) } |]
        Just a ->
            withCString a (\cs ->
                fmap toBool [C.exp| int { OCI_Initialize(NULL, $(char *cs), $(unsigned int m)) } |]
            )
ociInitialize (Just h) fp mode = do
    let m = fromIntegral $ toBitMask mode
    case fp of
        Nothing ->
            fmap toBool [C.exp| int { OCI_Initialize(NULL, NULL, $(unsigned int m)) } |]
        Just a ->
            withCString a (\cs -> do
                fmap toBool [C.exp| int { OCI_Initialize($fun:(void (*h)(OCI_Error*)), $(char *cs), $(unsigned int m)) } |]
            )

-- | Clean up all resources allocated by the library.
ociCleanup :: IO Bool
ociCleanup = fmap toBool [C.exp| int { OCI_Cleanup() } |]

-- | Return the version of OCI used for compilation.
ociGetOCICompileVersion :: IO CUInt
ociGetOCICompileVersion = [C.exp| unsigned int { OCI_GetOCICompileVersion() } |]

-- | Return the version of OCI used at runtime.
ociGetOCIRuntimeVersion :: IO CUInt
ociGetOCIRuntimeVersion = [C.exp| unsigned int { OCI_GetOCIRuntimeVersion() } |]

-- | Return the Oracle shared library import mode.
ociGetImportMode :: IO ImportMode
ociGetImportMode = fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_GetImportMode() } |]

-- | Return the OCILIB charset type.
ociGetCharset :: IO CharsetMode
ociGetCharset = fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_GetCharset() } |]

-- | Return the current number of bytes allocated internally in the library.
ociGetAllocatedBytes :: AllocatedBytesType -> IO CUInt
ociGetAllocatedBytes memType = do
    let abt = fromIntegral $ fromEnum memType
    [C.exp| unsigned int { OCI_GetAllocatedBytes($(unsigned int abt)) } |]

-- | Enable or disable Oracle warning notifications.
ociEnableWarnings :: Bool -> IO Bool
ociEnableWarnings v = do
    let vAsInt = fromIntegral $ fromEnum v
    fmap toBool [C.exp| int { OCI_EnableWarnings($(int vAsInt)) } |]

-- | Set the global error user handler.
-- boolean OCI_SetErrorHandler (POCI_ERROR handler)
ociSetErrorHandler :: (Ptr OCI_Error -> IO ()) -> IO Bool
ociSetErrorHandler h = fmap toBool [C.exp| int { OCI_SetErrorHandler($fun:(void (*h)(OCI_Error *))) } |]

{-
-- | Set the High availability (HA) user handler.
-- boolean OCI_SetHAHandler (POCI_HA_HANDLER handler)
--ociSetHAHandler :: (ptr  -> IO)
-}
