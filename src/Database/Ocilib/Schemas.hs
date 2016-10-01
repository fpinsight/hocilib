{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Schemas
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Schemas where

import           Data.ByteString
import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci
import           Database.Ocilib.Enums
import           Database.Ocilib.Internal

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- Describing Schema Meta data and Objects

-- | Retrieve the available type info information.
ociTypeInfoGet :: Ptr OCI_Connection -> ByteString -> TypeInfoType -> IO (Maybe (Ptr OCI_TypeInfo))
ociTypeInfoGet c name typ = do
    let typ' = fromIntegral $ fromEnum typ
    useAsCString name (\name' ->
            fmap toMaybePtr [C.exp| OCI_TypeInfo* { OCI_TypeInfoGet($(OCI_Connection *c), $(char *name'), $(unsigned int typ')) } |]
        )

-- | Return the type of the type info object.
ociTypeInfoGetType :: Ptr OCI_TypeInfo -> IO CUInt
ociTypeInfoGetType t = [C.exp| unsigned int { OCI_TypeInfoGetType($(OCI_TypeInfo *t)) } |]

-- | Retrieve connection handle from the type info handle.
ociTypeInfoGetConnection :: Ptr OCI_TypeInfo -> IO (Ptr OCI_Connection)
ociTypeInfoGetConnection t = [C.exp| OCI_Connection* { OCI_TypeInfoGetConnection($(OCI_TypeInfo *t)) } |]

-- | Free a type info object.
ociTypeInfoFree :: Ptr OCI_TypeInfo -> IO Bool
ociTypeInfoFree t = fmap toBool [C.exp| int { OCI_TypeInfoFree($(OCI_TypeInfo *t)) } |]

-- | Return the number of columns of a table/view/object.
ociTypeInfoGetColumnCount :: Ptr OCI_TypeInfo -> IO CUInt
ociTypeInfoGetColumnCount t = [C.exp| unsigned int { OCI_TypeInfoGetColumnCount($(OCI_TypeInfo *t)) } |]

-- | Return the column object handle at the given index in the table.
ociTypeInfoGetColumn :: Ptr OCI_TypeInfo -> CUInt -> IO (Ptr OCI_Column)
ociTypeInfoGetColumn t i = [C.exp| OCI_Column* { OCI_TypeInfoGetColumn($(OCI_TypeInfo *t), $(unsigned int i))} |]

-- | Return the name described by the type info object.
ociTypeInfoGetName :: Ptr OCI_TypeInfo -> IO ByteString
ociTypeInfoGetName t = do
    s <- [C.exp| const char* { OCI_TypeInfoGetName($(OCI_TypeInfo *t)) } |]
    packCString s
