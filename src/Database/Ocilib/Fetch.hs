{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Fetch
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Fetch
    ( ociGetResultset
    , ociReleaseResultsets
    , ociFetchNext
    , ociFetchPrev
    , ociFetchFirst
    , ociFetchLast
    , ociFetchSeek
    , ociGetRowCount
    , ociGetCurrentRow
    , ociGetColumnCount
    , ociGetColumn
    , ociGetColumn2
    , ociGetColumnIndex
    , ociColumnGetName
    , ociColumnGetType
    , ociColumnGetSQLType
    , ociColumnGetSize
    , ociColumnGetScale
    , ociColumnGetPrecision
    , ociColumnGetFractionalPrecision
    , ociColumnGetLeadingPrecision
    , ociColumnGetNullable
    , ociColumnGetCharUsed
    , ociColumnGetPropertyFlags
    , ociColumnGetTypeInfo
    , ociColumnGetSubType
    , ociSetStructNumericType
    , ociSetStructNumericType2
    --, ociGetStruct
    , ociGetShort
    , ociGetShort2
    , ociGetUnsignedShort
    , ociGetUnsignedShort2
    , ociGetInt
    , ociGetInt2
    , ociGetUnsignedInt
    , ociGetUnsignedInt2
    --, ociGetBigInt
    --, ociGetBigInt2
    --, ociGetUnsignedBigInt
    --, ociGetUnsignedBigInt2
    , ociGetString
    , ociGetString2
    --, ociGetRaw
    --, ociGetRaw2
    , ociGetDouble
    , ociGetDouble2
    , ociGetFloat
    , ociGetFloat2
    , ociGetDate
    , ociGetDate2
    , ociGetTimestamp
    , ociGetTimestamp2
    , ociGetInterval
    , ociGetInterval2
    , ociGetStatement
    , ociGetStatement2
    -- , ociGetLob
    -- , ociGetLob2
    -- , ociGetFile
    -- , ociGetFile2
    -- , ociGetObject
    -- , ociGetObject2
    -- , ociGetColl
    -- , ociGetColl2
    -- , ociGetRef
    -- , ociGetRef2
    -- , ociGetLong
    -- , ociGetLong2
    , ociIsNull
    , ociIsNull2
    , ociResultsetGetStatement
    , ociGetDataLength
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

-- | Retrieve the resultset handle from an executed statement.
ociGetResultset :: Ptr OCI_Statement -> IO (Maybe (Ptr OCI_Resultset))
ociGetResultset s = fmap toMaybePtr [C.exp| OCI_Resultset* { OCI_GetResultset($(OCI_Statement *s)) } |]

-- | Free the statement resultsets.
ociReleaseResultsets :: Ptr OCI_Statement -> IO Bool
ociReleaseResultsets s = fmap toBool [C.exp| int { OCI_ReleaseResultsets($(OCI_Statement *s)) } |]

-- | Fetch the next row of the resultset.
ociFetchNext :: Ptr OCI_Resultset -> IO Bool
ociFetchNext rs = fmap toBool [C.exp| int { OCI_FetchNext($(OCI_Resultset *rs)) } |]

-- | Fetch the previous row of the resultset.
ociFetchPrev :: Ptr OCI_Resultset -> IO Bool
ociFetchPrev rs = fmap toBool [C.exp| int { OCI_FetchPrev($(OCI_Resultset *rs))}|]

-- | Fetch the first row of the resultset.
ociFetchFirst :: Ptr OCI_Resultset -> IO Bool
ociFetchFirst rs = fmap toBool [C.exp| int { OCI_FetchFirst($(OCI_Resultset *rs)) } |]

-- | Fetch the last row of the resultset.
ociFetchLast :: Ptr OCI_Resultset -> IO Bool
ociFetchLast rs = fmap toBool [C.exp| int {OCI_FetchLast($(OCI_Resultset *rs)) }|]

-- | Custom Fetch of the resultset.
ociFetchSeek :: Ptr OCI_Resultset -> StatementFetchDirection -> CUInt -> IO Bool
ociFetchSeek rs mode offset = do
    let mode' = fromIntegral $ fromEnum mode
    fmap toBool [C.exp| int { OCI_FetchSeek($(OCI_Resultset *rs)
                                          , $(unsigned int mode')
                                          , $(unsigned int offset)) } |]

-- | Retrieve the number of rows fetched so far.
ociGetRowCount :: Ptr OCI_Resultset -> IO CUInt
ociGetRowCount rs = [C.exp| unsigned int { OCI_GetRowCount($(OCI_Resultset *rs)) } |]

-- | Retrieve the current row number.
ociGetCurrentRow :: Ptr OCI_Resultset -> IO CUInt
ociGetCurrentRow rs = [C.exp| unsigned int { OCI_GetCurrentRow($(OCI_Resultset *rs)) }|]

-- | Return the number of columns in the resultset.
ociGetColumnCount :: Ptr OCI_Resultset -> IO CUInt
ociGetColumnCount rs = [C.exp| unsigned int { OCI_GetColumnCount($(OCI_Resultset *rs)) } |]

-- | Return the column object handle at the given index in the resultset.
ociGetColumn :: Ptr OCI_Resultset -> CUInt -> IO (Ptr OCI_Column)
ociGetColumn rs i = [C.exp| OCI_Column* { OCI_GetColumn($(OCI_Resultset *rs), $(unsigned int i)) } |]

-- | Return the column object handle from its name in the resultset.
ociGetColumn2 :: Ptr OCI_Resultset -> String -> IO (Ptr OCI_Column)
ociGetColumn2 rs name =
    withCString name (\n ->
        [C.exp| OCI_Column* { OCI_GetColumn2($(OCI_Resultset *rs), $(char *n)) } |]
    )

-- | Return the index of the column in the result from its name.
ociGetColumnIndex :: Ptr OCI_Resultset -> String -> IO CUInt
ociGetColumnIndex rs name =
    withCString name (\n ->
        [C.exp| unsigned int { OCI_GetColumnIndex($(OCI_Resultset *rs), $(char *n)) } |]
    )

-- | Return the name of the given column.
ociColumnGetName :: Ptr OCI_Column -> IO String
ociColumnGetName c = do
    s <- [C.exp| const char* { OCI_ColumnGetName($(OCI_Column *c)) } |]
    peekCString s

-- | Return the type of the given column.
ociColumnGetType :: Ptr OCI_Column -> IO ColumnType
ociColumnGetType c = do
    ct <- [C.exp| int { OCI_ColumnGetType($(OCI_Column *c)) } |]
    return $ toEnum $ fromIntegral ct

{-
-- | Return the charset form of the given column.
-- unsigned int OCI_ColumnGetCharsetForm (OCI_Column *col)
-}

-- | Return the Oracle SQL type name of the column data type.
ociColumnGetSQLType :: Ptr OCI_Column -> IO String
ociColumnGetSQLType c = do
    s <- [C.exp| const char* { OCI_ColumnGetSQLType($(OCI_Column *c)) } |]
    peekCString s

{-
-- | Return the Oracle SQL Full name including precision and size of the column data type.
-- unsigned int OCI_ColumnGetFullSQLType (OCI_Column *col, otext *buffer, unsigned int len)
-}

-- | Return the size of the column.
ociColumnGetSize :: Ptr OCI_Column -> IO CUInt
ociColumnGetSize c = [C.exp| unsigned int { OCI_ColumnGetSize($(OCI_Column *c))} |]

-- | Return the scale of the column for numeric columns.
ociColumnGetScale :: Ptr OCI_Column -> IO CInt
ociColumnGetScale c = [C.exp| int { OCI_ColumnGetScale($(OCI_Column *c))} |]

-- | Return the precision of the column for numeric columns.
ociColumnGetPrecision :: Ptr OCI_Column -> IO CInt
ociColumnGetPrecision c = [C.exp| int { OCI_ColumnGetPrecision($(OCI_Column *c)) } |]

-- | Return the fractional precision of the column for timestamp and interval columns.
ociColumnGetFractionalPrecision :: Ptr OCI_Column -> IO CInt
ociColumnGetFractionalPrecision c = [C.exp| int { OCI_ColumnGetFractionalPrecision($(OCI_Column *c)) } |]

-- | Return the leading precision of the column for interval columns.
ociColumnGetLeadingPrecision :: Ptr OCI_Column -> IO CInt
ociColumnGetLeadingPrecision c = [C.exp| int { OCI_ColumnGetLeadingPrecision($(OCI_Column *c))} |]

-- | Return the nullable attribute of the column.
ociColumnGetNullable :: Ptr OCI_Column -> IO Bool
ociColumnGetNullable c = fmap toBool [C.exp| int { OCI_ColumnGetNullable($(OCI_Column *c)) } |]

-- | Return TRUE if the length of the column is character-length or FALSE if it is byte-length.
ociColumnGetCharUsed :: Ptr OCI_Column -> IO Bool
ociColumnGetCharUsed c = fmap toBool [C.exp| int { OCI_ColumnGetCharUsed($(OCI_Column *c)) } |]

-- | Return the column property flags.
ociColumnGetPropertyFlags :: Ptr OCI_Column -> IO CUInt
ociColumnGetPropertyFlags c = [C.exp| unsigned int { OCI_ColumnGetPropertyFlags($(OCI_Column *c)) }|]

-- | Return the type information object associated to the column.
ociColumnGetTypeInfo :: Ptr OCI_Column -> IO (Ptr OCI_TypeInfo)
ociColumnGetTypeInfo c = [C.exp| OCI_TypeInfo* { OCI_ColumnGetTypeInfo($(OCI_Column *c)) } |]

-- | Return the OCILIB object subtype of a column.
ociColumnGetSubType :: Ptr OCI_Column -> IO CUInt
ociColumnGetSubType c = [C.exp| unsigned int { OCI_ColumnGetSubType($(OCI_Column *c)) } |]

-- | set the numeric data type of the given structure member (identified from position in the resultset) to retrieve when calling OCI_GetStruct()
ociSetStructNumericType :: Ptr OCI_Resultset -> CUInt -> CUInt -> IO Bool -- FIXME use enum for the type
ociSetStructNumericType rs i t = fmap toBool [C.exp| int { OCI_SetStructNumericType($(OCI_Resultset *rs), $(unsigned int i), $(unsigned int t)) } |]

-- | set the numeric data type of the given structure member (identified from column name in the resultset) to retrieve when calling OCI_GetStruct()
ociSetStructNumericType2 :: Ptr OCI_Resultset -> String -> CUInt -> IO Bool -- FIXME use enum for the type
ociSetStructNumericType2 rs name t =
    withCString name (\n ->
        fmap toBool [C.exp| int { OCI_SetStructNumericType2($(OCI_Resultset *rs), $(char *n), $(unsigned int t))} |]
    )

{-
-- | Return the row columns values into a single structure.
-- boolean OCI_GetStruct (OCI_Resultset *rs, void *row_struct, void *row_struct_ind)
-}

-- | Return the current short value of the column at the given index in the resultset.
ociGetShort :: Ptr OCI_Resultset -> CUInt -> IO CShort
ociGetShort rs i = [C.exp| short { OCI_GetShort($(OCI_Resultset *rs), $(unsigned int i)) } |]

-- | Return the current short value of the column from its name in the resultset.
ociGetShort2 :: Ptr OCI_Resultset -> String -> IO CShort
ociGetShort2 rs name =
    withCString name (\n ->
        [C.exp| short { OCI_GetShort2($(OCI_Resultset *rs), $(char *n)) } |]
    )

-- | Return the current unsigned short value of the column at the given index in the resultset.
ociGetUnsignedShort :: Ptr OCI_Resultset -> CUInt -> IO CUShort
ociGetUnsignedShort rs i = [C.exp| unsigned short { OCI_GetUnsignedShort($(OCI_Resultset *rs), $(unsigned int i)) } |]

-- | Return the current unsigned short value of the column from its name in the resultset.
ociGetUnsignedShort2 :: Ptr OCI_Resultset -> String -> IO CUShort
ociGetUnsignedShort2 rs name =
    withCString name (\n ->
        [C.exp| unsigned short { OCI_GetUnsignedShort2($(OCI_Resultset *rs), $(char *n)) } |]
    )

-- Return the current integer value of the column at the given index in the resultset.
ociGetInt :: Ptr OCI_Resultset -> CUInt -> IO CInt
ociGetInt rs i = [C.exp| int { OCI_GetInt($(OCI_Resultset *rs), $(unsigned int i)) } |]

-- | Return the current integer value of the column from its name in the resultset.
ociGetInt2 :: Ptr OCI_Resultset -> String -> IO CInt
ociGetInt2 rs name =
    withCString name (\n ->
        [C.exp| int { OCI_GetInt2($(OCI_Resultset *rs), $(char *n)) } |]
    )

-- | Return the current unsigned integer value of the column at the given index in the resultset.
ociGetUnsignedInt :: Ptr OCI_Resultset -> CUInt -> IO CUInt
ociGetUnsignedInt rs i = [C.exp| unsigned int { OCI_GetUnsignedInt($(OCI_Resultset *rs), $(unsigned int i)) } |]

-- | Return the current unsigned integer value of the column from its name in the resultset.
ociGetUnsignedInt2 :: Ptr OCI_Resultset -> String -> IO CUInt
ociGetUnsignedInt2 rs name =
    withCString name (\n ->
        [C.exp| unsigned int { OCI_GetUnsignedInt2($(OCI_Resultset *rs), $(char *n))} |]
    )

{-
-- | Return the current big integer value of the column at the given index in the resultset.
-- big_int OCI_GetBigInt (OCI_Resultset *rs, unsigned int index)
-- ociGetBigInt :: Ptr OCI_resultset -> CUInt -> IO

-- | Return the current big integer value of the column from its name in the resultset.
-- big_int OCI_GetBigInt2 (OCI_Resultset *rs, const otext *name)

-- | Return the current unsigned big integer value of the column at the given index in the resultset.
-- big_uint OCI_GetUnsignedBigInt (OCI_Resultset *rs, unsigned int index)

-- | Return the current unsigned big integer value of the column from its name in the resultset.
-- big_uint OCI_GetUnsignedBigInt2 (OCI_Resultset *rs, const otext *name)
-}

-- | Return the current string value of the column at the given index in the resultset.
ociGetString :: Ptr OCI_Resultset -> CUInt -> IO String
ociGetString rs i = do
    s <- [C.exp| const char* { OCI_GetString($(OCI_Resultset *rs), $(unsigned int i)) }|]
    peekCString s -- FIXME release CString ?

-- | Return the current string value of the column from its name in the resultset.
ociGetString2 :: Ptr OCI_Resultset -> String -> IO String
ociGetString2 rs name =
    withCString name (\n -> do
        s <- [C.exp| const char* { OCI_GetString2($(OCI_Resultset *rs), $(char *n)) } |]
        peekCString s
    )

{-
-- | Copy the current raw value of the column at the given index into the specified buffer.
-- unsigned int OCI_GetRaw (OCI_Resultset *rs, unsigned int index, void *buffer, unsigned int len)

-- | Copy the current raw value of the column from its name into the specified buffer.
-- unsigned int OCI_GetRaw2 (OCI_Resultset *rs, const otext *name, void *buffer, unsigned int len)
-}

-- | Return the current double value of the column at the given index in the resultset.
ociGetDouble :: Ptr OCI_Resultset -> CUInt -> IO CDouble
ociGetDouble rs i = [C.exp| double { OCI_GetDouble($(OCI_Resultset *rs), $(unsigned int i))} |]

-- | Return the current double value of the column from its name in the resultset.
ociGetDouble2 :: Ptr OCI_Resultset -> String -> IO CDouble
ociGetDouble2 rs name =
    withCString name (\n ->
        [C.exp| double { OCI_GetDouble2($(OCI_Resultset *rs), $(char *n))} |]
    )

-- | Return the current float value of the column at the given index in the resultset.
ociGetFloat :: Ptr OCI_Resultset -> CUInt -> IO CFloat
ociGetFloat rs i = [C.exp| float { OCI_GetFloat($(OCI_Resultset *rs), $(unsigned int i)) } |]

-- | Return the current float value of the column from its name in the resultset.
ociGetFloat2 :: Ptr OCI_Resultset -> String -> IO CFloat
ociGetFloat2 rs name =
    withCString name (\n ->
        [C.exp| float { OCI_GetFloat2($(OCI_Resultset *rs), $(char *n))} |]
    )

-- | Return the current date value of the column at the given index in the resultset.
ociGetDate :: Ptr OCI_Resultset -> CUInt -> IO (Ptr OCI_Date)
ociGetDate rs i = [C.exp| OCI_Date* { OCI_GetDate($(OCI_Resultset *rs), $(unsigned int i))} |]

-- | Return the current date value of the column from its name in the resultset.
ociGetDate2 :: Ptr OCI_Resultset -> String -> IO (Ptr OCI_Date)
ociGetDate2 rs name =
    withCString name (\n ->
        [C.exp| OCI_Date* { OCI_GetDate2($(OCI_Resultset *rs), $(char *n))} |]
    )

-- | Return the current timestamp value of the column at the given index in the resultset.
ociGetTimestamp :: Ptr OCI_Resultset -> CUInt -> IO (Ptr OCI_Timestamp)
ociGetTimestamp rs i = [C.exp| OCI_Timestamp* { OCI_GetTimestamp($(OCI_Resultset *rs), $(unsigned int i)) } |]

-- | Return the current timestamp value of the column from its name in the resultset.
ociGetTimestamp2 :: Ptr OCI_Resultset -> String -> IO (Ptr OCI_Timestamp)
ociGetTimestamp2 rs name =
    withCString name (\n ->
        [C.exp| OCI_Timestamp* { OCI_GetTimestamp2($(OCI_Resultset *rs), $(char *n)) } |]
    )

-- | Return the current interval value of the column at the given index in the resultset.
ociGetInterval :: Ptr OCI_Resultset -> CUInt -> IO (Ptr OCI_Interval)
ociGetInterval rs i = [C.exp| OCI_Interval* { OCI_GetInterval($(OCI_Resultset *rs), $(unsigned int i))  } |]

-- | Return the current interval value of the column from its name in the resultset.
ociGetInterval2 :: Ptr OCI_Resultset -> String -> IO (Ptr OCI_Interval)
ociGetInterval2 rs name =
    withCString name (\n ->
        [C.exp| OCI_Interval* { OCI_GetInterval2($(OCI_Resultset *rs), $(char *n)) } |]
    )

-- | Return the current cursor value (Nested table) of the column at the given index in the resultset.
ociGetStatement :: Ptr OCI_Resultset -> CUInt -> IO (Ptr OCI_Statement)
ociGetStatement rs i = [C.exp| OCI_Statement* { OCI_GetStatement($(OCI_Resultset *rs), $(unsigned int i)) }|]

-- | Return the current cursor value of the column from its name in the resultset.
ociGetStatement2 :: Ptr OCI_Resultset -> String -> IO (Ptr OCI_Statement)
ociGetStatement2 rs name =
    withCString name (\n ->
        [C.exp| OCI_Statement* { OCI_GetStatement2($(OCI_Resultset *rs), $(char *n)) } |]
    )

{-
-- | Return the current lob value of the column at the given index in the resultset.
-- OCI_Lob *OCI_GetLob (OCI_Resultset *rs, unsigned int index)

-- | Return the current lob value of the column from its name in the resultset.
-- OCI_Lob *OCI_GetLob2 (OCI_Resultset *rs, const otext *name)

-- | Return the current File value of the column at the given index in the resultset.
-- OCI_File *OCI_GetFile (OCI_Resultset *rs, unsigned int index)

-- | Return the current File value of the column from its name in the resultset.
-- OCI_File *OCI_GetFile2 (OCI_Resultset *rs, const otext *name)

-- | Return the current Object value of the column at the given index in the resultset.
-- OCI_Object *OCI_GetObject (OCI_Resultset *rs, unsigned int index)

-- | Return the current Object value of the column from its name in the resultset.
-- OCI_Object *OCI_GetObject2 (OCI_Resultset *rs, const otext *name)

-- | Return the current Collection value of the column at the given index in the resultset.
-- OCI_Coll *OCI_GetColl (OCI_Resultset *rs, unsigned int index)

-- | Return the current Collection value of the column from its name in the resultset.
-- OCI_Coll *OCI_GetColl2 (OCI_Resultset *rs, const otext *name)

-- | Return the current Ref value of the column at the given index in the resultset.
-- OCI_Ref *OCI_GetRef (OCI_Resultset *rs, unsigned int index)

-- | Return the current Ref value of the column from its name in the resultset.
-- OCI_Ref *OCI_GetRef2 (OCI_Resultset *rs, const otext *name)

-- | Return the current Long value of the column at the given index in the resultset.
-- OCI_Long *OCI_GetLong (OCI_Resultset *rs, unsigned int index)

-- | Return the current Long value of the column from its name in the resultset.
-- OCI_Long *OCI_GetLong2 (OCI_Resultset *rs, const otext *name)
-}

-- | Check if the current row value is null for the column at the given index in the resultset.
-- boolean OCI_IsNull (OCI_Resultset *rs, unsigned int index)
ociIsNull :: Ptr OCI_Resultset -> CUInt -> IO Bool
ociIsNull rs i = fmap toBool [C.exp| int { OCI_IsNull($(OCI_Resultset *rs), $(unsigned int i))}|]

-- | Check if the current row value is null for the column of the given name in the resultset.
-- boolean OCI_IsNull2 (OCI_Resultset *rs, const otext *name)
ociIsNull2 :: Ptr OCI_Resultset -> String -> IO Bool
ociIsNull2 rs name =
    withCString name (\n ->
        fmap toBool [C.exp| int { OCI_IsNull2($(OCI_Resultset *rs), $(char *n)) } |]
    )

-- | Return the statement handle associated with a resultset handle.
-- OCI_Statement *OCI_ResultsetGetStatement (OCI_Resultset *rs)
ociResultsetGetStatement :: Ptr OCI_Resultset -> IO (Ptr OCI_Statement)
ociResultsetGetStatement rs = [C.exp| OCI_Statement* { OCI_ResultsetGetStatement($(OCI_Resultset* rs)) } |]

-- | Return the current row data length of the column at the given index in the resultset.
-- unsigned int OCI_GetDataLength (OCI_Resultset *rs, unsigned int index)
ociGetDataLength :: Ptr OCI_Resultset -> CUInt -> IO CUInt
ociGetDataLength rs i = [C.exp| unsigned int { OCI_GetDataLength($(OCI_Resultset *rs), $(unsigned int i)) } |]
