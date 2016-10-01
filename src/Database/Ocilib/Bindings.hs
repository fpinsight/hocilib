{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Bindings
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Bindings
    ( ociBindArraySetSize
    , ociBindArrayGetSize
    , ociAllowRebinding
    , ociIsRebindingAllowed
    , ociBindBoolean
    , ociBindShort
    , ociBindArrayOfShorts
    , ociBindUnsignedShort
    , ociBindArrayOfUnsignedShorts
    , ociBindInt
    , ociBindArrayOfInts
    , ociBindUnsignedInt
    , ociBindArrayOfUnsignedInts
    --, ociBindBigInt
    --, ociBindArrayOfBigInts
    --, ociBindUnsignedBigInts
    --, ociBindArrayOfUnsignedBigInts
    , ociBindString
    , ociBindArrayOfStrings
    --, ociBindRaw
    --, ociBindArrayOfRaws
    , ociBindDouble
    , ociBindArrayOfDoubles
    , ociBindFloat
    , ociBindArrayOfFloats
    , ociBindDate
    --, ociBindArrayOfDates
    , ociBindTimestamp
    --, ociBindArrayOfTimestamps
    , ociBindInterval
    --, ociBindArrayOfIntervals
    , ociBindLob
    --, ociBindArrayOfLobs
    , ociBindFile
    --, ociBindArrayOfFiles
    , ociBindObject
    --, ociBindArrayOfObjects
    , ociBindColl
    --, ociBindArrayOfColls
    , ociBindRef
    --, ociBindArrayOfRefs
    , ociBindStatement
    , ociBindLong
    , ociGetBatchError
    , ociGetBatchErrorCount
    , ociGetBindCount
    , ociGetBind
    , ociGetBind2
    , ociGetBindIndex
    , ociBindGetName
    , ociBindSetDirection
    , ociBindGetDirection
    , ociBindGetType
    --, ociBindGetSubtype
    , ociBindGetDataCount
    , ociBindGetData
    , ociBindGetStatement
    , ociBindSetDataSize
    , ociBindGetDataSize
    , ociBindSetDataSizeAtPos
    , ociBindGetDataSizeAtPos
    , ociBindSetNull
    , ociBindSetNullAtPos
    , ociBindSetNotNull
    , ociBindSetNotNullAtPos
    , ociBindIsNull
    , ociBindIsNullAtPos
    , ociBindSetCharsetForm
    ) where

import           Data.ByteString
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

-- Binding variables and arrays

-- | Set the input array size for bulk operations.
ociBindArraySetSize :: Ptr OCI_Statement -> CUInt -> IO Bool
ociBindArraySetSize st s =
    fmap toBool [C.exp| int { OCI_BindArraySetSize($(OCI_Statement *st), $(unsigned int s)) } |]

-- | Return the current input array size for bulk operations.
ociBindArrayGetSize :: Ptr OCI_Statement -> IO CUInt
ociBindArrayGetSize st = [C.exp| unsigned int { OCI_BindArrayGetSize($(OCI_Statement *st)) } |]

-- | Allow different host variables to be binded using the same bind name or position between executions of a prepared statement.
ociAllowRebinding :: Ptr OCI_Statement -> Bool -> IO Bool
ociAllowRebinding st value = do
    let v = fromIntegral $ fromEnum value
    fmap toBool [C.exp| int { OCI_AllowRebinding($(OCI_Statement *st), $(int v)) }|]

-- | Indicate if rebinding is allowed on the given statement.
ociIsRebindingAllowed :: Ptr OCI_Statement -> IO Bool
ociIsRebindingAllowed st =
    fmap toBool [C.exp| int { OCI_IsRebindingAllowed($(OCI_Statement *st)) } |]

-- | Bind a boolean variable (PL/SQL ONLY)
ociBindBoolean :: Ptr OCI_Statement -> ByteString -> Ptr CInt -> IO Bool
ociBindBoolean st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindBoolean($(OCI_Statement *st), $(const char *n'), $(int *d)) } |]
    )

-- | Bind an short variable.
ociBindShort :: Ptr OCI_Statement -> ByteString -> Ptr CShort -> IO Bool
ociBindShort st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindShort($(OCI_Statement *st), $(const char *n'), $(short *d)) } |]
    )

-- | Bind an array of shorts.
ociBindArrayOfShorts :: Ptr OCI_Statement -> ByteString -> Ptr CShort -> CUInt -> IO Bool
ociBindArrayOfShorts st n d nb = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindArrayOfShorts($(OCI_Statement *st)
                                                      , $(const char *n')
                                                      , $(short *d)
                                                      , $(unsigned int nb)) } |]
    )

-- | Bind an unsigned short variable.
ociBindUnsignedShort :: Ptr OCI_Statement -> ByteString -> Ptr CUShort -> IO Bool
ociBindUnsignedShort st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindUnsignedShort($(OCI_Statement *st)
                                                      , $(const char *n')
                                                      , $(unsigned short *d)) } |]
    )

-- | Bind an array of unsigned shorts.
ociBindArrayOfUnsignedShorts :: Ptr OCI_Statement -> ByteString -> Ptr CUShort -> CUInt -> IO Bool
ociBindArrayOfUnsignedShorts st n d nb = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindArrayOfUnsignedShorts($(OCI_Statement * st)
                                                              , $(const char *n')
                                                              , $(unsigned short *d)
                                                              , $(unsigned int nb)) } |]
    )

-- | Bind an integer variable.
ociBindInt :: Ptr OCI_Statement -> ByteString -> Ptr CInt -> IO Bool
ociBindInt st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindInt($(OCI_Statement *st)
                                            , $(const char *n')
                                            , $(int *d)) } |]
    )

-- | Bind an array of integers.
ociBindArrayOfInts :: Ptr OCI_Statement -> ByteString -> Ptr CInt -> CUInt -> IO Bool
ociBindArrayOfInts st n d nb = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindArrayOfInts($(OCI_Statement *st)
                                                    , $(const char *n')
                                                    , $(int *d)
                                                    , $(unsigned int nb)) } |]
    )

-- | Bind an unsigned integer variable.
ociBindUnsignedInt :: Ptr OCI_Statement -> ByteString -> Ptr CUInt -> IO Bool
ociBindUnsignedInt st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindUnsignedInt($(OCI_Statement *st)
                                                    , $(const char* n')
                                                    , $(unsigned int *d)) } |]
    )

-- | Bind an array of unsigned integers.
ociBindArrayOfUnsignedInts :: Ptr OCI_Statement -> ByteString -> Ptr CUInt -> CUInt -> IO Bool
ociBindArrayOfUnsignedInts st n d nb = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindArrayOfUnsignedInts($(OCI_Statement *st)
                                                            , $(const char *n')
                                                            , $(unsigned int *d)
                                                            , $(unsigned int nb)) } |]
    )
{-
-- | Bind a big integer variable.
-- boolean OCI_BindBigInt (OCI_Statement *stmt, const otext *name, big_int *data)
ociBindBigInt :: Ptr OCI_Statement -> ByteString -> Ptr Big_Int -> IO Bool
ociBindBigInt st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindBigInt($(OCI_Statement *st)
                                               , $(const char *n')
                                               , $(Big_Int  *d)) } |]
    )

-- | Bind an array of big integers.
-- boolean OCI_BindArrayOfBigInts (OCI_Statement *stmt, const otext *name, big_int *data, unsigned int nbelem)

-- | Bind an unsigned big integer variable.
-- boolean OCI_BindUnsignedBigInt (OCI_Statement *stmt, const otext *name, big_uint *data)

-- | Bind an array of unsigned big integers.
-- boolean OCI_BindArrayOfUnsignedBigInts (OCI_Statement *stmt, const otext *name, big_uint *data, unsigned int nbelem)
-}

-- | Bind a string variable.
ociBindString :: Ptr OCI_Statement -> ByteString -> CString -> CUInt -> IO Bool
ociBindString st name d l =
    useAsCString name (\n ->
        fmap toBool [C.exp| int { OCI_BindString($(OCI_Statement *st)
                                               , $(char *n)
                                               , $(char *d)
                                               , $(unsigned int l))} |]
    )

-- | Bind an array of strings.
ociBindArrayOfStrings :: Ptr OCI_Statement -> ByteString -> CString -> CUInt -> CUInt -> IO Bool
ociBindArrayOfStrings st name d len nbElem =
    useAsCString name (\n ->
        fmap toBool [C.exp| int { OCI_BindArrayOfStrings($(OCI_Statement *st)
                                                       , $(char *n)
                                                       , $(char *d)
                                                       , $(unsigned int len)
                                                       , $(unsigned int nbElem)) } |]
    )

-- | Bind a raw buffer.
-- boolean OCI_BindRaw (OCI_Statement *stmt, const otext *name, void *data, unsigned int len)
-- ociBindRaw :: Ptr OCI_Statement -> String ->

-- | Bind an array of raw buffers.
-- boolean OCI_BindArrayOfRaws (OCI_Statement *stmt, const otext *name, void *data, unsigned int len, unsigned int nbelem)

-- | Bind a double variable.
ociBindDouble :: Ptr OCI_Statement -> ByteString -> Ptr CDouble -> IO Bool
ociBindDouble st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindDouble($(OCI_Statement *st)
                                               , $(const char *n')
                                               , $(double *d)) } |]
    )

-- | Bind an array of doubles.
ociBindArrayOfDoubles :: Ptr OCI_Statement -> ByteString -> Ptr CDouble -> CUInt -> IO Bool
ociBindArrayOfDoubles st n d nb = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindArrayOfDoubles($(OCI_Statement *st)
                                                       , $(const char *n')
                                                       , $(double *d)
                                                       , $(unsigned int nb)) } |]
    )

-- | Bind a float variable.
ociBindFloat :: Ptr OCI_Statement -> ByteString -> Ptr CFloat -> IO Bool
ociBindFloat st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindFloat($(OCI_Statement *st)
                                              , $(const char *n')
                                              , $(float *d)) } |]
    )
-- | Bind an array of floats.
ociBindArrayOfFloats :: Ptr OCI_Statement -> ByteString -> Ptr CFloat -> CUInt -> IO Bool
ociBindArrayOfFloats st n d nb = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindArrayOfFloats($(OCI_Statement *st)
                                                      , $(const char *n')
                                                      , $(float *d)
                                                      , $(unsigned int nb)) } |]
    )
-- | Bind a date variable.
ociBindDate :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Date -> IO Bool
ociBindDate st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindDate($(OCI_Statement *st)
                                             , $(const char* n')
                                             , $(OCI_Date *d)) } |]
    )

-- | Bind an array of dates.
-- boolean OCI_BindArrayOfDates (OCI_Statement *stmt, const otext *name, OCI_Date **data, unsigned int nbelem)
{-
ociBindArrayOfDates :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Date -> CUInt -> IO Bool
ociBindArrayOfDates st n d nb = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindArrayOfDates($(OCI_Statement *st)
                                                     , $(const char *n')
                                                     , $(OCI_Date *d)
                                                     , $(unsigned int nb)) } |]
    )
-}

-- | Bind a timestamp variable.
ociBindTimestamp :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Timestamp -> IO Bool
ociBindTimestamp st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindTimestamp($(OCI_Statement *st)
                                                  , $(const char *n')
                                                  , $(OCI_Timestamp *d)) } |]
    )

-- | Bind an array of timestamp handles.
-- boolean OCI_BindArrayOfTimestamps (OCI_Statement *stmt, const otext *name, OCI_Timestamp **data, unsigned int type, unsigned int nbelem)

-- | Bind an interval variable.
ociBindInterval :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Interval -> IO Bool
ociBindInterval st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindInterval($(OCI_Statement *st)
                                                 , $(const char *n')
                                                 , $(OCI_Interval *d)) } |]
    )
-- | Bind an array of interval handles.
-- boolean OCI_BindArrayOfIntervals (OCI_Statement *stmt, const otext *name, OCI_Interval **data, unsigned int type, unsigned int nbelem)

-- | Bind a Lob variable.
ociBindLob :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Lob -> IO Bool
ociBindLob st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindLob($(OCI_Statement *st)
                                            , $(const char *n')
                                            , $(OCI_Lob *d)) } |]
    )

-- | Bind an array of Lob handles.
-- boolean OCI_BindArrayOfLobs (OCI_Statement *stmt, const otext *name, OCI_Lob **data, unsigned int type, unsigned int nbelem)

-- | Bind a File variable.
ociBindFile :: Ptr OCI_Statement -> ByteString -> Ptr OCI_File -> IO Bool
ociBindFile st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindFile($(OCI_Statement *st)
                                             , $(const char *n')
                                             , $(OCI_File *d)) } |]
    )

-- | Bind an array of File handles.
-- boolean OCI_BindArrayOfFiles (OCI_Statement *stmt, const otext *name, OCI_File **data, unsigned int type, unsigned int nbelem)

-- | Bind an object (named type) variable.
ociBindObject :: Ptr OCI_Statement -> String -> Ptr OCI_Object -> IO Bool
ociBindObject st n d = withCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindObject($(OCI_Statement *st), $(const char * n'), $(OCI_Object *d)) } |]
    )

-- | Bind an array of object handles.
-- boolean OCI_BindArrayOfObjects (OCI_Statement *stmt, const otext *name, OCI_Object **data, OCI_TypeInfo *typinf, unsigned int nbelem)

-- | Bind a Collection variable.
ociBindColl :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Coll -> IO Bool
ociBindColl st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindColl($(OCI_Statement *st)
                                             , $(const char *n')
                                             , $(OCI_Coll *d)) } |]
    )

-- | Bind an array of Collection handles.
-- boolean OCI_BindArrayOfColls (OCI_Statement *stmt, const otext *name, OCI_Coll **data, OCI_TypeInfo *typinf, unsigned int nbelem)

-- | Bind a Ref variable.
ociBindRef :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Ref -> IO Bool
ociBindRef st n d = useAsCString n (\n' ->
        fmap toBool [C.exp| int { OCI_BindRef($(OCI_Statement *st)
                                            , $(const char *n')
                                            , $(OCI_Ref *d)) } |]
    )

-- | Bind an array of Ref handles.
-- boolean OCI_BindArrayOfRefs (OCI_Statement *stmt, const otext *name, OCI_Ref **data, OCI_TypeInfo *typinf, unsigned int nbelem)

-- | Bind a Statement variable (PL/SQL Ref Cursor)
ociBindStatement :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Statement -> IO Bool
ociBindStatement st name d = useAsCString name (\name' ->
        fmap toBool [C.exp| int { OCI_BindStatement($(OCI_Statement *st), $(const char *name'), $(OCI_Statement *d)) } |]
    )

-- | Bind a Long variable.
ociBindLong :: Ptr OCI_Statement -> ByteString -> Ptr OCI_Long -> CUInt -> IO Bool
ociBindLong st name d s = useAsCString name (\name' ->
        fmap toBool [C.exp| int { OCI_BindLong($(OCI_Statement *st), $(const char *name'), $(OCI_Long *d), $(unsigned int s)) } |]
    )

-- | Returns the first or next error that occurred within a DML array statement execution.
ociGetBatchError :: Ptr OCI_Statement -> IO (Maybe (Ptr OCI_Error))
ociGetBatchError st = fmap toMaybePtr [C.exp| OCI_Error* { OCI_GetBatchError($(OCI_Statement *st)) } |]

-- | Returns the number of errors that occurred within the last DML array statement.
ociGetBatchErrorCount :: Ptr OCI_Statement -> IO CUInt
ociGetBatchErrorCount st = [C.exp| unsigned int { OCI_GetBatchErrorCount($(OCI_Statement *st)) } |]

-- | Return the number of binds currently associated to a statement.
ociGetBindCount :: Ptr OCI_Statement -> IO CUInt
ociGetBindCount st = [C.exp| unsigned int { OCI_GetBindCount($(OCI_Statement *st)) } |]

-- | Return the bind handle at the given index in the internal array of bind handle.
ociGetBind :: Ptr OCI_Statement -> CUInt -> IO (Maybe (Ptr OCI_Bind))
ociGetBind st i = fmap toMaybePtr [C.exp| OCI_Bind* { OCI_GetBind($(OCI_Statement *st), $(unsigned int i)) } |]

-- | Return a bind handle from its name.
ociGetBind2 :: Ptr OCI_Statement -> ByteString -> IO (Maybe (Ptr OCI_Bind))
ociGetBind2 st name = useAsCString name (\name' ->
        fmap toMaybePtr [C.exp| OCI_Bind* { OCI_GetBind2($(OCI_Statement *st), $(char *name')) } |]
    )

-- | Return the index of the bind from its name belonging to the given statement.
ociGetBindIndex :: Ptr OCI_Statement -> ByteString -> IO CUInt
ociGetBindIndex st name = useAsCString name (\name' ->
        [C.exp| unsigned int { OCI_GetBindIndex($(OCI_Statement *st), $(char* name')) } |]
    )

-- | Return the name of the given bind.
ociBindGetName :: Ptr OCI_Bind -> IO ByteString
ociBindGetName b = do
    name <- [C.exp| const char* { OCI_BindGetName($(OCI_Bind *b)) } |]
    packCString name

-- | Set the direction mode of a bind handle.
ociBindSetDirection :: Ptr OCI_Bind -> BindDirectionMode -> IO Bool
ociBindSetDirection b d = do
    let d' = fromIntegral $ fromEnum d
    fmap toBool [C.exp| int { OCI_BindSetDirection($(OCI_Bind *b), $(unsigned int d')) } |]

-- | Get the direction mode of a bind handle.
ociBindGetDirection :: Ptr OCI_Bind -> IO BindDirectionMode
ociBindGetDirection b = fmap
    (toEnum . fromIntegral)
    [C.exp| unsigned int { OCI_BindGetDirection($(OCI_Bind *b)) } |]

-- | Return the OCILIB type of the given bind.
-- unsigned int OCI_BindGetType (OCI_Bind *bnd)
ociBindGetType :: Ptr OCI_Bind -> IO ColumnType
ociBindGetType b = fmap
    (toEnum . fromIntegral)
    [C.exp| unsigned int { OCI_BindGetType($(OCI_Bind *b)) } |]

-- | Return the OCILIB object subtype of the given bind.
-- unsigned int OCI_BindGetSubtype (OCI_Bind *bnd)
-- ociBindGetSubtype :: Ptr OCI_Bind -> IO

-- | Return the number of elements of the bind handle.
ociBindGetDataCount :: Ptr OCI_Bind -> IO CUInt
ociBindGetDataCount b = [C.exp| unsigned int { OCI_BindGetDataCount($(OCI_Bind *b)) } |]

-- | Return the user defined data associated with a bind handle.
ociBindGetData :: Ptr OCI_Bind -> IO (Ptr ())
ociBindGetData b = [C.exp| void* { OCI_BindGetData($(OCI_Bind *b)) } |]

-- | Return the statement handle associated with a bind handle.
ociBindGetStatement :: Ptr OCI_Bind -> IO (Ptr OCI_Statement)
ociBindGetStatement b = [C.exp| OCI_Statement* { OCI_BindGetStatement($(OCI_Bind *b)) } |]

-- | Set the actual size of the element held by the given bind handle.
ociBindSetDataSize :: Ptr OCI_Bind -> CUInt -> IO Bool
ociBindSetDataSize b s = fmap
    toBool
    [C.exp| int { OCI_BindSetDataSize($(OCI_Bind *b), $(unsigned int s)) } |]

-- | Set the size of the element at the given position in the bind input array.
ociBindSetDataSizeAtPos :: Ptr OCI_Bind -> CUInt -> CUInt -> IO Bool
ociBindSetDataSizeAtPos b p s = fmap
    toBool
    [C.exp| int { OCI_BindSetDataSizeAtPos($(OCI_Bind *b), $(unsigned int p), $(unsigned int s)) } |]

-- | Return the actual size of the element held by the given bind handle.
ociBindGetDataSize :: Ptr OCI_Bind -> IO CUInt
ociBindGetDataSize b = [C.exp| unsigned int { OCI_BindGetDataSize($(OCI_Bind *b)) } |]

-- | Return the actual size of the element at the given position in the bind input array.
ociBindGetDataSizeAtPos :: Ptr OCI_Bind -> CUInt -> IO CUInt
ociBindGetDataSizeAtPos b i = [C.exp| unsigned int { OCI_BindGetDataSizeAtPos($(OCI_Bind *b), $(unsigned int i))} |]

-- | Set the bind variable to null.
ociBindSetNull :: Ptr OCI_Bind -> IO Bool
ociBindSetNull b = fmap toBool [C.exp| int { OCI_BindSetNull($(OCI_Bind *b))} |]

-- | Set to null the entry in the bind variable input array.
ociBindSetNullAtPos :: Ptr OCI_Bind -> CUInt -> IO Bool
ociBindSetNullAtPos b i = fmap toBool [C.exp| int { OCI_BindSetNullAtPos($(OCI_Bind *b), $(unsigned int i)) } |]

-- | Set the bind variable to NOT null.
ociBindSetNotNull :: Ptr OCI_Bind -> IO Bool
ociBindSetNotNull b = fmap toBool [C.exp| int { OCI_BindSetNotNull($(OCI_Bind *b)) } |]

-- | Set to NOT null the entry in the bind variable input array.
ociBindSetNotNullAtPos :: Ptr OCI_Bind -> CUInt -> IO Bool
ociBindSetNotNullAtPos b i = fmap toBool [C.exp| int { OCI_BindSetNotNullAtPos($(OCI_Bind *b), $(unsigned int i)) } |]

-- | Check if the current value of the binded variable is marked as NULL.
ociBindIsNull :: Ptr OCI_Bind -> IO Bool
ociBindIsNull b = fmap toBool [C.exp| int { OCI_BindIsNull($(OCI_Bind *b)) } |]

-- | Check if the current entry value at the given index of the binded array is marked as NULL.
ociBindIsNullAtPos :: Ptr OCI_Bind -> CUInt -> IO Bool
ociBindIsNullAtPos b i = fmap toBool [C.exp| int { OCI_BindIsNullAtPos($(OCI_Bind *b), $(unsigned int i)) } |]

-- | Set the charset form of the given character based bind variable.
-- boolean OCI_BindSetCharsetForm (OCI_Bind *bnd, unsigned int csfrm)
ociBindSetCharsetForm :: Ptr OCI_Bind -> CharsetFormType -> IO Bool
ociBindSetCharsetForm b c = do
    let c' = fromIntegral $ fromEnum c
    fmap toBool [C.exp| int { OCI_BindSetCharsetForm($(OCI_Bind *b), $(unsigned int c')) } |]
