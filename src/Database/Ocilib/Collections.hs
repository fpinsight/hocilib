{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Collections
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Collections
    ( ociCollCreate
    , ociCollFree
    , ociCollAssign
    , ociCollGetTypeInfo
    , ociCollGetType
    , ociCollGetMax
    , ociCollGetSize
    , ociCollGetCount
    , ociCollTrim
    , ociCollClear
    , ociCollGetElem
    --, ociCollGetElem2
    , ociCollSetElem
    , ociCollAppend
    , ociCollToText
    , ociCollDeleteElem
    , ociIterCreate
    , ociIterFree
    , ociIterGetNext
    , ociIterGetPrev
    , ociIterGetCurrent
    , ociElemCreate
    , ociElemFree
    , ociElemGetBoolean
    --, ociElemGetShort
    --, ociElemGetUnsignedShort
    --, ociElemGetInt
    --, ociElemGetUnsignedInt
    --, ociElemGetBigInt
    --, ociElemGetUnsignedBigInt
    --, ociElemGetDouble
    --, ociElemGetFloat
    , ociElemGetString
    --, ociElemGetRaw
    --, ociElemGetRawSize
    --, ociElemGetDate
    --, ociElemGetTimestamp
    --, ociElemGetInterval
    --, ociElemGetLob
    --, ociElemGetFile
    --, ociElemGetObject
    --, ociElemGetColl
    --, ociElemGetRef
    --, ociElemSetShort
    --, ociElemSetUnsignedShort
    --, ociElemSetInt
    --, ociElemSetUnsignedInt
    --, ociElemSetBigInt
    --, ociElemSetUnsignedBigInt
    --, ociElemSetDouble
    --, ociElemSetFloat
    , ociElemSetString
    --, ociElemSetRaw
    --, ociElemSetDate
    --, ociElemSetTimestamp
    --, ociElemSetInterval
    --, ociElemSetColl
    --, ociElemSetObject
    --, ociElemSetLob
    --, ociElemSetFile
    --, ociElemSetRef
    , ociElemIsNull
    , ociElemSetNull
    ) where

import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Marshal.Utils
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci
import           Database.Ocilib.Enums
import           Database.Ocilib.Internal

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- Oracle Collections (VARRAYS and Nested Tables)

-- | Create a local collection instance.
ociCollCreate :: Ptr OCI_TypeInfo -> IO (Maybe (Ptr OCI_Coll))
ociCollCreate ti = fmap toMaybePtr [C.exp| OCI_Coll* { OCI_CollCreate($(OCI_TypeInfo *ti)) } |]

-- | Free a local collection.
ociCollFree :: Ptr OCI_Coll -> IO Bool
ociCollFree coll =
    fmap toBool [C.exp| int { OCI_CollFree($(OCI_Coll* coll)) } |]

{-
-- | Create an array of Collection object.
-- OCI_Coll **OCI_CollArrayCreate (OCI_Connection *con, OCI_TypeInfo *typinf, unsigned int nbelem)

-- | Free an array of Collection objects.
-- boolean OCI_CollArrayFree (OCI_Coll **colls)
-}

-- | Assign a collection to another one.
ociCollAssign :: Ptr OCI_Coll -> Ptr OCI_Coll -> IO Bool
ociCollAssign dst src =
    fmap toBool [C.exp| int { OCI_CollAssign($(OCI_Coll *dst), $(OCI_Coll *src)) } |]

-- | Return the type info object associated to the collection.
ociCollGetTypeInfo :: Ptr OCI_Coll -> IO (Ptr OCI_TypeInfo)
ociCollGetTypeInfo c = [C.exp| OCI_TypeInfo* { OCI_CollGetTypeInfo($(OCI_Coll *c)) }|]

-- | Return the collection type.
ociCollGetType :: Ptr OCI_Coll -> IO CollectionType
ociCollGetType c = fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_CollGetType($(OCI_Coll *c)) } |]

-- | Returns the maximum number of elements of the given collection.
ociCollGetMax :: Ptr OCI_Coll -> IO CUInt
ociCollGetMax c = [C.exp| unsigned int { OCI_CollGetMax($(OCI_Coll *c)) } |]

-- | Returns the total number of elements of the given collection.
ociCollGetSize :: Ptr OCI_Coll -> IO CUInt
ociCollGetSize c = [C.exp| unsigned int { OCI_CollGetSize($(OCI_Coll *c)) } |]

-- | Returns the current number of elements of the given collection.
ociCollGetCount :: Ptr OCI_Coll -> IO CUInt
ociCollGetCount c = [C.exp| unsigned int { OCI_CollGetCount($(OCI_Coll *c)) } |]

-- | Trims the given number of elements from the end of the collection.
ociCollTrim :: Ptr OCI_Coll -> CUInt -> IO Bool
ociCollTrim c n = fmap toBool [C.exp| int { OCI_CollTrim($(OCI_Coll *c), $(unsigned int n)) } |]

-- | clear all items of the given collection.
ociCollClear :: Ptr OCI_Coll -> IO Bool
ociCollClear c = fmap toBool [C.exp| int { OCI_CollClear($(OCI_Coll *c)) } |]

-- | Return the element at the given position in the collection.
ociCollGetElem :: Ptr OCI_Coll -> CUInt -> IO (Maybe (Ptr OCI_Elem))
ociCollGetElem c i = fmap toMaybePtr [C.exp| OCI_Elem* { OCI_CollGetElem($(OCI_Coll *c), $(unsigned int i)) } |]

{-
-- | Return the element at the given position in the collection.
-- boolean OCI_CollGetElem2 (OCI_Coll *coll, unsigned int index, OCI_Elem *elem)
-}

-- | Assign the given element value to the element at the given position in the collection.
ociCollSetElem :: Ptr OCI_Coll -> CUInt -> Ptr OCI_Elem -> IO Bool
ociCollSetElem c i e = fmap toBool [C.exp| int { OCI_CollSetElem($(OCI_Coll *c), $(unsigned int i), $(OCI_Elem *e)) } |]

-- | Append the given element at the end of the collection.
ociCollAppend :: Ptr OCI_Coll -> Ptr OCI_Elem -> IO Bool
ociCollAppend c e = fmap toBool [C.exp| int { OCI_CollAppend($(OCI_Coll *c), $(OCI_Elem *e)) } |]

-- | Convert a collection handle value to a string.
ociCollToText :: Ptr OCI_Coll -> Ptr CUInt -> CString -> IO Bool
ociCollToText c sizePtr csptr = fmap toBool [C.exp| int { OCI_CollToText($(OCI_Coll *c), $(unsigned int *sizePtr), $(char *csptr)) } |]

-- | Delete the element at the given position in the Nested Table Collection.
ociCollDeleteElem :: Ptr OCI_Coll -> CUInt -> IO Bool
ociCollDeleteElem c i = fmap toBool [C.exp| int { OCI_CollDeleteElem($(OCI_Coll *c), $(unsigned int i)) }|]

-- | Create an iterator handle to iterate through a collection.
ociIterCreate :: Ptr OCI_Coll -> IO (Maybe (Ptr OCI_Iter))
ociIterCreate c = fmap toMaybePtr [C.exp| OCI_Iter* { OCI_IterCreate($(OCI_Coll *c)) } |]

-- | Free an iterator handle.
ociIterFree :: Ptr OCI_Iter -> IO Bool
ociIterFree i = fmap toBool [C.exp| int { OCI_IterFree($(OCI_Iter *i)) } |]

-- | Get the next element in the collection.
ociIterGetNext :: Ptr OCI_Iter -> IO (Maybe (Ptr OCI_Elem))
ociIterGetNext i = fmap toMaybePtr [C.exp| OCI_Elem* { OCI_IterGetNext($(OCI_Iter *i)) } |]

-- | Get the previous element in the collection.
ociIterGetPrev :: Ptr OCI_Iter -> IO (Maybe (Ptr OCI_Elem))
ociIterGetPrev i = fmap toMaybePtr [C.exp| OCI_Elem* {OCI_IterGetPrev($(OCI_Iter *i)) } |]

-- | Get the current element in the collection.
ociIterGetCurrent :: Ptr OCI_Iter -> IO (Maybe (Ptr OCI_Elem))
ociIterGetCurrent i = fmap toMaybePtr [C.exp| OCI_Elem* { OCI_IterGetCurrent($(OCI_Iter *i)) } |]

-- | Create a local collection element instance based on a collection type descriptor.
ociElemCreate :: Ptr OCI_TypeInfo -> IO (Maybe (Ptr OCI_Elem))
ociElemCreate ti = fmap toMaybePtr [C.exp| OCI_Elem* { OCI_ElemCreate($(OCI_TypeInfo *ti)) } |]

-- | Free a local collection element.
ociElemFree :: Ptr OCI_Elem -> IO Bool
ociElemFree e = fmap toBool [C.exp| int { OCI_ElemFree($(OCI_Elem *e)) } |]

-- | Return the boolean value of the given collection element.
ociElemGetBoolean :: Ptr OCI_Elem -> IO Bool
ociElemGetBoolean e = fmap toBool [C.exp| int { OCI_ElemGetBoolean($(OCI_Elem *e)) } |]

{-
-- | Return the short value of the given collection element.
-- short OCI_ElemGetShort (OCI_Elem *elem)

-- | Return the unsigned short value of the given collection element.
-- unsigned short OCI_ElemGetUnsignedShort (OCI_Elem *elem)

-- | Return the int value of the given collection element.
-- int OCI_ElemGetInt (OCI_Elem *elem)

-- | Return the unsigned int value of the given collection element.
-- unsigned int OCI_ElemGetUnsignedInt (OCI_Elem *elem)

-- | Return the big int value of the given collection element.
-- big_int OCI_ElemGetBigInt (OCI_Elem *elem)

-- | Return the unsigned big int value of the given collection element.
-- big_uint OCI_ElemGetUnsignedBigInt (OCI_Elem *elem)

-- | Return the Double value of the given collection element.
-- double OCI_ElemGetDouble (OCI_Elem *elem)

-- | Return the float value of the given collection element.
-- float OCI_ElemGetFloat (OCI_Elem *elem)

-}

-- | Return the String value of the given collection element.
ociElemGetString :: Ptr OCI_Elem -> IO String
ociElemGetString elem = do
    s <- [C.exp| const char* { OCI_ElemGetString($(OCI_Elem *elem)) } |]
    peekCString s

{-
-- | Read the RAW value of the collection element into the given buffer.
-- unsigned int OCI_ElemGetRaw (OCI_Elem *elem, void *value, unsigned int len)

-- | Return the raw attribute value size of the given element handle.
-- unsigned int OCI_ElemGetRawSize (OCI_Elem *elem)

-- | Return the Date value of the given collection element.
-- OCI_Date *OCI_ElemGetDate (OCI_Elem *elem)

-- | Return the Timestamp value of the given collection element.
-- OCI_Timestamp *OCI_ElemGetTimestamp (OCI_Elem *elem)

-- | Return the Interval value of the given collection element.
-- OCI_Interval *OCI_ElemGetInterval (OCI_Elem *elem)

-- | Return the Lob value of the given collection element.
-- OCI_Lob *OCI_ElemGetLob (OCI_Elem *elem)

-- | Return the File value of the given collection element.
-- OCI_File *OCI_ElemGetFile (OCI_Elem *elem)

-- | Return the object value of the given collection element.
-- OCI_Object *OCI_ElemGetObject (OCI_Elem *elem)

-- | Return the collection value of the given collection element.
-- OCI_Coll *OCI_ElemGetColl (OCI_Elem *elem)

-- | Return the Ref value of the given collection element.
-- OCI_Ref *OCI_ElemGetRef (OCI_Elem *elem)

-- | Set a boolean value to a collection element.
-- boolean OCI_ElemSetBoolean (OCI_Elem *elem, boolean value)

-- | Set a short value to a collection element.
-- boolean OCI_ElemSetShort (OCI_Elem *elem, short value)

-- | Set a unsigned short value to a collection element.
-- boolean OCI_ElemSetUnsignedShort (OCI_Elem *elem, unsigned short value)

-- | Set a int value to a collection element.
-- boolean OCI_ElemSetInt (OCI_Elem *elem, int value)

-- | Set a unsigned int value to a collection element.
-- boolean OCI_ElemSetUnsignedInt (OCI_Elem *elem, unsigned int value)

-- | Set a big int value to a collection element.
-- boolean OCI_ElemSetBigInt (OCI_Elem *elem, big_int value)

-- | Set a unsigned big_int value to a collection element.
-- boolean OCI_ElemSetUnsignedBigInt (OCI_Elem *elem, big_uint value)

-- | Set a double value to a collection element.
-- boolean OCI_ElemSetDouble (OCI_Elem *elem, double value)

-- | Set a float value to a collection element.
-- boolean OCI_ElemSetFloat (OCI_Elem *elem, float value)
-}

-- | Set a string value to a collection element.
ociElemSetString :: Ptr OCI_Elem -> String -> IO Bool
ociElemSetString elem value =
    withCString value (\value' ->
        fmap toBool [C.exp| int { OCI_ElemSetString($(OCI_Elem *elem), $(char *value')) } |]
    )
{-
-- | Set a RAW value to a collection element.
-- boolean OCI_ElemSetRaw (OCI_Elem *elem, void *value, unsigned int len)

-- | Assign a Date handle to a collection element.
-- boolean OCI_ElemSetDate (OCI_Elem *elem, OCI_Date *value)

-- | Assign a Timestamp handle to a collection element.
-- boolean OCI_ElemSetTimestamp (OCI_Elem *elem, OCI_Timestamp *value)

-- | Assign an Interval handle to a collection element.
-- boolean OCI_ElemSetInterval (OCI_Elem *elem, OCI_Interval *value)

-- | Assign a Collection handle to a collection element.
-- boolean OCI_ElemSetColl (OCI_Elem *elem, OCI_Coll *value)

-- | Assign an Object handle to a collection element.
-- boolean OCI_ElemSetObject (OCI_Elem *elem, OCI_Object *value)

-- | Assign a Lob handle to a collection element.
-- boolean OCI_ElemSetLob (OCI_Elem *elem, OCI_Lob *value)

-- | Assign a File handle to a collection element.
-- boolean OCI_ElemSetFile (OCI_Elem *elem, OCI_File *value)

-- | Assign a Ref handle to a collection element.
-- boolean OCI_ElemSetRef (OCI_Elem *elem, OCI_Ref *value)
-}

-- | Check if the collection element value is null.
ociElemIsNull :: Ptr OCI_Elem -> IO Bool
ociElemIsNull e = fmap toBool [C.exp| int { OCI_ElemIsNull($(OCI_Elem *e)) }|]

-- | Set a collection element value to null.
ociElemSetNull :: Ptr OCI_Elem -> IO Bool
ociElemSetNull e = fmap toBool [C.exp| int { OCI_ElemSetNull($(OCI_Elem *e)) } |]
