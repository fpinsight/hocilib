{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Timestamp
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Timestamp where

import           Data.Monoid ((<>))
import           Foreign.C.Types
-- import           Foreign.C.String
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci
-- import           Database.Ocilib.Enums

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- | Create a local Timestamp instance.
ociTimestampCreate :: Ptr OCI_Connection -> CUInt -> IO (Ptr OCI_Timestamp)
ociTimestampCreate c t = [C.exp| OCI_Timestamp* { OCI_TimestampCreate($(OCI_Connection *c), $(unsigned int t)) } |] -- FIXME use enum for type

-- | Free an OCI_Timestamp handle.
ociTimestampFree :: Ptr OCI_Timestamp -> IO Bool
ociTimestampFree t = fmap toBool [C.exp| int { OCI_TimestampFree($(OCI_Timestamp *t)) } |]

{-
-- | Create an array of timestamp object.
OCI_Timestamp **OCI_TimestampArrayCreate (OCI_Connection *con, unsigned int type, unsigned int nbelem)

-- | Free an array of timestamp objects.
boolean OCI_TimestampArrayFree (OCI_Timestamp **tmsps)

-- | Return the type of the given Timestamp object.
unsigned int OCI_TimestampGetType (OCI_Timestamp *tmsp)

-- | Assign the value of a timestamp handle to another one.
boolean OCI_TimestampAssign (OCI_Timestamp *tmsp, OCI_Timestamp *tmsp_src)

-- | Check if the given timestamp is valid.
int OCI_TimestampCheck (OCI_Timestamp *tmsp)

-- | Compares two timestamp handles. More...
int OCI_TimestampCompare (OCI_Timestamp *tmsp, OCI_Timestamp *tmsp2)

-- | Set a timestamp handle value.
boolean OCI_TimestampConstruct (OCI_Timestamp *tmsp, int year, int month, int day, int hour, int min, int sec, int fsec, const otext *time_zone)

-- | Convert one timestamp value from one type to another.
boolean OCI_TimestampConvert (OCI_Timestamp *tmsp, OCI_Timestamp *tmsp_src)

-- | Convert a string to a timestamp and store it in the given timestamp handle.
boolean OCI_TimestampFromText (OCI_Timestamp *tmsp, const otext *str, const otext *fmt)

-- |Convert a timestamp value from the given timestamp handle to a string.
boolean OCI_TimestampToText (OCI_Timestamp *tmsp, const otext *fmt, int size, otext *str, int precision)

-- | Extract the date part from a timestamp handle.
boolean OCI_TimestampGetDate (OCI_Timestamp *tmsp, int *year, int *month, int *day)

-- | Extract the time portion from a timestamp handle.
boolean OCI_TimestampGetTime (OCI_Timestamp *tmsp, int *hour, int *min, int *sec, int *fsec)

-- | Extract the date and time parts from a date handle.
boolean OCI_TimestampGetDateTime (OCI_Timestamp *tmsp, int *year, int *month, int *day, int *hour, int *min, int *sec, int *fsec)

-- | Return the time zone name of a timestamp handle.
boolean OCI_TimestampGetTimeZoneName (OCI_Timestamp *tmsp, int size, otext *str)

-- | Return the time zone (hour, minute) portion of a timestamp handle.
boolean OCI_TimestampGetTimeZoneOffset (OCI_Timestamp *tmsp, int *hour, int *min)

-- | Add an interval value to a timestamp value of a timestamp handle.
boolean OCI_TimestampIntervalAdd (OCI_Timestamp *tmsp, OCI_Interval *itv)

-- | Subtract an interval value from a timestamp value of a timestamp handle.
boolean OCI_TimestampIntervalSub (OCI_Timestamp *tmsp, OCI_Interval *itv)

-- | Store the difference of two timestamp handles into an interval handle.
boolean OCI_TimestampSubtract (OCI_Timestamp *tmsp, OCI_Timestamp *tmsp2, OCI_Interval *itv)

-- | Stores the system current date and time as a timestamp value with time zone into the timestamp handle.
boolean OCI_TimestampSysTimestamp (OCI_Timestamp *tmsp)

-- | Affect an OCI_Timestamp handle value to ISO C time data types.
boolean OCI_TimestampToCTime (OCI_Timestamp *tmsp, struct tm *ptm, time_t *pt)

-- | Affect ISO C time data types values to an OCI_Timestamp handle.
boolean OCI_TimestampFromCTime (OCI_Timestamp *tmsp, struct tm *ptm, time_t t)

-- | Create a local interval object.
OCI_Interval *OCI_IntervalCreate (OCI_Connection *con, unsigned int type)

-- Free an OCI_Interval handle.
boolean OCI_IntervalFree (OCI_Interval *itv)

-- | Create an array of Interval object.
OCI_Interval **OCI_IntervalArrayCreate (OCI_Connection *con, unsigned int type, unsigned int nbelem)

-- | Free an array of Interval objects.
boolean OCI_IntervalArrayFree (OCI_Interval **itvs)

-- | Return the type of the given Interval object.
unsigned int OCI_IntervalGetType (OCI_Interval *itv)

-- | Assign the value of a interval handle to another one.
boolean OCI_IntervalAssign (OCI_Interval *itv, OCI_Interval *itv_src)

-- | Check if the given interval is valid.
int OCI_IntervalCheck (OCI_Interval *itv)

-- | Compares two interval handles.
int OCI_IntervalCompare (OCI_Interval *itv, OCI_Interval *itv2)

-- | Convert a string to an interval and store it in the given interval handle.
boolean OCI_IntervalFromText (OCI_Interval *itv, const otext *str)

-- | Convert an interval value from the given interval handle to a string.
boolean OCI_IntervalToText (OCI_Interval *itv, int leading_prec, int fraction_prec, int size, otext *str)

-- | Correct an interval handle value with the given time zone.
boolean OCI_IntervalFromTimeZone (OCI_Interval *itv, const otext *str)

-- | Return the day / time portion of an interval handle.
boolean OCI_IntervalGetDaySecond (OCI_Interval *itv, int *day, int *hour, int *min, int *sec, int *fsec)

-- | Return the year / month portion of an interval handle.
boolean OCI_IntervalGetYearMonth (OCI_Interval *itv, int *year, int *month)

-- | Set the day / time portion if the given interval handle.
boolean OCI_IntervalSetDaySecond (OCI_Interval *itv, int day, int hour, int min, int sec, int fsec)

-- | Set the year / month portion if the given Interval handle.
boolean OCI_IntervalSetYearMonth (OCI_Interval *itv, int year, int month)

-- | Adds an interval handle value to another.
boolean OCI_IntervalAdd (OCI_Interval *itv, OCI_Interval *itv2)

-- | Subtract an interval handle value from another.
boolean OCI_IntervalSubtract (OCI_Interval *itv, OCI_Interval *itv2)

-}
