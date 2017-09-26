{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.DateTime
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.DateTime where

import           Data.Monoid ((<>))
-- import           Foreign.C.Types
-- import           Foreign.C.String
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci
-- import           Database.Ocilib.Enums

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- | Create a local date object.
ociDateCreate :: Ptr OCI_Connection -> IO (Ptr OCI_Date)
ociDateCreate c = [C.exp| OCI_Date* { OCI_DateCreate($(OCI_Connection *c)) } |]

-- | Free a date object.
ociDateFree :: Ptr OCI_Date -> IO Bool
ociDateFree d = fmap toBool [C.exp| int { OCI_DateFree($(OCI_Date *d)) } |]

{-
-- | Create an array of date object.
-- OCI_Date **OCI_DateArrayCreate (OCI_Connection *con, unsigned int nbelem)

-- | Free an array of date objects.
-- boolean OCI_DateArrayFree (OCI_Date **dates)

-- | Add or subtract days to a date handle.
-- boolean OCI_DateAddDays (OCI_Date *date, int nb)

-- | Add or subtract months to a date handle.
-- boolean OCI_DateAddMonths (OCI_Date *date, int nb)

-- | Assign the value of a date handle to another one.
-- int OCI_DateAssign (OCI_Date *date, OCI_Date *date_src)

-- | Check if the given date is valid.
-- int OCI_DateCheck (OCI_Date *date)

-- | Compares two date handles.
-- int OCI_DateCompare (OCI_Date *date, OCI_Date *date2)

-- | Return the number of days betWeen two dates.
-- int OCI_DateDaysBetween (OCI_Date *date, OCI_Date *date2)

-- | Convert a string to a date and store it in the given date handle.
-- boolean OCI_DateFromText (OCI_Date *date, const otext *str, const otext *fmt)

-- | Convert a Date value from the given date handle to a string.
-- boolean OCI_DateToText (OCI_Date *date, const otext *fmt, int size, otext *str)

-- | Extract the date part from a date handle.
-- boolean OCI_DateGetDate (OCI_Date *date, int *year, int *month, int *day)

-- | Extract the time part from a date handle.
-- boolean OCI_DateGetTime (OCI_Date *date, int *hour, int *min, int *sec)

-}

-- | Extract the date and time parts from a date handle.
-- boolean OCI_DateGetDateTime (OCI_Date *date, int *year, int *month, int *day, int *hour, int *min, int *sec)
ociDateGetDateTime :: Ptr OCI_Date -> IO (Maybe (Int, Int, Int, Int, Int, Int))
ociDateGetDateTime d = do
    ((year', month', day', hour', mi', sec'), done) <- C.withPtrs (\(year, month, day, hour, mi, sec) ->
       [C.exp| int { OCI_DateGetDateTime( $(OCI_Date *d)
                                        , $(int *year)
                                        , $(int *month)
                                        , $(int *day)
                                        , $(int *hour)
                                        , $(int *mi)
                                        , $(int *sec)
                                        ) } |])
    if (toBool done) then
        return $ Just ( fromIntegral year'
                      , fromIntegral month'
                      , fromIntegral day'
                      , fromIntegral hour'
                      , fromIntegral mi'
                      , fromIntegral sec'
                      )
    else
        return Nothing
{-

-- | Set the date portion if the given date handle.
-- boolean OCI_DateSetDate (OCI_Date *date, int year, int month, int day)

-- | Set the time portion if the given date handle.
-- boolean OCI_DateSetTime (OCI_Date *date, int hour, int min, int sec)

-- | Set the date and time portions if the given date handle.
-- boolean OCI_DateSetDateTime (OCI_Date *date, int year, int month, int day, int hour, int min, int sec)

-- | Place the last day of month (from the given date) into the given date.
-- boolean OCI_DateLastDay (OCI_Date *date)

-- | Gets the date of next day of the week, after a given date.
-- boolean OCI_DateNextDay (OCI_Date *date, const otext *day)

-- | Return the current system date/time into the date handle.
-- boolean OCI_DateSysDate (OCI_Date *date)

-- | Convert a date from one zone to another zone. More...
-- boolean OCI_DateZoneToZone (OCI_Date *date, const otext *zone1, const otext *zone2)

-- | Affect an OCI_Date handle value to ISO C time data types.
-- boolean OCI_DateToCTime (OCI_Date *date, struct tm *ptm, time_t *pt)

-- | Affect ISO C time data types values to an OCI_Date handle.
-- boolean OCI_DateFromCTime (OCI_Date *date, struct tm *ptm, time_t t)
-}
