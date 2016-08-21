{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

-- |
-- Module:      Database.Ocilib.Oci
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Oci
    ( ociCtx
    , OCI_Pool
    , OCI_Connection
    , OCI_Statement
    , OCI_Bind
    , OCI_Resultset
    , OCI_Column
    , OCI_Lob
    , OCI_File
    , OCI_Transaction
    , OCI_Long
    , OCI_Date
    , OCI_Timestamp
    , OCI_Interval
    , OCI_Object
    , OCI_Coll
    , OCI_Elem
    , OCI_Iter
    , OCI_Ref
    , OCI_TypeInfo
    , OCI_HashTable
    , OCI_Error
    , OCI_Mutex
    , OCI_Thread
    , OCI_DirPath
    , OCI_Subscription
    , OCI_Event
    , OCI_Msg
    , OCI_Agent
    , OCI_Dequeue
    , OCI_Enqueue
    ) where

import qualified Data.Map as Map
import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

ociCtx :: C.Context
ociCtx = mempty { ctxTypesTable = ociTypesTable }

-- | Pool object (session or connection)
-- A pool is a set of pooled objects
data OCI_Pool

-- | Oracle physical connection.
-- It holds all information about a connection such as error handling, associated statements, ... Error handling and transactions are embedded within a connection object.
data OCI_Connection

-- | Oracle SQL or PL/SQL statement.
-- A Statement object allows users to prepare, execute SQL orders or PL/SQL blocks
data OCI_Statement

-- | Internal bind representation.
-- A bind object is an object that holds all information about an Oracle statement binding operation
data OCI_Bind

-- | Collection of output columns from a select statement.
-- A resultset object is the result of 'select' SQL Statement.
-- It's a set of data (ordered in columns) that can be fetched row by row to get data returned by the SQL statement
data OCI_Resultset

-- | Oracle SQL Column and Type member representation.
-- A column object represents an output column from a select statement
data OCI_Column

-- | Oracle Internal Large objects
data OCI_Lob

-- | Oracle External Large objects
-- FILEs were introduced by OCI8 in order to store references to files located outside the database.
data OCI_File

-- | Oracle Transaction
data OCI_Transaction

-- | Oracle Long data type
-- Those types were used in older versions of Oracle (before Oracle8i) to store large chunks of data in the database.
-- It's now depreciated by Oracle that recommends using LOBs
data OCI_Long

-- | Oracle internal date representation
data OCI_Date

-- | Oracle internal timestamp representation.
data OCI_Timestamp

-- | Oracle internal interval representation.
data OCI_Interval

-- | Oracle Named types representation.
data OCI_Object

-- | Oracle Collections (VARRAYs and Nested Tables) representation.
data OCI_Coll

-- | Oracle Collection item representation.
data OCI_Elem

-- | Oracle Collection iterator representation.
data OCI_Iter

-- | Oracle REF type representation.
data OCI_Ref

-- | Type info metadata handle.
data OCI_TypeInfo

-- | OCILIB implementation of hash tables.
data OCI_HashTable

-- | Encapsulates an Oracle or OCILIB exception.
data OCI_Error

-- | OCILIB encapsulation of OCI mutexes.
data OCI_Mutex

-- | OCILIB encapsulation of OCI Threads.
data OCI_Thread

-- | OCILIB encapsulation of OCI Direct Path handle.
data OCI_DirPath

-- | OCILIB encapsulation of Oracle DCN notification.
data OCI_Subscription

-- | OCILIB encapsulation of Oracle DCN event.
data OCI_Event

-- | OCILIB encapsulation of A/Q message.
data OCI_Msg

-- | OCILIB encapsulation of A/Q Agent.
data OCI_Agent

-- | OCILIB encapsulation of A/Q dequeuing operations.
data OCI_Dequeue

-- | OCILIB encapsulation of A/Q enqueuing operations.
data OCI_Enqueue

ociTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
ociTypesTable = Map.fromList
    [ (C.TypeName "OCI_Pool"        , [t| OCI_Pool         |])
    , (C.TypeName "OCI_Connection"  , [t| OCI_Connection   |])
    , (C.TypeName "OCI_Statement"   , [t| OCI_Statement    |])
    , (C.TypeName "OCI_Bind"        , [t| OCI_Bind         |])
    , (C.TypeName "OCI_Resultset"   , [t| OCI_Resultset    |])
    , (C.TypeName "OCI_Column"      , [t| OCI_Column       |])
    , (C.TypeName "OCI_Lob"         , [t| OCI_Lob          |])
    , (C.TypeName "OCI_File"        , [t| OCI_File         |])
    , (C.TypeName "OCI_Transaction" , [t| OCI_Transaction  |])
    , (C.TypeName "OCI_Long"        , [t| OCI_Long         |])
    , (C.TypeName "OCI_Date"        , [t| OCI_Date         |])
    , (C.TypeName "OCI_Timestamp"   , [t| OCI_Timestamp    |])
    , (C.TypeName "OCI_Interval"    , [t| OCI_Interval     |])
    , (C.TypeName "OCI_Object"      , [t| OCI_Object       |])
    , (C.TypeName "OCI_Coll"        , [t| OCI_Coll         |])
    , (C.TypeName "OCI_Elem"        , [t| OCI_Elem         |])
    , (C.TypeName "OCI_Iter"        , [t| OCI_Iter         |])
    , (C.TypeName "OCI_Ref"         , [t| OCI_Ref          |])
    , (C.TypeName "OCI_TypeInfo"    , [t| OCI_TypeInfo     |])
    , (C.TypeName "OCI_HashTable"   , [t| OCI_HashTable    |])
    , (C.TypeName "OCI_Error"       , [t| OCI_Error        |])
    , (C.TypeName "OCI_Mutex"       , [t| OCI_Mutex        |])
    , (C.TypeName "OCI_Thread"      , [t| OCI_Thread       |])
    , (C.TypeName "OCI_DirPath"     , [t| OCI_DirPath      |])
    , (C.TypeName "OCI_Subscription", [t| OCI_Subscription |])
    , (C.TypeName "OCI_Event"       , [t| OCI_Event        |])
    , (C.TypeName "OCI_Msg"         , [t| OCI_Msg          |])
    , (C.TypeName "OCI_Agent"       , [t| OCI_Agent        |])
    , (C.TypeName "OCI_Dequeue"     , [t| OCI_Dequeue      |])
    , (C.TypeName "OCI_Enqueue"     , [t| OCI_Enqueue      |])
    ]
