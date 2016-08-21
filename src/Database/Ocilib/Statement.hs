{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Statement
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Statement
    ( ociStatementCreate
    , ociStatementFree
    , ociPrepare
    , ociExecute
    , ociExecuteStmt
    , ociParse
    , ociDescribe
    , ociGetSql
    , ociGetSqlErrorPos
    , ociGetAffectedRows
    , ociGetSQLCommand
    , ociGetSQLVerb
    , ociGetStatementType
    , ociSetFetchMode
    , ociGetFetchMode
    , ociSetBindMode
    , ociGetBindMode
    , ociSetBindAllocation
    , ociGetBindAllocation
    , ociSetFetchSize
    , ociGetFetchSize
    , ociSetPrefetchSize
    , ociGetPrefetchSize
    , ociSetPrefetchMemory
    , ociGetPrefetchMemory
    , ociSetLongMaxSize
    , ociGetLongMaxSize
    , ociSetLongMode
    , ociGetLongMode
    , ociStatementGetConnection
    ) where

import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci
import           Database.Ocilib.Enums

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- Executing Statements

-- | Create a statement object and return its handle.
ociStatementCreate :: Ptr OCI_Connection -> IO (Ptr OCI_Statement)
ociStatementCreate c = [C.exp| OCI_Statement* { OCI_StatementCreate($(OCI_Connection *c)) } |]

-- | Free a statement and all resources associated to it (resultsets ...)
ociStatementFree :: Ptr OCI_Statement -> IO Bool
ociStatementFree s = fmap toBool [C.exp| int { OCI_StatementFree($(OCI_Statement *s)) } |]

-- | Prepare a SQL statement or PL/SQL block.
ociPrepare :: Ptr OCI_Statement -> String -> IO Bool
ociPrepare s sql =
    withCString sql (\q ->
        fmap toBool [C.exp| int { OCI_Prepare($(OCI_Statement *s), $(char *q)) } |]
    )

-- | Execute a prepared SQL statement or PL/SQL block.
ociExecute :: Ptr OCI_Statement -> IO Bool
ociExecute s = fmap toBool [C.exp| int { OCI_Execute($(OCI_Statement *s)) } |]

-- | Prepare and Execute a SQL statement or PL/SQL block.
ociExecuteStmt :: Ptr OCI_Statement -> String -> IO Bool
ociExecuteStmt s sql =
    withCString sql (\q ->
        fmap toBool [C.exp| int { OCI_ExecuteStmt($(OCI_Statement *s), $(char *q)) } |]
    )

-- | Parse a SQL statement or PL/SQL block.
ociParse :: Ptr OCI_Statement -> String -> IO Bool
ociParse st sql =
    withCString sql (\s ->
        fmap toBool [C.exp| int { OCI_Parse($(OCI_Statement *st), $(char *s)) } |]
    )

-- | Describe the select list of a SQL select statement.
ociDescribe :: Ptr OCI_Statement -> String -> IO Bool
ociDescribe st sql =
    withCString sql (\s ->
        fmap toBool [C.exp| int {OCI_Describe($(OCI_Statement *st), $(char *s)) } |]
    )

-- | Return the last SQL or PL/SQL statement prepared or executed by the statement.
ociGetSql :: Ptr OCI_Statement -> IO String
ociGetSql st = do
    r <- [C.exp| const char* { OCI_GetSql($(OCI_Statement *st)) } |] -- FIXME should release the CString
    peekCString r

-- | Return the error position (in terms of characters) in the SQL statement where the error occurred in case of SQL parsing error.
ociGetSqlErrorPos :: Ptr OCI_Statement -> IO CUInt
ociGetSqlErrorPos st = [C.exp| unsigned int { OCI_GetSqlErrorPos($(OCI_Statement *st)) } |]

-- | Return the number of rows affected by the SQL statement.
ociGetAffectedRows :: Ptr OCI_Statement -> IO CUInt
ociGetAffectedRows st = [C.exp| unsigned int { OCI_GetAffectedRows($(OCI_Statement *st)) } |]

-- | Return the Oracle SQL code the command held by the statement handle.
ociGetSQLCommand :: Ptr OCI_Statement -> IO CUInt
ociGetSQLCommand st = [C.exp| unsigned int { OCI_GetSQLCommand($(OCI_Statement *st)) } |]

-- | Return the verb of the SQL command held by the statement handle.
ociGetSQLVerb :: Ptr OCI_Statement -> IO String
ociGetSQLVerb st = do
    v <- [C.exp| const char* { OCI_GetSQLVerb($(OCI_Statement *st)) } |]
    peekCString v

-- Statement Control

-- | Return the type of a SQL statement.
ociGetStatementType :: Ptr OCI_Statement -> IO StatementType
ociGetStatementType st = fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_GetStatementType($(OCI_Statement *st))} |]

-- | Set the fetch mode of a SQL statement.
ociSetFetchMode :: Ptr OCI_Statement -> StatementFetchMode -> IO Bool
ociSetFetchMode st mode = do
    let mode' = fromIntegral $ fromEnum mode
    fmap toBool [C.exp| int { OCI_SetFetchMode($(OCI_Statement *st), $(unsigned int mode')) } |]

-- | Return the fetch mode of a SQL statement.
ociGetFetchMode :: Ptr OCI_Statement -> IO StatementFetchMode
ociGetFetchMode st = fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_GetFetchMode($(OCI_Statement *st)) } |]

-- | Set the binding mode of a SQL statement.
ociSetBindMode :: Ptr OCI_Statement -> BindMode -> IO Bool
ociSetBindMode st mode = do
    let mode' = fromIntegral $ fromEnum mode
    fmap toBool [C.exp| int { OCI_SetBindMode($(OCI_Statement *st), $(unsigned int mode')) } |]

-- | Return the binding mode of a SQL statement.
ociGetBindMode :: Ptr OCI_Statement -> IO BindMode
ociGetBindMode st = fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_GetBindMode($(OCI_Statement *st)) }|]

-- | Set the bind allocation mode of a SQL statement.
ociSetBindAllocation :: Ptr OCI_Statement -> BindAllocationMode -> IO Bool
ociSetBindAllocation st mode = do
    let mode' = fromIntegral $ fromEnum mode
    fmap toBool [C.exp| int { OCI_SetBindAllocation($(OCI_Statement *st), $(unsigned int mode')) } |]

-- | Return the bind allocation mode of a SQL statement.
ociGetBindAllocation :: Ptr OCI_Statement -> IO BindAllocationMode
ociGetBindAllocation st = fmap (toEnum . fromIntegral)
    [C.exp| unsigned int { OCI_GetBindAllocation($(OCI_Statement *st)) } |]

-- | Set the number of rows fetched per internal server fetch call.
ociSetFetchSize :: Ptr OCI_Statement -> CUInt -> IO Bool
ociSetFetchSize st sz =
    fmap toBool [C.exp| int {OCI_SetFetchSize($(OCI_Statement *st), $(unsigned int sz)) } |]

-- | Return the number of rows fetched per internal server fetch call.
ociGetFetchSize :: Ptr OCI_Statement -> IO CUInt
ociGetFetchSize st = [C.exp| unsigned int { OCI_GetFetchSize($(OCI_Statement *st)) } |]

-- | Set the number of rows pre-fetched by OCI Client.
ociSetPrefetchSize :: Ptr OCI_Statement -> CUInt -> IO Bool
ociSetPrefetchSize st n =
    fmap toBool [C.exp| int { OCI_SetPrefetchSize($(OCI_Statement *st), $(unsigned int n)) } |]

-- | Return the number of rows pre-fetched by OCI Client.
ociGetPrefetchSize :: Ptr OCI_Statement -> IO CUInt
ociGetPrefetchSize st = [C.exp| unsigned int { OCI_GetPrefetchSize($(OCI_Statement *st)) } |]

-- | Set the amount of memory pre-fetched by OCI Client.
ociSetPrefetchMemory :: Ptr OCI_Statement -> CUInt -> IO Bool
ociSetPrefetchMemory st s =
    fmap toBool [C.exp| int {OCI_SetPrefetchMemory($(OCI_Statement *st), $(unsigned int s)) } |]

-- | Return the amount of memory used to retrieve rows pre-fetched by OCI Client.
ociGetPrefetchMemory :: Ptr OCI_Statement -> IO CUInt
ociGetPrefetchMemory st =
    [C.exp| unsigned int { OCI_GetPrefetchMemory($(OCI_Statement *st)) } |]

-- | Set the LONG data type piece buffer size.
ociSetLongMaxSize :: Ptr OCI_Statement -> CUInt -> IO Bool
ociSetLongMaxSize st s =
    fmap toBool [C.exp| int { OCI_SetLongMaxSize($(OCI_Statement *st), $(unsigned int s)) } |]

-- | Return the LONG data type piece buffer size.
ociGetLongMaxSize :: Ptr OCI_Statement -> IO CUInt
ociGetLongMaxSize st = [C.exp| unsigned int { OCI_GetLongMaxSize($(OCI_Statement *st)) } |]

-- | Set the long data type handling mode of a SQL statement.
ociSetLongMode :: Ptr OCI_Statement -> LongMode -> IO Bool
ociSetLongMode st mode = do
    let mode' = fromIntegral $ fromEnum mode
    fmap toBool [C.exp| int { OCI_SetLongMode($(OCI_Statement *st), $(unsigned int mode')) } |]

-- | Return the long data type handling mode of a SQL statement.
ociGetLongMode :: Ptr OCI_Statement -> IO LongMode
ociGetLongMode st = fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_GetLongMode($(OCI_Statement *st)) } |]

-- | Return the connection handle associated with a statement handle.
ociStatementGetConnection :: Ptr OCI_Statement -> IO (Ptr OCI_Connection)
ociStatementGetConnection st = [C.exp| OCI_Connection* { OCI_StatementGetConnection($(OCI_Statement *st)) } |]
