{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Connection
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Connection
    (
    -- * Connecting to Database
      ociConnectionCreate
    , ociConnectionFree
    , ociIsConnected
    , ociPing
    , ociBreak
    ) where

import           Data.Monoid ((<>))
-- import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           Database.Ocilib.Oci
import           Database.Ocilib.Enums
import           Database.Ocilib.Internal

C.context (C.baseCtx <> C.funCtx <> ociCtx)

C.include "<ocilib.h>"

-- Connecting to database

type Connection = Ptr OCI_Connection

-- | Create a physical connection to an Oracle database server.
ociConnectionCreate :: String -> String -> String -> SessionMode -> IO (Maybe Connection)
ociConnectionCreate db user pass mode = do
    let m = fromIntegral $ fromEnum mode
    withCString db ( \d ->
        withCString user ( \u ->
            withCString pass ( \p ->
                fmap toMaybePtr [C.exp| OCI_Connection* { OCI_ConnectionCreate($(char* d), $(char* u), $(char* p), $(unsigned int m)) } |]
            )
        )
        )

-- | Close a physical connection to an Oracle database server.
ociConnectionFree :: Ptr OCI_Connection -> IO Bool
ociConnectionFree c =
    fmap toBool [C.exp| int { OCI_ConnectionFree($(OCI_Connection *c)) } |]

-- | Returns TRUE is the given connection is still connected otherwise FALSE.
ociIsConnected :: Ptr OCI_Connection -> IO Bool
ociIsConnected c =
    fmap toBool [C.exp| int { OCI_IsConnected($(OCI_Connection *c)) } |]

{-
-- | Return the pointer to user data previously associated with the connection.
-- void *OCI_GetUserData (OCI_Connection *con)

-- | Associate a pointer to user data to the given connection.
-- boolean OCI_SetUserData (OCI_Connection *con, void *data)

-- | Associate a tag to the given connection/session.
-- boolean OCI_SetSessionTag (OCI_Connection *con, const otext *tag)

-- | Return the tag associated the given connection.
-- const otext *OCI_GetSessionTag (OCI_Connection *con)

-- | Return the name of the connected database/service name.
-- const otext *OCI_GetDatabase (OCI_Connection *con)

-- | Return the current logged user name.
-- const otext *OCI_GetUserName (OCI_Connection *con)

-- | Return the current logged user password.
-- const otext *OCI_GetPassword (OCI_Connection *con)

-- | Change the password of the logged user.
-- boolean OCI_SetPassword(OCI_Connection *con, const otext *password)

-- | Change the password of the given user on the given database.
-- boolean OCI_SetUserPassword (const otext *db, const otext *user, const otext *pwd, const otext *new_pwd)

-- | Return the current session mode.
-- unsigned int OCI_GetSessionMode (OCI_Connection *con)

-- | Return the connected database server version.
-- const otext *OCI_GetVersionServer (OCI_Connection *con)

-- | Return the major version number of the connected database server.
-- unsigned int OCI_GetServerMajorVersion (OCI_Connection *con)

-- | Return the minor version number of the connected database server.
-- unsigned int OCI_GetServerMinorVersion (OCI_Connection *con)

-- | Return the revision version number of the connected database server.
-- unsigned int OCI_GetServerRevisionVersion (OCI_Connection *con)

-- | Set the format string for implicit string conversions of the given type.
-- boolean OCI_SetFormat (OCI_Connection *con, unsigned int type, const otext *format)

-- | Return the format string for implicit string conversions of the given type.
-- const otext *OCI_GetFormat (OCI_Connection *con, unsigned int type)

-- | Return the current transaction of the connection.
-- OCI_Transaction *OCI_GetTransaction (OCI_Connection *con)

-- | Return the highest Oracle version is supported by the connection.
-- unsigned int OCI_GetVersionConnection (OCI_Connection *con)

-- | Set tracing information to the session of the given connection.
-- boolean OCI_SetTrace (OCI_Connection *con, unsigned int trace, const otext *value)

-- | Get the current trace for the trace type from the given connection.
-- const otext *OCI_GetTrace (OCI_Connection *con, unsigned int trace)

-}

-- | Makes a round trip call to the server to confirm that the connection and the server are active.
ociPing :: Ptr OCI_Connection -> IO Bool
ociPing c = fmap toBool [C.exp| int { OCI_Ping($(OCI_Connection *c)) } |]

{-
-- | Return the Oracle server database name of the connected database/service name.
-- const otext *OCI_GetDBName (OCI_Connection *con)

-- | Return the Oracle server Instance name of the connected database/service name.
-- const otext *OCI_GetInstanceName (OCI_Connection *con)

-- | Return the Oracle server service name of the connected database/service name.
-- const otext *OCI_GetServiceName (OCI_Connection *con)

-- | Return the Oracle server machine name of the connected database/service name.
-- const otext *OCI_GetServerName (OCI_Connection *con)

-- | Return the Oracle server domain name of the connected database/service name.
-- const otext *OCI_GetDomainName (OCI_Connection *con)

-- | Return the date and time (Timestamp) server instance start of the connected database/service name.
-- OCI_Timestamp *OCI_GetInstanceStartTime (OCI_Connection *con)

-- | Verify if the given connection support TAF events.
-- boolean OCI_IsTAFCapable (OCI_Connection *con)

-- | Set the Transparent Application Failover (TAF) user handler.
-- boolean OCI_SetTAFHandler (OCI_Connection *con, POCI_TAF_HANDLER handler)

-- | Return the maximum number of statements to keep in the statement cache.
-- unsigned int OCI_GetStatementCacheSize (OCI_Connection *con)

-- | Set the maximum number of statements to keep in the statement cache.
-- boolean OCI_SetStatementCacheSize (OCI_Connection *con, unsigned int value)

-- | Return the default LOB prefetch buffer size for the connection.
-- unsigned int OCI_GetDefaultLobPrefetchSize (OCI_Connection *con)

-- | Enable or disable prefetching for all LOBs fetched in the connection.
-- boolean OCI_SetDefaultLobPrefetchSize (OCI_Connection *con, unsigned int value)

-- | Return the maximum number of SQL statements that can be opened in one session.
-- unsigned int OCI_GetMaxCursors (OCI_Connection *con)
-}

-- | Perform an immediate abort of any currently Oracle OCI call.
ociBreak :: Ptr OCI_Connection -> IO Bool
ociBreak c = fmap toBool [C.exp| int { OCI_Break($(OCI_Connection *c)) } |]
