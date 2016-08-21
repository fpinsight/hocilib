{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module:      Database.Ocilib.Enums
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Enums where

import Database.Ocilib.BitMask


#include <ocilib.h>

{#enum define ImportMode { OCI_IMPORT_MODE_LINKAGE as OCI_IMPORT_MODE_LINKAGE
                         , OCI_IMPORT_MODE_RUNTIME as OCI_IMPORT_MODE_RUNTIME
                         } deriving (Eq, Ord, Show) #}

{#enum define CharsetMode { OCI_CHAR_ANSI as OCI_CHAR_ANSI
                          , OCI_CHAR_WIDE as OCI_CHAR_WIDE
                          } deriving (Eq, Ord, Show) #}

-- OCILIB Error types

{#enum define ErrorType { OCI_ERR_ORACLE  as OCI_ERR_ORACLE
                        , OCI_ERR_OCILIB  as OCI_ERR_OCILIB
                        , OCI_ERR_WARNING as OCI_ERR_WARNING
                        } deriving (Eq, Ord, Show) #}

-- OCILIB Error codes

{#enum define ErrorCode { OCI_ERR_NONE                   as OCI_ERR_NONE
                        , OCI_ERR_NOT_INITIALIZED        as OCI_ERR_NOT_INITIALIZED
                        , OCI_ERR_LOADING_SHARED_LIB     as OCI_ERR_LOADING_SHARED_LIB
                        , OCI_ERR_LOADING_SYMBOLS        as OCI_ERR_LOADING_SYMBOLS
                        , OCI_ERR_MULTITHREADED          as OCI_ERR_MULTITHREADED
                        , OCI_ERR_MEMORY                 as OCI_ERR_MEMORY
                        , OCI_ERR_NOT_AVAILABLE          as OCI_ERR_NOT_AVAILABLE
                        , OCI_ERR_NULL_POINTER           as OCI_ERR_NULL_POINTER
                        , OCI_ERR_DATATYPE_NOT_SUPPORTED as OCI_ERR_DATATYPE_NOT_SUPPORTED
                        , OCI_ERR_PARSE_TOKEN            as OCI_ERR_PARSE_TOKEN
                        , OCI_ERR_MAP_ARGUMENT           as OCI_ERR_MAP_ARGUMENT
                        , OCI_ERR_OUT_OF_BOUNDS          as OCI_ERR_OUT_OF_BOUNDS
                        , OCI_ERR_UNFREED_DATA           as OCI_ERR_UNFREED_DATA
                        , OCI_ERR_MAX_BIND               as OCI_ERR_MAX_BIND
                        , OCI_ERR_ATTR_NOT_FOUND         as OCI_ERR_ATTR_NOT_FOUND
                        , OCI_ERR_MIN_VALUE              as OCI_ERR_MIN_VALUE
                        , OCI_ERR_NOT_COMPATIBLE         as OCI_ERR_NOT_COMPATIBLE
                        , OCI_ERR_STMT_STATE             as OCI_ERR_STMT_STATE
                        , OCI_ERR_STMT_NOT_SCROLLABLE    as OCI_ERR_STMT_NOT_SCROLLABLE
                        , OCI_ERR_BIND_ALREADY_USED      as OCI_ERR_BIND_ALREADY_USED
                        , OCI_ERR_BIND_ARRAY_SIZE        as OCI_ERR_BIND_ARRAY_SIZE
                        , OCI_ERR_COLUMN_NOT_FOUND       as OCI_ERR_COLUMN_NOT_FOUND
                        , OCI_ERR_DIRPATH_STATE          as OCI_ERR_DIRPATH_STATE
                        , OCI_ERR_CREATE_OCI_ENVIRONMENT as OCI_ERR_CREATE_OCI_ENVIRONMENT
                        , OCI_ERR_REBIND_BAD_DATATYPE    as OCI_ERR_REBIND_BAD_DATATYPE
                        , OCI_ERR_TYPEINFO_DATATYPE      as OCI_ERR_TYPEINFO_DATATYPE
                        , OCI_ERR_ITEM_NOT_FOUND         as OCI_ERR_ITEM_NOT_FOUND
                        , OCI_ERR_ARG_INVALID_VALUE      as OCI_ERR_ARG_INVALID_VALUE
                        } deriving (Eq, Ord, Show) #}

-- allocated bytes types --

{#enum define AllocatedBytesType { OCI_MEM_ORACLE as OCI_MEM_ORACLE
                                 , OCI_MEM_OCILIB as OCI_MEM_OCILIB
                                 , OCI_MEM_ALL    as OCI_MEM_ALL
                                 } deriving (Eq, Ord, Show) #}

-- binding --

{#enum define BindMode { OCI_BIND_BY_POS as OCI_BIND_BY_POS
                       , OCI_BIND_BY_NAME as OCI_BIND_BY_NAME
                       } deriving (Eq, Ord, Show) #}


-- fetching --
{#enum define LongMode { OCI_LONG_EXPLICIT as OCI_LONG_EXPLICIT
                       , OCI_LONG_IMPLICIT as OCI_LONG_IMPLICIT
                       } deriving (Eq, Ord, Show) #}

{-
-- unknown value --

OCI_UNKNOWN                         0
-}

-- C Data Type mapping --
{#enum define ColumnType { OCI_CDT_NUMERIC    as OCI_CDT_NUMERIC
                         , OCI_CDT_DATETIME   as OCI_CDT_DATETIME
                         , OCI_CDT_TEXT       as OCI_CDT_TEXT
                         , OCI_CDT_LONG       as OCI_CDT_LONG
                         , OCI_CDT_CURSOR     as OCI_CDT_CURSOR
                         , OCI_CDT_LOB        as OCI_CDT_LOB
                         , OCI_CDT_FILE       as OCI_CDT_FILE
                         , OCI_CDT_TIMESTAMP  as OCI_CDT_TIMESTAMP
                         , OCI_CDT_INTERVAL   as OCI_CDT_INTERVAL
                         , OCI_CDT_RAW        as OCI_CDT_RAW
                         , OCI_CDT_OBJECT     as OCI_CDT_OBJECT
                         , OCI_CDT_COLLECTION as OCI_CDT_COLLECTION
                         , OCI_CDT_REF        as OCI_CDT_REF
                         , OCI_CDT_BOOLEAN    as OCI_CDT_BOOLEAN
                         } deriving (Eq, Ord, Show) #}

-- Data Type codes for OCI_ImmediateXXX() calls --
{#enum define DataTypeCode { OCI_ARG_SHORT      as OCI_ARG_SHORT
                           , OCI_ARG_USHORT     as OCI_ARG_USHORT
                           , OCI_ARG_INT        as OCI_ARG_INT
                           , OCI_ARG_UINT       as OCI_ARG_UINT
                           , OCI_ARG_BIGINT     as OCI_ARG_BIGINT
                           , OCI_ARG_BIGUINT    as OCI_ARG_BIGUINT
                           , OCI_ARG_DOUBLE     as OCI_ARG_DOUBLE
                           , OCI_ARG_DATETIME   as OCI_ARG_DATETIME
                           , OCI_ARG_TEXT       as OCI_ARG_TEXT
                           , OCI_ARG_LOB        as OCI_ARG_LOB
                           , OCI_ARG_FILE       as OCI_ARG_FILE
                           , OCI_ARG_TIMESTAMP  as OCI_ARG_TIMESTAMP
                           , OCI_ARG_INTERVAL   as OCI_ARG_INTERVAL
                           , OCI_ARG_RAW        as OCI_ARG_RAW
                           , OCI_ARG_OBJECT     as OCI_ARG_OBJECT
                           , OCI_ARG_COLLECTION as OCI_ARG_COLLECTION
                           , OCI_ARG_REF        as OCI_ARG_REF
                           , OCI_ARG_FLOAT      as OCI_ARG_FLOAT
                           } deriving (Eq, Ord, Show) #}


-- statement types --

{#enum define StatementType { OCI_CST_SELECT as OCI_CST_SELECT
                            , OCI_CST_UPDATE as OCI_CST_UPDATE
                            , OCI_CST_DELETE as OCI_CST_DELETE
                            , OCI_CST_INSERT as OCI_CST_INSERT
                            , OCI_CST_CREATE as OCI_CST_CREATE
                            , OCI_CST_DROP as OCI_CST_DROP
                            , OCI_CST_ALTER as OCI_CST_ALTER
                            , OCI_CST_BEGIN as OCI_CST_BEGIN
                            , OCI_CST_DECLARE as OCI_CST_DECLARE
                            , OCI_CST_CALL as OCI_CST_CALL
                            } deriving (Eq, Ord, Show) #}

-- environment modes --

{#enum define EnvironmentMode { OCI_ENV_DEFAULT as OCI_ENV_DEFAULT
                              , OCI_ENV_THREADED as OCI_ENV_THREADED
                              , OCI_ENV_CONTEXT as OCI_ENV_CONTEXT
                              , OCI_ENV_EVENTS as OCI_ENV_EVENTS
                              } deriving (Eq, Ord, Show) #}

instance ToBitMask EnvironmentMode

-- sessions modes --

{#enum define SessionMode { OCI_SESSION_DEFAULT as OCI_SESSION_DEFAULT
                          , OCI_SESSION_XA as OCI_SESSION_XA
                          , OCI_SESSION_SYSDBA as OCI_SESSION_SYSDBA
                          , OCI_SESSION_SYSOPER as OCI_SESSION_SYSOPER
                          , OCI_SESSION_PRELIM_AUTH as OCI_SESSION_PRELIM_AUTH
                          } deriving (Eq, Ord, Show) #}


-- change notification types --

{#enum define ChangeNotificationType { OCI_CNT_OBJECTS   as OCI_CNT_OBJECTS
                                     , OCI_CNT_ROWS      as OCI_CNT_ROWS
                                     , OCI_CNT_DATABASES as OCI_CNT_DATABASES
                                     , OCI_CNT_ALL       as OCI_CNT_ALL
                                     } deriving (Eq, Ord, Show) #}

-- event notification types --

{#enum define EventNotificationType { OCI_ENT_STARTUP        as OCI_ENT_STARTUP
                                    , OCI_ENT_SHUTDOWN       as OCI_ENT_SHUTDOWN
                                    , OCI_ENT_SHUTDOWN_ANY   as OCI_ENT_SHUTDOWN_ANY
                                    , OCI_ENT_DROP_DATABASE  as OCI_ENT_DROP_DATABASE
                                    , OCI_ENT_DEREGISTER     as OCI_ENT_DEREGISTER
                                    , OCI_ENT_OBJECT_CHANGED as OCI_ENT_OBJECT_CHANGED
                                    } deriving (Eq, Ord, Show) #}

-- event object notification types --

{#enum define EventObjectNotificationType { OCI_ONT_INSERT  as OCI_ONT_INSERT
                                          , OCI_ONT_UPDATE  as OCI_ONT_UPDATE
                                          , OCI_ONT_DELETE  as OCI_ONT_DELETE
                                          , OCI_ONT_ALTER   as OCI_ONT_ALTER
                                          , OCI_ONT_DROP    as OCI_ONT_DROP
                                          , OCI_ONT_GENERIC as OCI_ONT_GENERIC
                                          } deriving (Eq, Ord, Show) #}

-- database startup modes --

{#enum define DatabaseStartupMode { OCI_DB_SPM_START as OCI_DB_SPM_START
                                  , OCI_DB_SPM_MOUNT as OCI_DB_SPM_MOUNT
                                  , OCI_DB_SPM_OPEN  as OCI_DB_SPM_OPEN
                                  , OCI_DB_SPM_FULL  as OCI_DB_SPM_FULL
                                  } deriving (Eq, Ord, Show) #}

-- database startup flags --

{#enum define DatabaseStartupFlag { OCI_DB_SPF_DEFAULT  as OCI_DB_SPF_DEFAULT
                                  , OCI_DB_SPF_FORCE    as OCI_DB_SPF_FORCE
                                  , OCI_DB_SPF_RESTRICT as OCI_DB_SPF_RESTRICT
                                  } deriving (Eq, Ord, Show) #}


-- database shutdown modes --

{#enum define DatabaseShutdownMode { OCI_DB_SDM_SHUTDOWN as OCI_DB_SDM_SHUTDOWN
                                   , OCI_DB_SDM_CLOSE    as OCI_DB_SDM_CLOSE
                                   , OCI_DB_SDM_DISMOUNT as OCI_DB_SDM_DISMOUNT
                                   , OCI_DB_SDM_FULL     as OCI_DB_SDM_FULL
                                   } deriving (Eq, Ord, Show) #}

-- database shutdown flags --

{#enum define DatabaseShutdownFlag { OCI_DB_SDF_DEFAULT     as OCI_DB_SDF_DEFAULT
                                   , OCI_DB_SDF_TRANS       as OCI_DB_SDF_TRANS
                                   , OCI_DB_SDF_TRANS_LOCAL as OCI_DB_SDF_TRANS_LOCAL
                                   , OCI_DB_SDF_IMMEDIATE   as OCI_DB_SDF_IMMEDIATE
                                   , OCI_DB_SDF_ABORT       as OCI_DB_SDF_ABORT
                                   } deriving (Eq, Ord, Show) #}

-- charset form types --
{#enum define CharsetFormType { OCI_CSF_NONE     as OCI_CSF_NONE
                              , OCI_CSF_DEFAULT  as OCI_CSF_DEFAULT
                              , OCI_CSF_NATIONAL as OCI_CSF_NATIONAL
                              } deriving (Eq, Ord, Show) #}

-- statement fetch mode --

{#enum define StatementFetchMode { OCI_SFM_DEFAULT    as OCI_SFM_DEFAULT
                                 , OCI_SFM_SCROLLABLE as OCI_SFM_SCROLLABLE
                                 } deriving (Eq, Ord, Show) #}

-- statement fetch direction --

{#enum define StatementFetchDirection { OCI_SFD_ABSOLUTE as OCI_SFD_ABSOLUTE
                                      , OCI_SFD_RELATIVE as OCI_SFD_RELATIVE
                                      } deriving (Eq, Ord, Show) #}

-- bind allocation mode --

{#enum define BindAllocationMode { OCI_BAM_EXTERNAL as OCI_BAM_EXTERNAL
                                 , OCI_BAM_INTERNAL as OCI_BAM_INTERNAL
                                 } deriving (Eq, Ord, Show) #}

-- bind direction mode --

{#enum define BindDirectionMode { OCI_BDM_IN     as OCI_BDM_IN
                                , OCI_BDM_OUT    as OCI_BDM_OUT
                                , OCI_BDM_IN_OUT as OCI_BDM_IN_OUT
                                } deriving (Eq, Ord, Show) #}

-- Column property flags --

{#enum define ColumnPropertyFlag { OCI_CPF_NONE                      as OCI_CPF_NONE
                                 , OCI_CPF_IS_IDENTITY               as OCI_CPF_IS_IDENTITY
                                 , OCI_CPF_IS_GEN_ALWAYS             as OCI_CPF_IS_GEN_ALWAYS
                                 , OCI_CPF_IS_GEN_BY_DEFAULT_ON_NULL as OCI_CPF_IS_GEN_BY_DEFAULT_ON_NULL
                                 } deriving (Eq, Ord, Show) #}

{- See OCI_SUBTYPE
-- Integer sign flag --

OCI_NUM_UNSIGNED                    2

-- External Integer types --

OCI_NUM_SHORT                       4
OCI_NUM_INT                         8
OCI_NUM_BIGINT                      16
OCI_NUM_FLOAT                       32
OCI_NUM_DOUBLE                      64

OCI_NUM_USHORT                      (OCI_NUM_SHORT  | OCI_NUM_UNSIGNED)
OCI_NUM_UINT                        (OCI_NUM_INT    | OCI_NUM_UNSIGNED)
OCI_NUM_BIGUINT                     (OCI_NUM_BIGINT | OCI_NUM_UNSIGNED)

-}

-- timestamp types --

{#enum define TimestampType { OCI_TIMESTAMP     as OCI_TIMESTAMP
                            , OCI_TIMESTAMP_TZ  as OCI_TIMESTAMP_TZ
                            , OCI_TIMESTAMP_LTZ as OCI_TIMESTAMP_LTZ
                            } deriving (Eq, Ord, Show) #}

-- interval types --
{#enum define IntervalType { OCI_INTERVAL_YM as OCI_INTERVAL_YM
                           , OCI_INTERVAL_DS as OCI_INTERVAL_DS
                           } deriving (Eq, Ord, Show) #}

-- long types --

{#enum define LongType { OCI_BLONG as OCI_BLONG
                       , OCI_CLONG as OCI_CLONG
                       } deriving (Eq, Ord, Show) #}

-- lob types --

{#enum define LobType { OCI_BLOB as OCI_BLOB
                      , OCI_CLOB as OCI_CLOB
                      , OCI_NCLOB as OCI_NCLOB
                      } deriving (Eq, Ord, Show) #}

-- lob opening mode --

{#enum define LobOpeningMode { OCI_LOB_READONLY as OCI_LOB_READONLY
                             , OCI_LOB_READWRITE as OCI_LOB_READWRITE
                             } deriving (Eq, Ord, Show) #}

-- file types --

{#enum define FileType { OCI_BFILE as OCI_BFILE
                       , OCI_CFILE as OCI_CFILE
                       } deriving (Eq, Ord, Show) #}

-- lob browsing mode --

{#enum define LobBrowsingMode { OCI_SEEK_SET as OCI_SEEK_SET
                              , OCI_SEEK_END as OCI_SEEK_END
                              , OCI_SEEK_CUR as OCI_SEEK_CUR
                              } deriving (Eq, Ord, Show) #}

-- type info types --

{#enum define TypeInfoType { OCI_UNKNOWN as OCI_UNKNOWN
                           , OCI_TIF_TABLE as OCI_TIF_TABLE
                           , OCI_TIF_VIEW  as OCI_TIF_VIEW
                           , OCI_TIF_TYPE  as OCI_TIF_TYPE
                           } deriving (Eq, Ord, Show) #}

-- object type --

{#enum define ObjectType { OCI_OBJ_PERSISTENT as OCI_OBJ_PERSISTENT
                         , OCI_OBJ_TRANSIENT  as OCI_OBJ_TRANSIENT
                         , OCI_OBJ_VALUE      as OCI_OBJ_VALUE
                         } deriving (Eq, Ord, Show) #}

-- collection types --
{#enum define CollectionType { OCI_COLL_VARRAY        as OCI_COLL_VARRAY
                             , OCI_COLL_NESTED_TABLE  as OCI_COLL_NESTED_TABLE
                             , OCI_COLL_INDEXED_TABLE as OCI_COLL_INDEXED_TABLE
                             } deriving (Eq, Ord, Show) #}

-- pool types --
{#enum define PoolType { OCI_POOL_CONNECTION as OCI_POOL_CONNECTION
                       , OCI_POOL_SESSION as OCI_POOL_SESSION
                       } deriving (Eq, Ord, Show) #}

-- AQ message state --
{#enum define AQMessageState { OCI_AMS_READY as OCI_AMS_READY
                             , OCI_AMS_WAITING as OCI_AMS_WAITING
                             , OCI_AMS_PROCESSED as OCI_AMS_PROCESSED
                             , OCI_AMS_EXPIRED as OCI_AMS_EXPIRED
                             } deriving (Eq, Ord, Show) #}

-- AQ sequence deviation --
{#enum define AQSequenceDeviation { OCI_ASD_BEFORE as OCI_ASD_BEFORE
                                  , OCI_ASD_TOP as OCI_ASD_TOP
                                  } deriving (Eq, Ord, Show) #}

{-
-- AQ message visibility --

OCI_AMV_IMMEDIATE as OCI_AMV_IMMEDIATE
OCI_AMV_ON_COMMIT as OCI_AMV_ON_COMMIT

-- AQ dequeue mode --

OCI_ADM_BROWSE        as OCI_ADM_BROWSE
OCI_ADM_LOCKED        as OCI_ADM_LOCKED
OCI_ADM_REMOVE        as OCI_ADM_REMOVE
OCI_ADM_REMOVE_NODATA as OCI_ADM_REMOVE_NODATA

-- AQ dequeue navigation --

OCI_ADN_FIRST_MSG        as OCI_ADN_FIRST_MSG
OCI_ADN_NEXT_TRANSACTION as OCI_ADN_NEXT_TRANSACTION
OCI_ADN_NEXT_MSG         as OCI_ADN_NEXT_MSG

-- AQ queue table purge mode --

OCI_APM_BUFFERED   as OCI_APM_BUFFERED
OCI_APM_PERSISTENT as OCI_APM_PERSISTENT
OCI_APM_ALL        as OCI_APM_ALL

-- AQ queue table grouping mode --

OCI_AGM_NONE           as OCI_AGM_NONE
OCI_AGM_TRANSACTIONNAL as OCI_AGM_TRANSACTIONNAL

-- AQ queue table type --

OCI_AQT_NORMAL         as OCI_AQT_NORMAL
OCI_AQT_EXCEPTION      as OCI_AQT_EXCEPTION
OCI_AQT_NON_PERSISTENT as OCI_AQT_NON_PERSISTENT


-- direct path processing return status --

OCI_DPR_COMPLETE as OCI_DPR_COMPLETE
OCI_DPR_ERROR    as OCI_DPR_ERROR
OCI_DPR_FULL     as OCI_DPR_FULL
OCI_DPR_PARTIAL  as OCI_DPR_PARTIAL
OCI_DPR_EMPTY    as OCI_DPR_EMPTY

-- direct path conversion modes --

OCI_DCM_DEFAULT as OCI_DCM_DEFAULT
OCI_DCM_FORCE   as OCI_DCM_FORCE

-- trace size constants --

OCI_SIZE_TRACE_ID     as OCI_SIZE_TRACE_ID
OCI_SIZE_TRACE_MODULE as OCI_SIZE_TRACE_MODULE
OCI_SIZE_TRACE_ACTION as OCI_SIZE_TRACE_ACTION
OCI_SIZE_TRACE_INFO   as OCI_SIZE_TRACE_INFO

-- trace types --

OCI_TRC_IDENTITY as OCI_TRC_IDENTITY
OCI_TRC_MODULE   as OCI_TRC_MODULE
OCI_TRC_ACTION   as OCI_TRC_ACTION
OCI_TRC_DETAIL   as OCI_TRC_DETAIL

-- HA event type --

OCI_HET_DOWN as OCI_HET_DOWN
OCI_HET_UP   as OCI_HET_UP

-- HA event source --
OCI_HES_INSTANCE       as OCI_HES_INSTANCE
OCI_HES_DATABASE       as OCI_HES_DATABASE
OCI_HES_NODE           as OCI_HES_NODE
OCI_HES_SERVICE        as OCI_HES_SERVICE
OCI_HES_SERVICE_MEMBER as OCI_HES_SERVICE_MEMBER
OCI_HES_ASM_INSTANCE   as OCI_HES_ASM_INSTANCE
OCI_HES_PRECONNECT     as OCI_HES_PRECONNECT

-- Fail over types --

OCI_FOT_NONE    as OCI_FOT_NONE
OCI_FOT_SESSION as OCI_FOT_SESSION
OCI_FOT_SELECT  as OCI_FOT_SELECT

-- fail over notifications --

OCI_FOE_END    as OCI_FOE_END
OCI_FOE_ABORT  as OCI_FOE_ABORT
OCI_FOE_REAUTH as OCI_FOE_REAUTH
OCI_FOE_BEGIN  as OCI_FOE_BEGIN
OCI_FOE_ERROR  as OCI_FOE_ERROR

-- fail over callback return code --

OCI_FOC_OK    as OCI_FOC_OK
OCI_FOC_RETRY as OCI_FOC_RETRY

-- hash tables support --

OCI_HASH_STRING  as OCI_HASH_STRING
OCI_HASH_INTEGER as OCI_HASH_INTEGER
OCI_HASH_POINTER as OCI_HASH_POINTER

-- transaction types --

OCI_TRS_NEW          as OCI_TRS_NEW
OCI_TRS_READONLY     as OCI_TRS_READONLY
OCI_TRS_READWRITE    as OCI_TRS_READWRITE
OCI_TRS_SERIALIZABLE as OCI_TRS_SERIALIZABLE
OCI_TRS_LOOSE        as OCI_TRS_LOOSE
OCI_TRS_TIGHT        as OCI_TRS_TIGHT

-- format types --
OCI_FMT_DATE          as OCI_FMT_DATE
OCI_FMT_TIMESTAMP     as OCI_FMT_TIMESTAMP
OCI_FMT_NUMERIC       as OCI_FMT_NUMERIC
OCI_FMT_BINARY_DOUBLE as OCI_FMT_BINARY_DOUBLE
OCI_FMT_BINARY_FLOAT  as OCI_FMT_BINARY_FLOAT

-- sql function codes --
OCI_SFC_CREATE_TABLE             as OCI_SFC_CREATE_TABLE
OCI_SFC_SET_ROLE                 as OCI_SFC_SET_ROLE
OCI_SFC_INSERT                   as OCI_SFC_INSERT
OCI_SFC_SELECT                   as OCI_SFC_SELECT
OCI_SFC_UPDATE                   as OCI_SFC_UPDATE
OCI_SFC_DROP_ROLE                as OCI_SFC_DROP_ROLE
OCI_SFC_DROP_VIEW                as OCI_SFC_DROP_VIEW
OCI_SFC_DROP_TABLE               as OCI_SFC_DROP_TABLE
OCI_SFC_DELETE                   as OCI_SFC_DELETE
OCI_SFC_CREATE_VIEW              as OCI_SFC_CREATE_VIEW
OCI_SFC_DROP_USER                as OCI_SFC_DROP_USER
OCI_SFC_CREATE_ROLE              as OCI_SFC_CREATE_ROLE
OCI_SFC_CREATE_SEQUENCE          as OCI_SFC_CREATE_SEQUENCE
OCI_SFC_ALTER_SEQUENCE           as OCI_SFC_ALTER_SEQUENCE
OCI_SFC_DROP_SEQUENCE            as OCI_SFC_DROP_SEQUENCE
OCI_SFC_CREATE_SCHEMA            as OCI_SFC_CREATE_SCHEMA
OCI_SFC_CREATE_CLUSTER           as OCI_SFC_CREATE_CLUSTER
OCI_SFC_CREATE_USER              as OCI_SFC_CREATE_USER
OCI_SFC_CREATE_INDEX             as OCI_SFC_CREATE_INDEX
OCI_SFC_DROP_INDEX               as OCI_SFC_DROP_INDEX
OCI_SFC_DROP_CLUSTER             as OCI_SFC_DROP_CLUSTER
OCI_SFC_VALIDATE_INDEX           as OCI_SFC_VALIDATE_INDEX
OCI_SFC_CREATE_PROCEDURE         as OCI_SFC_CREATE_PROCEDURE
OCI_SFC_ALTER_PROCEDURE          as OCI_SFC_ALTER_PROCEDURE
OCI_SFC_ALTER_TABLE              as OCI_SFC_ALTER_TABLE
OCI_SFC_EXPLAIN                  as OCI_SFC_EXPLAIN
OCI_SFC_GRANT                    as OCI_SFC_GRANT
OCI_SFC_REVOKE                   as OCI_SFC_REVOKE
OCI_SFC_CREATE_SYNONYM           as OCI_SFC_CREATE_SYNONYM
OCI_SFC_DROP_SYNONYM             as OCI_SFC_DROP_SYNONYM
OCI_SFC_ALTER_SYSTEM_SWITCHLOG   as OCI_SFC_ALTER_SYSTEM_SWITCHLOG
OCI_SFC_SET_TRANSACTION          as OCI_SFC_SET_TRANSACTION
OCI_SFC_PLSQL_EXECUTE            as OCI_SFC_PLSQL_EXECUTE
OCI_SFC_LOCK                     as OCI_SFC_LOCK
OCI_SFC_NOOP                     as OCI_SFC_NOOP
OCI_SFC_RENAME                   as OCI_SFC_RENAME
OCI_SFC_COMMENT                  as OCI_SFC_COMMENT
OCI_SFC_AUDIT                    as OCI_SFC_AUDIT
OCI_SFC_NO_AUDIT                 as OCI_SFC_NO_AUDIT
OCI_SFC_ALTER_INDEX              as OCI_SFC_ALTER_INDEX
OCI_SFC_CREATE_EXTERNAL_DATABASE as OCI_SFC_CREATE_EXTERNAL_DATABASE
OCI_SFC_DROP_EXTERNALDATABASE    as OCI_SFC_DROP_EXTERNALDATABASE
OCI_SFC_CREATE_DATABASE          as OCI_SFC_CREATE_DATABASE
OCI_SFC_ALTER_DATABASE           as OCI_SFC_ALTER_DATABASE
OCI_SFC_CREATE_ROLLBACK_SEGMENT  as OCI_SFC_CREATE_ROLLBACK_SEGMENT
OCI_SFC_ALTER_ROLLBACK_SEGMENT   as OCI_SFC_ALTER_ROLLBACK_SEGMENT
OCI_SFC_DROP_ROLLBACK_SEGMENT    as OCI_SFC_DROP_ROLLBACK_SEGMENT
OCI_SFC_CREATE_TABLESPACE        as OCI_SFC_CREATE_TABLESPACE
OCI_SFC_ALTER_TABLESPACE         as OCI_SFC_ALTER_TABLESPACE
OCI_SFC_DROP_TABLESPACE          as OCI_SFC_DROP_TABLESPACE
OCI_SFC_ALTER_SESSION            as OCI_SFC_ALTER_SESSION
OCI_SFC_ALTER_USER               as OCI_SFC_ALTER_USER
OCI_SFC_COMMIT_WORK              as OCI_SFC_COMMIT_WORK
OCI_SFC_ROLLBACK                 as OCI_SFC_ROLLBACK
OCI_SFC_SAVEPOINT                as OCI_SFC_SAVEPOINT
OCI_SFC_CREATE_CONTROL_FILE      as OCI_SFC_CREATE_CONTROL_FILE
OCI_SFC_ALTER_TRACING            as OCI_SFC_ALTER_TRACING
OCI_SFC_CREATE_TRIGGER           as OCI_SFC_CREATE_TRIGGER
OCI_SFC_ALTER_TRIGGER            as OCI_SFC_ALTER_TRIGGER
OCI_SFC_DROP_TRIGGER             as OCI_SFC_DROP_TRIGGER
OCI_SFC_ANALYZE_TABLE            as OCI_SFC_ANALYZE_TABLE
OCI_SFC_ANALYZE_INDEX            as OCI_SFC_ANALYZE_INDEX
OCI_SFC_ANALYZE_CLUSTER          as OCI_SFC_ANALYZE_CLUSTER
OCI_SFC_CREATE_PROFILE           as OCI_SFC_CREATE_PROFILE
OCI_SFC_DROP_PROFILE             as OCI_SFC_DROP_PROFILE
OCI_SFC_ALTER_PROFILE            as OCI_SFC_ALTER_PROFILE
OCI_SFC_DROP_PROCEDURE           as OCI_SFC_DROP_PROCEDURE
OCI_SFC_ALTER_RESOURCE_COST      as OCI_SFC_ALTER_RESOURCE_COST
OCI_SFC_CREATE_SNAPSHOT_LOG      as OCI_SFC_CREATE_SNAPSHOT_LOG
OCI_SFC_ALTER_SNAPSHOT_LOG       as OCI_SFC_ALTER_SNAPSHOT_LOG
OCI_SFC_DROP_SNAPSHOT_LOG        as OCI_SFC_DROP_SNAPSHOT_LOG
OCI_SFC_DROP_SUMMARY             as OCI_SFC_DROP_SUMMARY
OCI_SFC_CREATE_SNAPSHOT          as OCI_SFC_CREATE_SNAPSHOT
OCI_SFC_ALTER_SNAPSHOT           as OCI_SFC_ALTER_SNAPSHOT
OCI_SFC_DROP_SNAPSHOT            as OCI_SFC_DROP_SNAPSHOT
OCI_SFC_CREATE_TYPE              as OCI_SFC_CREATE_TYPE
OCI_SFC_DROP_TYPE                as OCI_SFC_DROP_TYPE
OCI_SFC_ALTER_ROLE               as OCI_SFC_ALTER_ROLE
OCI_SFC_ALTER_TYPE               as OCI_SFC_ALTER_TYPE
OCI_SFC_CREATE_TYPE_BODY         as OCI_SFC_CREATE_TYPE_BODY
OCI_SFC_ALTER_TYPE_BODY          as OCI_SFC_ALTER_TYPE_BODY
OCI_SFC_DROP_TYPE_BODY           as OCI_SFC_DROP_TYPE_BODY
OCI_SFC_DROP_LIBRARY             as OCI_SFC_DROP_LIBRARY
OCI_SFC_TRUNCATE_TABLE           as OCI_SFC_TRUNCATE_TABLE
OCI_SFC_TRUNCATE_CLUSTER         as OCI_SFC_TRUNCATE_CLUSTER
OCI_SFC_CREATE_BITMAPFILE        as OCI_SFC_CREATE_BITMAPFILE
OCI_SFC_ALTER_VIEW               as OCI_SFC_ALTER_VIEW
OCI_SFC_DROP_BITMAPFILE          as OCI_SFC_DROP_BITMAPFILE
OCI_SFC_SET_CONSTRAINTS          as OCI_SFC_SET_CONSTRAINTS
OCI_SFC_CREATE_FUNCTION          as OCI_SFC_CREATE_FUNCTION
OCI_SFC_ALTER_FUNCTION           as OCI_SFC_ALTER_FUNCTION
OCI_SFC_DROP_FUNCTION            as OCI_SFC_DROP_FUNCTION
OCI_SFC_CREATE_PACKAGE           as OCI_SFC_CREATE_PACKAGE
OCI_SFC_ALTER_PACKAGE            as OCI_SFC_ALTER_PACKAGE
OCI_SFC_DROP_PACKAGE             as OCI_SFC_DROP_PACKAGE
OCI_SFC_CREATE_PACKAGE_BODY      as OCI_SFC_CREATE_PACKAGE_BODY
OCI_SFC_ALTER_PACKAGE_BODY       as OCI_SFC_ALTER_PACKAGE_BODY
OCI_SFC_DROP_PACKAGE_BODY        as OCI_SFC_DROP_PACKAGE_BODY
OCI_SFC_CREATE_DIRECTORY         as OCI_SFC_CREATE_DIRECTORY
OCI_SFC_DROP_DIRECTORY           as OCI_SFC_DROP_DIRECTORY
OCI_SFC_CREATE_LIBRARY           as OCI_SFC_CREATE_LIBRARY
OCI_SFC_CREATE_JAVA              as OCI_SFC_CREATE_JAVA
OCI_SFC_ALTER_JAVA               as OCI_SFC_ALTER_JAVA
OCI_SFC_DROP_JAVA                as OCI_SFC_DROP_JAVA
OCI_SFC_CREATE_OPERATOR          as OCI_SFC_CREATE_OPERATOR
OCI_SFC_CREATE_INDEXTYPE         as OCI_SFC_CREATE_INDEXTYPE
OCI_SFC_DROP_INDEXTYPE           as OCI_SFC_DROP_INDEXTYPE
OCI_SFC_ALTER_INDEXTYPE          as OCI_SFC_ALTER_INDEXTYPE
OCI_SFC_DROP_OPERATOR            as OCI_SFC_DROP_OPERATOR
OCI_SFC_ASSOCIATE_STATISTICS     as OCI_SFC_ASSOCIATE_STATISTICS
OCI_SFC_DISASSOCIATE_STATISTICS  as OCI_SFC_DISASSOCIATE_STATISTICS
OCI_SFC_CALL_METHOD              as OCI_SFC_CALL_METHOD
OCI_SFC_CREATE_SUMMARY           as OCI_SFC_CREATE_SUMMARY
OCI_SFC_ALTER_SUMMARY            as OCI_SFC_ALTER_SUMMARY
OCI_SFC_CREATE_DIMENSION         as OCI_SFC_CREATE_DIMENSION
OCI_SFC_ALTER_DIMENSION          as OCI_SFC_ALTER_DIMENSION
OCI_SFC_DROP_DIMENSION           as OCI_SFC_DROP_DIMENSION
OCI_SFC_CREATE_CONTEXT           as OCI_SFC_CREATE_CONTEXT
OCI_SFC_DROP_CONTEXT             as OCI_SFC_DROP_CONTEXT
OCI_SFC_ALTER_OUTLINE            as OCI_SFC_ALTER_OUTLINE
OCI_SFC_CREATE_OUTLINE           as OCI_SFC_CREATE_OUTLINE
OCI_SFC_DROP_OUTLINE             as OCI_SFC_DROP_OUTLINE
OCI_SFC_UPDATE_INDEXES           as OCI_SFC_UPDATE_INDEXES
OCI_SFC_ALTER_OPERATOR           as OCI_SFC_ALTER_OPERATOR

-}
