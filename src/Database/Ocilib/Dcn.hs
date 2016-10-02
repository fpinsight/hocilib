{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module:      Database.Ocilib.Dcn
-- Copyright:   (c) 2016 Thierry Bourrillon
--              (c) 2016 FPInsight, Eurl.
-- License:     BSD3
-- Maintainer:  Thierry Bourrillon <thierry.bourrillon@fpinsight.com>
-- Stability:   experimental
-- Portability: portable
--
--

module Database.Ocilib.Dcn
    ( ociSubscriptionRegister
    , ociSubscriptionUnregister
    , ociSubscriptionAddStatement
    , ociSubscriptionGetName
    , ociSubscriptionGetPort
    , ociSubscriptionGetTimeout
    , ociSubscriptionGetConnection
    , ociEventGetType
    , ociEventGetOperation
    , ociEventGetDatabase
    , ociEventGetObject
    , ociEventGetRowId
    , ociEventGetSubscription
    ) where

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

type NotifyHandler = Ptr OCI_Event -> IO ()
type Subscription = Ptr OCI_Subscription


-- | Register a notification against the given database.
ociSubscriptionRegister :: Ptr OCI_Connection
                        -> ByteString            -- ^ name
                        -> EventNotificationType
                        -> NotifyHandler
                        -> CUInt                 -- ^ port
                        -> CUInt                 -- ^ timeout
                        -> IO (Maybe Subscription)
ociSubscriptionRegister conn name ent handler port timeout = do
    let ent' = fromIntegral $ fromEnum ent
    useAsCString name (\name' ->
        fmap toMaybePtr [C.exp| OCI_Subscription* { OCI_SubscriptionRegister( $(OCI_Connection *conn)
                                                                            , $(const char *name')
                                                                            , $(unsigned int ent')
                                                                            , $fun:(void (*handler)(OCI_Event*))
                                                                            , $(unsigned int port)
                                                                            , $(unsigned int timeout)) } |]
        )

-- | Unregister a previously registered notification.
ociSubscriptionUnregister :: Subscription -> IO Bool
ociSubscriptionUnregister s =
    fmap toBool [C.exp| int { OCI_SubscriptionUnregister($(OCI_Subscription *s)) } |]

-- | Add a statement to the notification to monitor.
ociSubscriptionAddStatement :: Subscription -> Ptr OCI_Statement -> IO Bool
ociSubscriptionAddStatement su st =
    fmap toBool [C.exp| int { OCI_SubscriptionAddStatement($(OCI_Subscription *su), $(OCI_Statement *st)) } |]

-- | Return the name of the given registered subscription.
ociSubscriptionGetName :: Subscription -> IO ByteString
ociSubscriptionGetName su = do
    n <- [C.exp| const char * { OCI_SubscriptionGetName($(OCI_Subscription *su)) } |]
    packCString n

-- | Return the port used by the notification.
ociSubscriptionGetPort :: Subscription -> IO CUInt
ociSubscriptionGetPort su =
    [C.exp| unsigned int { OCI_SubscriptionGetPort($(OCI_Subscription *su)) } |]

-- | Return the timeout of the given registered subscription.
ociSubscriptionGetTimeout :: Subscription -> IO CUInt
ociSubscriptionGetTimeout su =
    [C.exp| unsigned int { OCI_SubscriptionGetTimeout($(OCI_Subscription *su)) } |]

-- | Return the connection handle associated with a subscription handle.
ociSubscriptionGetConnection :: Subscription -> IO (Ptr OCI_Connection)
ociSubscriptionGetConnection su =
    [C.exp| OCI_Connection * { OCI_SubscriptionGetConnection($(OCI_Subscription *su)) } |]

-- | Return the type of event reported by a notification.
ociEventGetType :: Ptr OCI_Event -> IO EventNotificationType
ociEventGetType e =
    fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_EventGetType($(OCI_Event *e)) } |]

-- | Return the type of operation reported by a notification.
ociEventGetOperation :: Ptr OCI_Event -> IO EventObjectNotificationType
ociEventGetOperation e =
    fmap (toEnum . fromIntegral) [C.exp| unsigned int { OCI_EventGetOperation($(OCI_Event *e)) } |]

-- | Return the name of the database that generated the event.
ociEventGetDatabase :: Ptr OCI_Event -> IO ByteString
ociEventGetDatabase e = do
    db <- [C.exp| const char* { OCI_EventGetDatabase($(OCI_Event *e)) } |]
    packCString db

-- | Return the name of the object that generated the event.
ociEventGetObject :: Ptr OCI_Event -> IO ByteString
ociEventGetObject e = do
    o <- [C.exp| const char* { OCI_EventGetObject($(OCI_Event *e)) } |]
    packCString o

-- | Return the rowid of the altered database object row.
ociEventGetRowId :: Ptr OCI_Event -> IO ByteString
ociEventGetRowId e = do
    rid <- [C.exp| const char* { OCI_EventGetRowid($(OCI_Event *e)) } |]
    packCString rid

-- | Return the subscription handle that generated this event.
ociEventGetSubscription :: Ptr OCI_Event -> IO Subscription
ociEventGetSubscription e =
    [C.exp| OCI_Subscription* { OCI_EventGetSubscription($(OCI_Event *e)) } |]
