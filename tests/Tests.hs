{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Database.Ocilib
import Database.Ocilib.Bindings
import Database.Ocilib.Connection
import Database.Ocilib.Collections
import Database.Ocilib.Errors
import Database.Ocilib.Fetch
import Database.Ocilib.Pool
import Database.Ocilib.Statement

import Foreign.Ptr
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "fetchTest" fetchTest
--    , testCase "poolTest" poolTest
--    , testCase "testErrors" testErrors
    , testCase "testStringBinding" testStringBinding
    , testCase "testVarray" testVarray
    ]

fetchTest :: Assertion
fetchTest = do
    e <- ociInitialize Nothing Nothing [OCI_ENV_DEFAULT, OCI_ENV_CONTEXT]
    assertBool "ociInitialize" e

    maybeC <- ociConnectionCreate "localhost:1521/xe" "system" "oracle" OCI_SESSION_DEFAULT
    case maybeC of
        Nothing -> assertFailure "failed to create connection"
        Just c -> do
            Just st <- ociStatementCreate c
            b <- ociExecuteStmt st "select * from test_fetch"
            assertBool "ociExecuteStatement" b

            maybeRs <- ociGetResultset st
            case maybeRs of
                Nothing -> assertFailure "fail to get resultset"
                Just rs -> do
                    ml <- ociFetchNext rs
                    assertBool "ociFetchNext" ml

                    code    <- ociGetInt2    rs "code"
                    article <- ociGetString2 rs "article"
                    price   <- ociGetDouble2 rs "price"
                    date    <- ociGetString2 rs "creation"

                    assertEqual "code"    1 code
                    assertEqual "article" "shoes" article
                    assertEqual "price" 3.14 price
                    assertEqual "Date" "1978-12-23" date

                    _ <- ociStatementFree st
                    _ <- ociConnectionFree c

                    q <- ociCleanup
                    assertBool "ociCleanup" q

--------------------------------------------------------------------------------
poolTest :: Assertion
poolTest = do
    e <- ociInitialize Nothing Nothing [OCI_ENV_DEFAULT, OCI_ENV_THREADED, OCI_ENV_CONTEXT]
    assertBool "ociInitialize" e

    maybeP <- ociPoolCreate "localhost:1521/xe" "system" "oracle" OCI_POOL_CONNECTION OCI_SESSION_DEFAULT 1 2 1
    case maybeP of
        Nothing -> assertFailure "failed to create pool"
        Just p -> do
            maybeC <- ociPoolGetConnection p ""
            case maybeC of
                Nothing -> assertFailure "failed to get Connection"
                Just c ->  do
                    bc <- ociPoolGetBusyCount p
                    assertEqual "ociPoolGetBusyCount" 1 bc
                    _ <- ociConnectionFree c
                    return ()
            f <- ociPoolFree p
            assertBool "ociPoolFree" f
    _ <- ociCleanup
    return ()

--------------------------------------------------------------------------------
testErrors :: Assertion
testErrors = do
    e <- ociInitialize (Just errHandler) Nothing [OCI_ENV_DEFAULT, OCI_ENV_THREADED, OCI_ENV_CONTEXT]
    assertBool "ociInitialize" e

    p <- ociPoolCreate "localhost:1521/xe" "system" "bad" OCI_POOL_CONNECTION OCI_SESSION_DEFAULT 5 10 1
    assertBool "ociPoolCreate" $ isNothing p

    e <- ociGetLastError
    assertBool "got error" $ isJust e

    where
        errHandler :: Ptr OCI_Error -> IO ()
        errHandler e = do
            message <- ociErrorGetString e
            print message


--------------------------------------------------------------------------------
testStringBinding :: Assertion
testStringBinding = do
    _ <- ociInitialize Nothing Nothing [OCI_ENV_DEFAULT, OCI_ENV_CONTEXT]

    maybeC <- ociConnectionCreate "localhost:1521/xe" "system" "oracle" OCI_SESSION_DEFAULT
    case maybeC of
        Nothing -> assertFailure "unable to connect"
        Just c -> do
            Just st <- ociStatementCreate c
            ociSetBindAllocation st OCI_BAM_INTERNAL
            b <- ociPrepare st "select * from test_fetch where article=(:article)"
            assertBool "prepared" b
            b <- ociBindString st "article" nullPtr 30
            assertBool "bound" b
            mBind <- ociGetBind2 st "article"
            case mBind of
                Nothing -> assertFailure "failed to get Bind"
                Just bind -> return ()

            b <- ociExecute st
            assertBool "Execute" b

            maybeRs <- ociGetResultset st
            assertBool "ociGetResultSet" (isJust maybeRs)

            {-
            b <- ociFetchNext rs
            assertBool "ociFetchNext" b

            code    <- ociGetInt2    rs "code"
            article <- ociGetString2 rs "article"
            price   <- ociGetDouble2 rs "price"
            date    <- ociGetString2 rs "creation"

            assertEqual "code"    2 code
            assertEqual "article" "shirt" article
            assertEqual "price" 5.99 price
            assertEqual "Date" "1999-09-12" date
            -}
            {-
            e <- ociGetLastError
            m <- ociErrorGetString e
            print m
            -}

            _ <- ociStatementFree st

            _ <- ociConnectionFree c

            _ <- ociCleanup
            return ()

--------------------------------------------------------------------------------
testVarray :: Assertion
testVarray = do
    i <- ociInitialize Nothing Nothing [OCI_ENV_DEFAULT, OCI_ENV_CONTEXT]
    assertBool "initialization" i

    maybeCn <- ociConnectionCreate "localhost:1521/xe" "system" "oracle" OCI_SESSION_DEFAULT
    let cn = fromJust maybeCn
    mt <- ociTypeInfoGet cn "t_tab1_emp" OCI_TIF_TYPE
    case mt of
        Nothing -> assertFailure "get t_tab1 type"
        Just t_tab1_emp -> do
            mcoll <- ociCollCreate t_tab1_emp
            case mcoll of
                Nothing -> assertFailure "create t_tab_emp coll"
                Just coll -> do
                    me <- ociElemCreate t_tab1_emp
                    case me of
                        Nothing -> assertFailure "create elem"
                        Just elem  -> do
                            b <- ociCollAppend coll elem
                            assertBool "append elem" b
                            s <- ociCollGetSize coll
                            assertEqual "collection size" 1 s

printErrorIfAny :: IO ()
printErrorIfAny = do
    me <- ociGetLastError
    case me of
        Nothing -> return ()
        Just e -> do
            s <- ociErrorGetString e
            print s
