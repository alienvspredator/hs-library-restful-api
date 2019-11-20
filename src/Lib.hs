{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   as JSON
import           Data.Aeson.Text
import           Data.Int                     (Int64 (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Data.Time
import           Database.Persist
import           Database.Persist.Sql         (SqlPersistT, runSqlConn)
import           Database.Persist.Sqlite
    ( SqlBackend (..)
    , SqliteConnectionInfo (..)
    , fromSqlKey
    , mkSqliteConnectionInfo
    , runMigration
    , runSqlPool
    , toSqlKey
    , withSqliteConnInfo
    )
import           Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
    )
import           Database.Persist.Types       (PersistValue (PersistInt64))

import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           System.Environment           (getArgs)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Author             json
      Id               Int Primary Unique
      name             Text
      surname          Text
      deriving Eq Show Generic
    Book               json
      Id               Int Primary Unique
      title            Text
      releaseDate      Day
      reader           ReaderId Maybe
      description      Text
      deriving Eq Show Generic
    Reader             json
      Id               Int
      name             Text
      surname          Text
      deriving Eq Show Generic
    Charter            json
      Id               Int
      title            Text
      book             BookId
      deriving Eq Show Generic
    AuthorBook         json
      author           AuthorId
      book             BookId
      Primary          author book
      deriving Eq Show Generic
    History            json
      Id               Int Primary Unique
      returnedDate     Day Maybe
      realReturnedDate Day Maybe
      takenDate        Day
      status           Text
      book             BookId
      reader           ReaderId
      deriving Eq Show Generic
    ReaderCard         json
      Id               Int Primary Unique
      owner            ReaderId
      deriving Eq Show Generic
    |]

type Api
   = "authors" :> Get '[ JSON] [Author] :<|> "authors" :> Capture "id" Int :> Get '[ JSON] Author :<|> "authors" :> Capture "id" Int :> Delete '[ JSON] () :<|> "authors" :> ReqBody '[ JSON] Author :> Post '[ JSON] Author :<|> "readers" :> Get '[ JSON] [Reader] :<|> "readers" :> Capture "id" Int :> Get '[ JSON] Reader :<|> "readers" :> Capture "id" Int :> Delete '[ JSON] () :<|> "readers" :> ReqBody '[ JSON] Reader :> Post '[ JSON] Reader :<|> "books" :> Get '[ JSON] [Book] :<|> "books" :> Capture "id" Int :> Get '[ JSON] Book :<|> "books" :> Capture "id" Int :> Delete '[ JSON] () :<|> "books" :> ReqBody '[ JSON] Book :> Post '[ JSON] Book

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> Handler a
runDB a = liftIO $ runNoLoggingT $ runResourceT $ withSqliteConnInfo connInfo $ runSqlConn a

connInfo :: SqliteConnectionInfo
connInfo = mkSqliteConnectionInfo "test.db3"

doMigration :: IO ()
doMigration = runNoLoggingT $ runResourceT $ withSqliteConnInfo connInfo $ runReaderT $ runMigration migrateAll

server :: Server Api
server =
  authorGET :<|> authorGETById :<|> authorDELETE :<|> authorPOST :<|> readerGET :<|> readerGETById :<|> readerDELETE :<|>
  readerPOST :<|>
  bookGET :<|>
  bookGETById :<|>
  bookDELETE :<|>
  bookPOST
  where
    authorGET = selectAuthors
    authorGETById = selectAuthorById
    authorDELETE = deleteAuthor
    authorPOST = createAuthor
    readerGET = selectReaders
    readerGETById = selectReaderById
    readerDELETE = deleteReader
    readerPOST = createReader
    bookGET = selectBooks
    bookGETById = selectBookById
    bookDELETE = deleteBook
    bookPOST = createBook

selectAuthors :: Handler [Author]
selectAuthors = runDB $ selectList [] [] >>= \x -> return $ map (\(Entity _ u) -> u) x

selectAuthorById :: Int -> Handler Author
selectAuthorById id = do
  sqlResult <- runDB $ get $ AuthorKey id
  case sqlResult of
    Just author -> return author
    Nothing     -> throwError err404 {errBody = "Author with ID not found"}

createAuthor :: Author -> Handler Author
createAuthor author = do
  runDB $ insert author
  return author

deleteAuthor :: Int -> Handler ()
deleteAuthor id = runDB $ delete $ AuthorKey id

selectReaders :: Handler [Reader]
selectReaders = runDB $ selectList [] [] >>= \x -> return $ map (\(Entity _ u) -> u) x

selectReaderById :: Int -> Handler Reader
selectReaderById id = do
  sqlResult <- runDB $ get $ ReaderKey id
  case sqlResult of
    Just reader -> return reader
    Nothing     -> throwError err404 {errBody = "Reader with ID not found"}

deleteReader :: Int -> Handler ()
deleteReader id = runDB $ delete $ ReaderKey id

createReader :: Reader -> Handler Reader
createReader reader = runDB $ insert reader >>= \x -> return reader

selectBooks :: Handler [Book]
selectBooks = runDB $ selectList [] [] >>= \x -> return $ map (\(Entity _ u) -> u) x

selectBookById :: Int -> Handler Book
selectBookById id = do
  sqlResult <- runDB $ get $ BookKey id
  case sqlResult of
    Just book -> return book
    Nothing   -> throwError err404 {errBody = "Book with ID not found"}

deleteBook :: Int -> Handler ()
deleteBook id = runDB $ delete $ BookKey id

createBook :: Book -> Handler Book
createBook book = runDB $ insert book >>= \x -> return book

startApp :: IO ()
startApp = do
  args <- getArgs
  let arg1 =
        if not (null args)
          then Just (head args)
          else Nothing
  case arg1 of
    Just "migrate" -> doMigration
    _              -> run 8080 app
