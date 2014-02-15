{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy.UTF8 as B
import Database.Persist.Sqlite hiding (get)
import Network.Wai.Test
import Test.Hspec
import Text.Shakespeare.Text
import Yesod hiding (get)
import Yesod.Paginate
import Yesod.Test

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item
    name String
    deriving Eq Read Show
|]

data TestApp = TestApp ConnectionPool
instance Yesod TestApp

mkYesod "TestApp" [parseRoutes|
/items ItemsR GET
|]

instance YesodPersist TestApp where
    type YesodPersistBackend TestApp = SqlPersistT

    runDB act = do
        TestApp p <- getYesod
        runSqlPool act p

getItemsR :: HandlerT TestApp IO TypedContent
getItemsR = do
    (items :: Page Item) <- paginate return
    selectRep . provideRep $ return [stext|#{show items}|]

main :: IO ()
main = withSqlitePool ":memory:" 1 $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $
        runMigration migrateAll
    hspec $ yesodSpec (TestApp pool) $
        ydescribe "pages" $ do
            yit "with nothing" $ do
                clearOut pool
                wantPage $ Page [] 0 Nothing Nothing

            yit "with some items" $ do
                clearOut pool
                k <- liftIO $ runSqlPersistMPool (insert $ Item "hello, world!") pool
                wantPage $ Page [Entity k (Item "hello, world!")] 1 Nothing Nothing

wantPage :: Page Item -> YesodExample TestApp ()
wantPage p = do
    get ItemsR
    withResponse $ \SResponse { simpleBody } ->
        liftIO $ read (B.toString simpleBody) `shouldBe` p

clearOut pool = liftIO $ runSqlPersistMPool (deleteWhere ([] :: [Filter Item])) pool
