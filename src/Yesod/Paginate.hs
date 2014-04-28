{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Easy pagination for Yesod.
module Yesod.Paginate (
    -- *** Paginating
    paginate, paginateWith,

    -- *** Datatypes
    PageConfig(..),
    Page(..)
) where

import Control.Monad
import Data.Int
import Data.Maybe
import Database.Esqueleto
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Prelude
import Yesod hiding (Value)

-- | Metadata about how pagination should work.
data PageConfig app = PageConfig
    { pageSize :: Int
    , currentPage :: Int
    , firstPageRoute :: Route app
    , pageRoute :: Int -> Route app
    }

-- | Returned by 'paginate' and friends.
data Page route r = Page
    { pageResults :: [r] -- ^ Returned entities.
    , firstPage :: Maybe route -- ^ Link to first page.
    , nextPage :: Maybe route -- ^ Link to next page.
    , previousPage :: Maybe route -- ^ Link to previous page.
    } deriving (Eq, Read, Show)

-- | Paginate a model, given a configuration. This just performs a @SELECT
-- *@.
paginate :: (YesodPersist site, SqlSelect a s,
             MonadResource (YesodPersistBackend site (HandlerT site IO)),
             From SqlQuery SqlExpr SqlBackend a,
             MonadSqlPersist (YesodPersistBackend site (HandlerT site IO)))
         => PageConfig site -- ^ Preferred config.
         -> HandlerT site IO (Page (Route site) s) -- ^ Returned page.
paginate c = paginateWith c return

-- | Paginate a model, given a configuration and an esqueleto query.
paginateWith :: (YesodPersist site, SqlSelect a s,
                 MonadResource (YesodPersistBackend site (HandlerT site IO)),
                 From SqlQuery SqlExpr SqlBackend q,
                 MonadSqlPersist (YesodPersistBackend site (HandlerT site IO)))
             => PageConfig site -- ^ Preferred config.
             -> (q -> SqlQuery a) -- ^ SQL query.
             -> HandlerT site IO (Page (Route site) s) -- ^ Returned page.
paginateWith c sel = do
    let cp = max 1 $ fromIntegral (currentPage c)

    es <- runDB $ select $ from $ \u -> do
        limit (fromIntegral (pageSize c) + 1)
        offset $ max 0 $ fromIntegral (pageSize c) * (cp - 1)
        sel u

    let route = pageRoute c . fromIntegral

    return Page { pageResults = take (fromIntegral $ pageSize c) es
                , firstPage = if cp >= 2 then Just (firstPageRoute c) else Nothing
                , nextPage = if fromIntegral (length es) == pageSize c + 1
                                 then Just (route $ cp + 1)
                                 else Nothing
                , previousPage = if cp == 1 then Nothing else Just (route $ cp - 1)
                }
