{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Easy pagination for Yesod.
module Yesod.Paginate (
    -- *** Paginating
    paginate, paginateWith, paginateWithConfig,

    -- *** Datatypes
    PageConfig(..), def,
    Page(..)
) where

import Control.Monad
import Data.Default
import Data.Int
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Read as R
import Database.Esqueleto
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Prelude
import Text.Shakespeare.Text
import Yesod hiding (Value)

-- | Which page we're on, and how big it is.
--
-- 'paginate' and 'paginateWith' build this datatype based on the current
-- query string parameters. Use 'paginateWithConfig' to provide your own.
data PageConfig = PageConfig
                { pageSize :: Int64
                , currentPage :: Int64
                } deriving Show

instance Default PageConfig where
    def = PageConfig { pageSize = 10, currentPage = 1 }

-- | Returned by 'paginate' and friends.
data Page r = Page
            { pageResults :: [r] -- ^ Returned entities.
            , pageCount :: Int64 -- ^ Total number of pages. This will be at minimum 1, even for an empty result set.
            , nextPage :: Maybe Text -- ^ Link to next page, pre-rendered.
            , previousPage :: Maybe Text -- ^ Link to previous page, pre-rendered.
            } deriving (Eq, Read, Show)

-- | Paginate a model using default options - nothing special.
paginate :: (From SqlQuery SqlExpr SqlBackend a, RenderRoute site,
             YesodPersist site, SqlSelect a r,
             YesodPersistBackend site ~ SqlPersistT)
         => HandlerT site IO (Page r) -- ^ Returned page.
paginate = paginateWith return

-- | Paginate a model, given an esqueleto query.
paginateWith :: (From SqlQuery SqlExpr SqlBackend t, SqlSelect a r,
                 RenderRoute site, YesodPersist site,
                 YesodPersistBackend site ~ SqlPersistT)
             => (t -> SqlQuery a) -- ^ SQL query.
             -> HandlerT site IO (Page r) -- ^ Returned page.
paginateWith sel = do
    params <- liftM2 (\a b -> fst a ++ reqGetParams b)
        runRequestBody getRequest

    let currentPage = maybe 1 (fromMaybe 1 . decimalM)
                    $ lookup "page" params
        pageSize = within (5, 50)
           . maybe 10 (fromMaybe 10 . decimalM)
           $ lookup "count" params

    paginateWithConfig def { pageSize, currentPage } sel

-- | Paginate a model, given a configuration and an esqueleto query.
paginateWithConfig :: (From SqlQuery SqlExpr SqlBackend t, SqlSelect a r,
                       RenderRoute site, YesodPersist site,
                       YesodPersistBackend site ~ SqlPersistT)
                   => PageConfig -- ^ Preferred config.
                   -> (t -> SqlQuery a) -- ^ SQL query.
                   -> HandlerT site IO (Page r) -- ^ Returned page.
paginateWithConfig c sel = do
    let filterStmt u = limit (pageSize c) >> return u

    [ct] <- runDB $ select $ from $ \u -> do
        _ <- filterStmt u -- does nothing, used for type constraint
        return (countRows :: SqlExpr (Value Int64))

    let maxPage = max 1 $ (unValue ct + pageSize c - 1) `div` pageSize c
        cp = within (1, maxPage) $ currentPage c

    es <- runDB $ select $ from $ \u -> do
        _ <- filterStmt u
        offset $ within (0, max 0 $ unValue ct - 1) $ pageSize c * (cp - 1)
        sel u

    rt' <- getCurrentRoute
    rend <- getUrlRenderParams

    let rt = fromMaybe (error "Attempting to use paginate on a server error page.") rt'
        qs = snd $ renderRoute rt
        np = rend rt $ updateQs qs ("page", [st|#{cp + 1}|])
        pp = rend rt $ updateQs qs ("page", [st|#{cp - 1}|])

    return Page { pageResults = es
                , pageCount = maxPage
                , nextPage = if cp == maxPage then Nothing else Just np
                , previousPage = if cp == 1 then Nothing else Just pp
                }
    where
        unValue (Value a) = a
        updateQs ((a,b):as) (k,v) | k == a = (k,v):as
                                  | otherwise = (a,b):updateQs as (k,v)
        updateQs [] (k,v) = [(k,v)]

decimalM :: Integral a => Text -> Maybe a
decimalM t = case R.decimal t of
    Right (i, _) -> Just i
    Left _ -> Nothing

within :: Ord a => (a,a) -> a -> a
within (a,b) _ | b < a = error "within error"
within (a,b) q | q <= a = a
               | q >= b = b
               | otherwise = q
