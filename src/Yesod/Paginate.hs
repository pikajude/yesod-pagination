{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Paginate (
    PageConfig(..),
    Page(..),
    paginate, paginateWith
) where

import Control.Monad
import Data.Default
import Data.Int
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Read as R
import Database.Esqueleto
import Database.Esqueleto.Internal.Language
import Debug.Trace
import Prelude
import Text.Shakespeare.Text
import Yesod hiding (Value)

data PageConfig = PageConfig
                { pageSize :: Int64
                , currentPage :: Int64
                } deriving Show

instance Default PageConfig where
    def = PageConfig { pageSize = 10, currentPage = 1 }

data Page r = Page
            { pageResults :: [Entity r]
            , pageCount :: Int64
            , nextPage :: Maybe Text
            , previousPage :: Maybe Text
            } deriving (Eq, Read, Show)

paginate :: (PersistEntity r, From SqlQuery SqlExpr SqlBackend t,
             RenderRoute site, YesodPersist site,
             YesodPersistBackend site ~ SqlPersistT)
         => (t -> SqlQuery (SqlExpr (Entity r)))
         -> HandlerT site IO (Page r)
paginate sel = do
    params <- liftM2 (\a b -> fst a ++ reqGetParams b)
        runRequestBody getRequest

    let currentPage = maybe 1 (fromMaybe 1 . decimalM)
                    $ lookup "page" params
        pageSize = within (5, 50)
           . maybe 10 (fromMaybe 10 . decimalM)
           $ lookup "count" params

    paginateWith def { pageSize, currentPage } sel

paginateWith :: (PersistEntity r, From SqlQuery SqlExpr SqlBackend t,
                 RenderRoute site, YesodPersist site,
                 YesodPersistBackend site ~ SqlPersistT)
             => PageConfig -> (t -> SqlQuery (SqlExpr (Entity r)))
             -> HandlerT site IO (Page r)
paginateWith c sel = do
    let filterStmt u = limit (pageSize c) >> return u

    [ct] <- runDB $ select $ from $ \u -> do
        _ <- filterStmt u -- does nothing, used for type constraint
        return (countRows :: SqlExpr (Value Int64))

    let maxPage = (unValue ct + pageSize c - 1) `div` pageSize c
        cp = within (1, maxPage) $ currentPage c

    es <- runDB $ select $ from $ \u -> do
        _ <- filterStmt u
        offset $ within (0, unValue ct - 1) $ pageSize c * (cp - 1)
        sel u

    rt' <- getCurrentRoute
    rend <- getUrlRenderParams

    let rt = fromMaybe (error "Attempting to use paginate on a server error page.") rt'
        qs = snd $ renderRoute rt
        np = rend rt $ updateQs qs ("page", [st|#{cp + 1}|])
        pp = rend rt $ updateQs qs ("page", [st|#{cp - 1}|])

    return Page { pageResults = es
                , pageCount = maxPage
                , nextPage = traceShow (cp, maxPage) $ if cp >= maxPage then Nothing else Just np
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
within (a,b) q | q <= a = a
               | q >= b = b
               | otherwise = q
