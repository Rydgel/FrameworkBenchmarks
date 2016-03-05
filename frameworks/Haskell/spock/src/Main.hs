{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import           Data.Aeson                    hiding (json)
import           Data.List                     (sort)
import           Data.Maybe                    (catMaybes, fromMaybe)
import           GHC.Exts
import qualified Hasql.Connection              as HC
import qualified Hasql.Pool                    as HP
import           Network.HTTP.Types.Status
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe

import           Models.Fortune
import           Models.World
import           Views.Fortune


settings :: HC.Settings
settings =
    HC.settings host port user password database
        where
          host = "172.16.0.16"
          port = 5432
          user = "benchmarkdbuser"
          password = "benchmarkdbpass"
          database = "hello_world"


blaze :: MonadIO m => H.Html -> ActionCtxT ctx m a
blaze h = do
    setHeader "Content-Type" "text/html; charset=UTF-8"
    lazyBytes $ renderHtml h
{-# INLINE blaze #-}


getQueriesNumber :: MonadIO m => ActionCtxT ctx m Int
getQueriesNumber = do
    queriesM <- param "queries"
    return $ max 1 . min 500 $ fromMaybe 1 queriesM
{-# INLINE getQueriesNumber #-}


-- | Test 1: JSON serialization
test1 :: MonadIO m => ActionCtxT ctx m a
test1 = do
    setHeader "Content-Type" "application/json"
    lazyBytes $ encode $ Object (fromList [("message", "Hello, World!")])
{-# INLINE test1 #-}

-- | Test 2: Single database query
test2 :: MonadIO m => HP.Pool -> ActionCtxT ctx m a
test2 pool = do
    maybeWorld <- liftIO $ getRandomWorld pool
    setHeader "Content-Type" "application/json"
    case maybeWorld of
      Just w  -> lazyBytes $ encode w
      Nothing -> setStatus status404 >> lazyBytes "{\"error\": \"World not found.\"}"
{-# INLINE test2 #-}

-- | Test 3: Multiple database queries
test3 :: MonadIO m => HP.Pool -> ActionCtxT ctx m a
test3 pool = do
    queries <- getQueriesNumber
    worlds <- liftIO $ mapConcurrently (const (getRandomWorld pool)) [1..queries]
    setHeader "Content-Type" "application/json"
    lazyBytes $ encode worlds
{-# INLINE test3 #-}

-- | Test 4: Fortunes
test4 :: MonadIO m => HP.Pool -> ActionCtxT ctx m a
test4 pool = do
    fortunes <- liftIO $ fetchFortunes pool
    blaze $ renderFortunes $ sort (newFortune : fortunes)
    where
      newFortune = Fortune 0 "Additional fortune added at request time."
{-# INLINE test4 #-}

-- | Test 5: Database Updates
test5 :: MonadIO m => HP.Pool -> ActionCtxT ctx m a
test5 pool = do
    queries <- getQueriesNumber
    worlds <- liftIO $ mapConcurrently (const (getRandomWorld pool)) [1..queries]
    updatedWorlds <- liftIO $ mapConcurrently (updateWorldRandom pool) (catMaybes worlds)
    setHeader "Content-Type" "application/json"
    lazyBytes $ encode updatedWorlds
{-# INLINE test5 #-}

-- | Test 6: Plain text
test6 :: MonadIO m => ActionCtxT ctx m a
test6 = do
    setHeader "Content-Type" "text/plain"
    lazyBytes "Hello, World!"
{-# INLINE test6 #-}


main :: IO ()
main = do
    hPool <- HP.acquire (50, 50, settings)
    runSpock 3000 $ spockT id $ do
        get "json"        test1
        get "db"        $ test2 hPool
        get "queries"   $ test3 hPool
        get "fortune"   $ test4 hPool
        get "updates"   $ test5 hPool
        get "plaintext"   test6
    HP.release hPool
