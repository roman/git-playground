{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Failure (Failure(..))
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.IO.Control (MonadControlIO)

import Database.Redis.Redis (Reply(..), connect, localhost, defaultPort)
import Database.Redis.Monad (WithRedis)
import qualified Database.Redis.Monad as Redis hiding (connect)
import Database.Redis.Monad.State (runWithRedis)

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Enumerator (
  Iteratee(..)
  , Enumerator
  , Step(..)
  , Stream(..)
  , run_
  , ($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Network.HTTP.Enumerator (HttpException, Response)
import qualified Network.HTTP.Enumerator as EH

import Control.Monad.State (StateT)


instance Failure e m => Failure e (StateT a m) where
  failure e = lift $ failure e


getUrlContent :: (WithRedis m, MonadIO m) 
              => String 
              -> m (Maybe ByteString)
getUrlContent url = do
    content <- Redis.hget ("github" :: String) url
    case content of
      RBulk Nothing -> return Nothing
      RBulk (Just content) -> return (Just content)

storeUrlContent :: (WithRedis m, MonadIO m)
                => String
                -> Response
                -> m ()
storeUrlContent url response = 
    Redis.hset ("github" :: String) url (BS.unpack $ EH.responseBody response) >>
    return ()

getNextURL response = (EH.responseHeaders response)

enumGithubAPI :: ( WithRedis m
                 , MonadIO m
                 , Failure HttpException m
                 , MonadControlIO m) 
              => String 
              -> Enumerator ByteString m [ByteString]
enumGithubAPI url result@(Yield {}) = E.returnI result
enumGithubAPI url (Continue fn) = Iteratee $ do
    content <- getUrlContent url
    case content of
      Just body -> runIteratee $ fn (Chunks [body])
      Nothing -> do
        response <- EH.withManager $ \manager -> do
                      request  <- EH.parseUrl url
                      response <- EH.httpLbs request manager
                      return response
        liftIO $ print (getNextURL response)
        storeUrlContent url response
        runIteratee $ fn (Chunks [EH.responseBody response])


main :: IO ()
main = do
    conn <- connect localhost defaultPort 
    runWithRedis conn $ do
      content <- run_ ( 
                enumGithubAPI "http://github.com/api/v2/json/repos/show/roman" $$
                EL.consume)
      -- liftIO $ print content
    return ()

--main = do
--  conn <- connect localhost defaultPort 
--  runWithRedis conn $ do
--    Redis.ping >>= liftIO . print
--    result <- Redis.hget "github" "nothing" 
--    --case result of 
--    --  RBulk content -> liftIO $ putStrLn content
--    liftIO $ print (result :: Redis.Reply String)
--  return ()
