{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>), (<*>))

import Data.Attoparsec (parseOnly)
import Data.Aeson (
    FromJSON(..)
  , fromJSON
  , Value(..)
  , Result(..)
  , Object
  , (.:)
  )
import Data.Aeson.Types (Parser, parse)
import Data.Aeson.Parser (json)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Enumerator (run_)
import Network.HTTP.Enumerator (simpleHttp)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (FormatTime, parseTime)
import Data.Typeable (Typeable)
import Data.Text (unpack)
import System.Locale (defaultTimeLocale)


newtype GithubTime
  = GithubTime {
    fromGithubTime :: UTCTime
  }
  deriving (Eq, Ord, Read, Show, Typeable, FormatTime)


-- "2011/08/10 11:30:58 -0700"

instance FromJSON GithubTime where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%Y/%m/%d %T %z" (unpack t) of
      Just d -> return (GithubTime d)
      _ -> fail "Could not parse github date"

data Repository
  = Repository {
    repoName      :: B.ByteString
  , repoUrl       :: B.ByteString
  , repoCreatedAt :: GithubTime
  , repoIsFork    :: Bool
  , repoForks     :: Int
  , repoOwner     :: B.ByteString
  , repoPushedAt  :: GithubTime
  , repoWatchers  :: Int
  , repoSize      :: Int
  }
  deriving (Show, Eq)

instance FromJSON Repository where
  parseJSON (Object repo) =
      Repository <$> (repo .: "name")
                 <*> (repo .: "url")
                 <*> (repo .: "created_at")
                 <*> (repo .: "fork")
                 <*> (repo .: "forks")
                 <*> (repo .: "owner")
                 <*> (repo .: "pushed_at")
                 <*> (repo .: "watchers")
                 <*> (repo .: "size")

requestRepos :: String -> IO B.ByteString
requestRepos user =
    fmap (B.concat . BL.toChunks)
         (simpleHttp $ url ++ user)
  where
    url = "http://github.com/api/v2/json/repos/show/"

pJSONRepos :: Value -> Parser [Repository]
pJSONRepos (Object o) = (o .: "repositories") >>= parseJSON

main :: IO ()
main = do
    eit <- parseOnly json `fmap` requestRepos "roman"
    case eit of
      Right result -> do
        print result
        print $ parse pJSONRepos result
      Left e -> error e

