{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
module Github.Types where

import           Data.Aeson          (FromJSON, (.:))
import           Data.Text           (unpack)
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    (FormatTime, parseTime)
import           Data.Typeable       (Typeable)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (MonadPlus(..))
import           System.Locale       (defaultTimeLocale)

import qualified Data.ByteString as B
import qualified Data.Aeson      as Aeson

-- Types
-------------------------------------------------------------------------------

newtype GithubTime
  = GithubTime {
    fromGithubTime :: UTCTime
  }
  deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

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


-- Aeson instances
------------------------------------------------------------------------------

instance FromJSON GithubTime where
  parseJSON (Aeson.String text) =
      case parseTime defaultTimeLocale dateFormat content of
        Just time -> return $ GithubTime time
        _         -> fail "Could not parse github date"
    where
      dateFormat = "%Y/%m/%d %T %z"
      content = unpack text
  parseJSON _ = mzero

instance FromJSON Repository where
  parseJSON (Aeson.Object repo) =
      Repository <$> (repo .: "name")
                 <*> (repo .: "url")
                 <*> (repo .: "created_at")
                 <*> (repo .: "fork")
                 <*> (repo .: "forks")
                 <*> (repo .: "owner")
                 <*> (repo .: "pushed_at")
                 <*> (repo .: "watchers")
                 <*> (repo .: "size")
  parseJSON _ = mzero

