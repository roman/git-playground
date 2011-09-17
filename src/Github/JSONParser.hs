{-# LANGUAGE OverloadedStrings #-}
module Github.JSONParser where


import           Control.Monad (MonadPlus(..))
import           Data.Aeson (FromJSON(..), (.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson as Aeson

import           Github.Types


pRepos :: Aeson.Value -> Aeson.Parser [Repository]
pRepos (Aeson.Object obj) = 
    obj .: "repositories" >>= parseJSON 
pRepos _ = mzero
