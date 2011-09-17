module Github.Request where

import           Control.Monad           (liftM)
import           Control.Monad.Trans     (MonadIO(..))
import           Data.Aeson.Types        (parseEither)
import           Network.HTTP.Enumerator (simpleHttp)

import qualified Data.Aeson           as Aeson
import qualified Data.Attoparsec      as Parsec
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import           Github.Types (Repository)
import           Github.JSONParser (pRepos)

-------------------------------------------------------------------------------

type Username = String

-------------------------------------------------------------------------------

-- TODO: memoize this function in the future
getBodyContentsFromUrl :: MonadIO m => String -> m B.ByteString
getBodyContentsFromUrl url = 
    (B.concat . BL.toChunks) `liftM` (liftIO $ simpleHttp url)

getJSONFromUrl :: MonadIO m => String -> m Aeson.Value
getJSONFromUrl url = do
    content <- getBodyContentsFromUrl url
    case Parsec.parseOnly Aeson.json content of
      Right result -> return result
      Left e       -> error e

-------------------------------------------------------------------------------

getReposForUser :: MonadIO m => Username -> m (Either String [Repository])
getReposForUser user = 
    parseEither pRepos `liftM` getJSONFromUrl url
  where
    url = "http://github.com/api/v2/json/repos/show/" ++ user
    
    
