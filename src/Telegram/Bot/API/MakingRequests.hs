{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Telegram.Bot.API.MakingRequests where

import           Data.Aeson                      (FromJSON (..), ToJSON (..))
import           Data.Monoid                     ((<>))
import           Data.String                     (IsString)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           GHC.Generics                    (Generic)
import           Network.HTTP.Client             (newManager)
import           Network.HTTP.Client.TLS         (tlsManagerSettings)
import           Servant.Client                  hiding (Response)
import           Web.HttpApiData                 (FromHttpApiData,
                                                  ToHttpApiData (..))

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.Types

botBaseUrl :: Token -> BaseUrl
botBaseUrl token = BaseUrl Https "api.telegram.org" 443
  (Text.unpack ("/bot" <> toUrlPiece token))

defaultTelegramClientEnv :: Token -> IO ClientEnv
defaultTelegramClientEnv token = mkClientEnv
  <$> newManager tlsManagerSettings
  <*> pure (botBaseUrl token)

defaultRunBot :: Token -> ClientM a -> IO (Either ClientError a)
defaultRunBot token bot = do
  env <- defaultTelegramClientEnv token
  runClientM bot env

data Response a = Response
  { responseOk          :: Bool
  , responseDescription :: Maybe Text
  , responseResult      :: a
  , responseErrorCode   :: Maybe Integer
  , responseParameters  :: Maybe ResponseParameters
  } deriving (Show, Generic)

instance ToJSON   a => ToJSON   (Response a) where toJSON = gtoJSON
instance FromJSON a => FromJSON (Response a) where parseJSON = gparseJSON

newtype Token = Token Text
  deriving (Eq, Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON, IsString)
