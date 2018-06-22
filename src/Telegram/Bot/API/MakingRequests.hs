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
import           Network.HTTP.Client.TLS         (tlsManagerSettings, mkManagerSettings)
import           Servant.Client                  hiding (Response)
import           Web.HttpApiData                 (FromHttpApiData,
                                                  ToHttpApiData (..))
import           Network.Connection
import           Network.Socket                  (HostName, PortNumber)

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.Types

botBaseUrl :: Token -> BaseUrl
botBaseUrl token = BaseUrl Https "api.telegram.org" 443
  (Text.unpack ("/bot" <> toUrlPiece token))

defaultTelegramClientEnv :: Token -> IO ClientEnv
defaultTelegramClientEnv token = ClientEnv
  <$> newManager tlsManagerSettings
  <*> pure (botBaseUrl token)
  <*> pure Nothing

defaultTelegramClientEnvWithProxy :: (HostName, PortNumber) -> Token -> IO ClientEnv
defaultTelegramClientEnvWithProxy (sockHost,sockPort) token = ClientEnv
  <$> newManager (mkManagerSettings (TLSSettingsSimple False False False) (Just (SockSettingsSimple sockHost sockPort)))
  <*> pure (botBaseUrl token)
  <*> pure Nothing


defaultRunBot :: Token -> ClientM a -> IO (Either ServantError a)
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
