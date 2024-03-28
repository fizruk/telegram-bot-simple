{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Telegram.Bot.API.MakingRequests where

import           Data.Aeson                      (FromJSON (..), ToJSON (..))
#if defined(MIN_VERSION_GLASGOW_HASKELL)
#if MIN_VERSION_GLASGOW_HASKELL(8,6,2,0)
#else
import           Data.Monoid                     ((<>))
#endif
#endif
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

botBaseUrlTest :: Token -> BaseUrl
botBaseUrlTest token = BaseUrl Https "api.telegram.org" 443
  (Text.unpack ("/bot" <> toUrlPiece token <> "/test"))

defaultTelegramClientEnv :: Token -> IO ClientEnv
defaultTelegramClientEnv token = mkClientEnv
  <$> newManager tlsManagerSettings
  <*> pure (botBaseUrl token)

defaultTelegramClientEnvTest :: Token -> IO ClientEnv
defaultTelegramClientEnvTest token = mkClientEnv
  <$> newManager tlsManagerSettings
  <*> pure (botBaseUrlTest token)

defaultRunBot :: Token -> ClientM a -> IO (Either ClientError a)
defaultRunBot token bot = do
  env <- defaultTelegramClientEnv token
  runClientM bot env

defaultRunBotTest :: Token -> ClientM a -> IO (Either ClientError a)
defaultRunBotTest token bot = do
  env <- defaultTelegramClientEnvTest token
  runClientM bot env

data Response a = Response
  { responseOk          :: Bool
  , responseDescription :: Maybe Text
  , responseResult      :: a
  , responseErrorCode   :: Maybe Integer
  , responseParameters  :: Maybe ResponseParameters
  } deriving (Show, Generic, Functor)

instance ToJSON   a => ToJSON   (Response a) where toJSON = gtoJSON
instance FromJSON a => FromJSON (Response a) where parseJSON = gparseJSON

newtype Token = Token Text
  deriving (Eq, Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON, IsString)
