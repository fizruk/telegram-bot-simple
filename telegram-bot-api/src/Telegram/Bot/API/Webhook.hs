{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Telegram.Bot.API.Webhook
  ( setUpWebhook,
    deleteWebhook,
    SetWebhookRequest (..),
    defSetWebhook,
    defDeleteWebhook
  )
where

import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Aeson                          (ToJSON (toJSON))
import           Data.Bool                           (bool)
import           Data.Functor                        (void, (<&>))
import           Data.Maybe                          (catMaybes, fromJust,
                                                      isJust)
import qualified Data.Text                           as Text
import           GHC.Generics                        (Generic)
import           Servant
import           Servant.Client                      (ClientEnv, ClientError,
                                                      client, runClientM)
import           Servant.Multipart.API
import           Servant.Multipart.Client            (genBoundary)

import           Telegram.Bot.API.Internal.Utils     (gtoJSON, showText)
import           Telegram.Bot.API.MakingRequests     (Response)
import           Telegram.Bot.API.Types              (InputFile, makeFile)
import Telegram.Bot.API.Internal.TH (makeDefault)


data SetWebhookRequest = SetWebhookRequest
  { setWebhookUrl                :: String,
    setWebhookCertificate        :: Maybe InputFile,
    setWebhookIpAddress          :: Maybe String,
    setWebhookMaxConnections     :: Maybe Int,
    setWebhookAllowedUpdates     :: Maybe [String],
    setWebhookDropPendingUpdates :: Maybe Bool,
    setWebhookSecretToken        :: Maybe String
  }
  deriving (Generic)

instance ToJSON SetWebhookRequest where toJSON = gtoJSON

newtype DeleteWebhookRequest = DeleteWebhookRequest
  { deleteWebhookDropPendingUpdates :: Maybe Bool
  }
  deriving (Generic)

instance ToJSON DeleteWebhookRequest where toJSON = gtoJSON

instance ToMultipart Tmp SetWebhookRequest where
  toMultipart SetWebhookRequest {..} =
    makeFile "certificate" (fromJust setWebhookCertificate) (MultipartData fields [])
    where
      fields =
        [Input "url" $ Text.pack setWebhookUrl]
          <> catMaybes
            [ setWebhookSecretToken <&> \t -> Input "secret_token" $ Text.pack t,
              setWebhookIpAddress <&> \t -> Input "ip_address" $ Text.pack t,
              setWebhookMaxConnections <&> \t -> Input "max_connections" $ showText t,
              setWebhookDropPendingUpdates <&> \t -> Input "drop_pending_updates" (bool "false" "true" t),
              setWebhookAllowedUpdates <&> \t -> Input "allowed_updates" (arrToJson t)
            ]
      arrToJson arr = Text.intercalate "" ["[", Text.intercalate "," (map (\s -> Text.pack $ "\"" ++ s ++ "\"") arr), "]"]

type SetWebhookForm =
  "setWebhook" :> MultipartForm Tmp SetWebhookRequest :> Get '[JSON] (Response Bool)

type SetWebhookJson =
  "setWebhook" :> ReqBody '[JSON] SetWebhookRequest :> Get '[JSON] (Response Bool)

type DeleteWebhook =
  "deleteWebhook" :> ReqBody '[JSON] DeleteWebhookRequest :> Get '[JSON] (Response Bool)

setUpWebhook :: SetWebhookRequest -> ClientEnv -> IO (Either ClientError ())
setUpWebhook requestData = (void <$>) <$> runClientM setUpWebhookRequest
  where
    setUpWebhookRequest =
      if isJust $ setWebhookCertificate requestData
        then do
          boundary <- liftIO genBoundary
          client (Proxy @SetWebhookForm) (boundary, requestData)
        else client (Proxy @SetWebhookJson) requestData

deleteWebhook :: ClientEnv -> IO (Either ClientError ())
deleteWebhook = (void <$>) <$> runClientM deleteWebhookRequest
  where
    requestData = DeleteWebhookRequest {deleteWebhookDropPendingUpdates = Nothing}
    deleteWebhookRequest = client (Proxy @DeleteWebhook) requestData


foldMap makeDefault
  [ ''SetWebhookRequest
  , ''DeleteWebhookRequest
  ]
