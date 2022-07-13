{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Telegram.Bot.API.WebHooks (setUpWebhook, webhookApp, deleteWebhook) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Functor (void)
import Debug.Trace
import GHC.Generics
import Network.HTTP.Client.MultipartFormData
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal
  ( Settings (settingsHost, settingsPort),
  )
import Servant
import Servant.Client
import Servant.Multipart.API
import Servant.Multipart.Client
import Telegram.Bot.API (InputFile, Update)
import Telegram.Bot.API.Internal.Utils (gparseJSON, gtoJSON)
import Telegram.Bot.Simple.BotApp.Internal

type WebhookAPI = ReqBody '[JSON] Update :> Get '[JSON] ()

server :: Server WebhookAPI
server =
  updateHandler
  where
    updateHandler :: Update -> Handler ()
    updateHandler update = trace (show update) return ()

userAPI :: Proxy WebhookAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

-- url	String	Yes	HTTPS URL to send updates to. Use an empty string to remove webhook integration
-- certificate	InputFile	Optional	Upload your public key certificate so that the root certificate in use can be checked. See our self-signed guide for details.
-- ip_address	String	Optional	The fixed IP address which will be used to send webhook requests instead of the IP address resolved through DNS
-- max_connections	Integer	Optional	The maximum allowed number of simultaneous HTTPS connections to the webhook for update delivery, 1-100. Defaults to 40. Use lower values to limit the load on your bot's server, and higher values to increase your bot's throughput.
-- allowed_updates	Array of String	Optional	A JSON-serialized list of the update types you want your bot to receive. For example, specify [“message”, “edited_channel_post”, “callback_query”] to only receive updates of these types. See Update for a complete list of available update types. Specify an empty list to receive all update types except chat_member (default). If not specified, the previous setting will be used.
-- Please note that this parameter doesn't affect updates created before the call to the setWebhook, so unwanted updates may be received for a short period of time.
-- drop_pending_updates	Boolean	Optional	Pass True to drop all pending updates
-- secret_token	String	Optional	A secret token to be sent in a header “X-Telegram-Bot-Api-Secret-Token” in every webhook request, 1-256 characters. Only characters A-Z, a-z, 0-9, _ and - are allowed. The header is useful to ensure that the request comes from a webhook set by you.

data SetWebhookRequest = SetWebhookRequest
  { setWebhookUrl :: String,
    setWebhookCertificate :: Maybe InputFile,
    setWebhookIpAddress :: Maybe String,
    setWebhookMaxConnections :: Maybe Int,
    setWebhookAllowedUpdates :: Maybe [String],
    setWebhookDropPendingUpdates :: Maybe Bool,
    setWebhookSecretToken :: Maybe String
  }
  deriving (Generic)

instance ToJSON SetWebhookRequest where toJSON = gtoJSON

newtype DeleteWebhookRequest = DeleteWebhookRequest
  { deleteWebhookDropPendingUpdates :: Maybe Bool
  }
  deriving (Generic)

instance ToJSON DeleteWebhookRequest where toJSON = gtoJSON

instance ToMultipart Tmp SetWebhookRequest where
  toMultipart SetWebhookRequest {..} = error ""

--   makeFile "sticker" sendStickerSticker (MultipartData fields []) where
--   fields =
--     [ Input "chat_id" $ case sendStickerChatId of
--         SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
--         SomeChatUsername txt -> txt
--     ] <> catMaybes
--     [ sendStickerDisableNotification <&>
--       \t -> Input "disable_notification" (bool "false" "true" t)
--     , sendStickerReplyToMessageId <&>
--       \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
--     , sendStickerAllowSendingWithoutReply <&>
--       \t -> Input "allow_sending_without_reply" (bool "false" "true" t)
--     , sendStickerReplyMarkup <&>
--       \t -> Input "reply_markup" (TL.toStrict $ encodeToLazyText t)
--     ]

type SetWebhookForm =
  "setWebhook" :> MultipartForm Tmp SetWebhookRequest :> Get '[JSON] Bool

type SetWebhookJson =
  "setWebhook" :> ReqBody '[JSON] SetWebhookRequest :> Get '[JSON] Bool

type DeleteWebhook =
  "deleteWebhook" :> ReqBody '[JSON] DeleteWebhookRequest :> Get '[JSON] Bool

setUpWebhook :: Settings -> Maybe InputFile  -> ClientEnv -> IO (Either ClientError ())
setUpWebhook warpOpts certFile = (void <$>) <$> runClientM setUpWebhookRequest
  where
    port = settingsPort warpOpts
    ip = show $ settingsHost warpOpts
    url = "https://" ++ ip ++ ":" ++ show port
    requestData =
      SetWebhookRequest
        { setWebhookUrl = url,
          setWebhookCertificate = certFile,
          setWebhookIpAddress = Nothing,
          setWebhookMaxConnections = Nothing,
          setWebhookAllowedUpdates = Nothing,
          setWebhookDropPendingUpdates = Nothing,
          setWebhookSecretToken = Nothing
        }
    setUpWebhookRequest :: ClientM Bool
    setUpWebhookRequest = case certFile of
      Just _ -> do
        boundary <- liftIO genBoundary
        client (Proxy @SetWebhookForm) (boundary, requestData)
      Nothing -> client (Proxy @SetWebhookJson) requestData

deleteWebhook :: ClientEnv -> IO (Either ClientError ())
deleteWebhook = (void <$>) <$> runClientM deleteWebhookRequest
  where
    requestData = DeleteWebhookRequest {deleteWebhookDropPendingUpdates = Nothing}
    deleteWebhookRequest = client (Proxy @DeleteWebhook) requestData

webhookApp :: BotEnv model action -> Application
webhookApp _ = app