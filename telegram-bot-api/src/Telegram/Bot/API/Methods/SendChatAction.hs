{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendChatAction where

import Data.Aeson (ToJSON (..))
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

data SendChatActionRequest = SendChatActionRequest
  { sendChatActionBusinessConnectionId :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the action will be sent.
  , sendChatActionChatId :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , sendChatActionMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread; for supergroups only.
  , sendChatActionAction :: Text -- ^ Type of action to broadcast. Choose one, depending on what the user is about to receive: typing for text messages, upload_photo for photos, record_video or upload_video for videos, record_voice or upload_voice for voice notes, upload_document for general files, choose_sticker for stickers, find_location for location data, record_video_note or upload_video_note for video notes.
  }
  deriving Generic

instance ToJSON SendChatActionRequest where toJSON = gtoJSON

type SendChatAction = "sendChatAction"
  :> ReqBody '[JSON] SendChatActionRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method when you need to tell the
--   user that something is happening on the bot's side.
--   The status is set for 5 seconds or less
--   (when a message arrives from your bot, Telegram
--   clients clear its typing status).
--   Returns True on success.
--
--   Example: The ImageBot needs some time to
--   process a request and upload the image.
--   Instead of sending a text message along
--   the lines of “Retrieving image, please wait…”,
--   the bot may use sendChatAction with action = upload_photo.
--   The user will see a “sending photo” status for the bot.
--
--   We only recommend using this method when a
--   response from the bot will take a noticeable
--   amount of time to arrive.
sendChatAction :: SendChatActionRequest -> ClientM (Response  Bool)
sendChatAction = client (Proxy @SendChatAction)
