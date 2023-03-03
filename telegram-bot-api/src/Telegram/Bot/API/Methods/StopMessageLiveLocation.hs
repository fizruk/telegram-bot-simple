{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.StopMessageLiveLocation where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'stopMessageLiveLocation'

-- | Request parameters for 'stopMessageLiveLocation'.
data StopMessageLiveLocationRequest = StopMessageLiveLocationRequest
  { stopMessageLiveLocationChatId :: Maybe SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , stopMessageLiveLocationMessageId :: Maybe MessageId -- ^ Required if inline_message_id is not specified. Identifier of the message with live location to stop
  , stopMessageLiveLocationInlineMessageId :: Maybe Text -- ^  	Required if chat_id and message_id are not specified. Identifier of the inline message
  , stopMessageLiveLocationReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON   StopMessageLiveLocationRequest where toJSON = gtoJSON
instance FromJSON StopMessageLiveLocationRequest where parseJSON = gparseJSON

type StopMessageLiveLocation = "stopMessageLiveLocation"
  :> ReqBody '[JSON] StopMessageLiveLocationRequest
  :> Post '[JSON] (Response (Either Bool Message))

-- | Use this method to stop updating a live
--   location message before live_period
--   expires. On success, if the message is
--   not an inline message, the edited Message
--   is returned, otherwise True is returned.
stopMessageLiveLocation :: StopMessageLiveLocationRequest ->  ClientM (Response (Either Bool Message))
stopMessageLiveLocation = client (Proxy @StopMessageLiveLocation)

makeDefault ''StopMessageLiveLocationRequest
