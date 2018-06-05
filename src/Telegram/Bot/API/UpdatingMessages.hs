{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Telegram.Bot.API.UpdatingMessages where

import           Data.Aeson
import           Data.Proxy
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Servant.API
import           Servant.Client                  (ClientM, client)

import           Telegram.Bot.API.Internal.Utils (gparseJSON, gtoJSON)
import           Telegram.Bot.API.MakingRequests
import           Telegram.Bot.API.Methods
import           Telegram.Bot.API.Types

-- ** 'editMessageText'

type EditMessageText
  = "editMessageText"
  :> ReqBody '[JSON] EditMessageTextRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send text messages.
-- On success, the sent 'Message' is returned.
editMessageText :: EditMessageTextRequest -> ClientM (Response Message)
editMessageText = client (Proxy @EditMessageText)

-- | Request parameters for 'sendMessage'.
data EditMessageTextRequest = EditMessageTextRequest
  { editMessageTextChatId                :: Maybe SomeChatId -- ^ Required if 'editMessageTextInlineMessageId' is not specified. Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , editMessageTextMessageId             :: Maybe MessageId -- ^ Required if 'editMessageTextInlineMessageId' is not specified. Identifier of the sent message.
  , editMessageTextInlineMessageId       :: Maybe MessageId -- ^ Required if 'editMessageTextChatId' and 'editMessageTextMessageId' are not specified. Identifier of the sent message.
  , editMessageTextText                  :: Text -- ^ Text of the message to be sent.
  , editMessageTextParseMode             :: Maybe ParseMode -- ^ Send 'Markdown' or 'HTML', if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , editMessageTextDisableWebPagePreview :: Maybe Bool -- ^ Disables link previews for links in this message.
  , editMessageTextReplyMarkup           :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
} deriving (Generic)

instance ToJSON   EditMessageTextRequest where toJSON = gtoJSON
instance FromJSON EditMessageTextRequest where parseJSON = gparseJSON
