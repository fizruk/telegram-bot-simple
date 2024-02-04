{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.KeyboardButton where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.KeyboardButtonRequestChat
import Telegram.Bot.API.Types.KeyboardButtonRequestUsers
import Telegram.Bot.API.Types.PollType
import Telegram.Bot.API.Internal.Utils

-- ** 'KeyboardButton'

-- | This object represents one button of the reply keyboard.
-- For simple text buttons String can be used instead of this object
-- to specify text of the button. Optional fields are mutually exclusive.
data KeyboardButton = KeyboardButton
  { keyboardButtonText            :: Text       -- ^ Text of the button. If none of the optional fields are used, it will be sent as a message when the button is pressed.
  , keyboardButtonRequestUsers    :: Maybe KeyboardButtonRequestUsers -- ^ If specified, pressing the button will open a list of suitable users. Identifiers of selected users will be sent to the bot in a “users_shared” service message. Available in private chats only.
  , keyboardButtonRequestChat     :: Maybe KeyboardButtonRequestChat -- ^ If specified, pressing the button will open a list of suitable chats. Tapping on a chat will send its identifier to the bot in a “chat_shared” service message. Available in private chats only.
  , keyboardButtonRequestContact  :: Maybe Bool -- ^ If 'True', the user's phone number will be sent as a contact when the button is pressed. Available in private chats only.
  , keyboardButtonRequestLocation :: Maybe Bool -- ^ If 'True', the user's current location will be sent when the button is pressed. Available in private chats only.
  , keyboardButtonRequestPoll     :: Maybe PollType -- ^ If specified, the user will be asked to create a poll and send it to the bot when the button is pressed. Available in private chats only.
  , keyboardButtonWebApp          :: Maybe WebAppInfo -- ^ If specified, the described Web App will be launched when the button is pressed. The Web App will be able to send a “web_app_data” service message. Available in private chats only.
  }
  deriving (Generic, Show)

instance IsString KeyboardButton where
  fromString s = KeyboardButton (fromString s) Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON   KeyboardButton where toJSON = gtoJSON
instance FromJSON KeyboardButton where parseJSON = gparseJSON
