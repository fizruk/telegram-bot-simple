{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.KeyboardButtonRequestChat where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.ChatAdministratorRights
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'KeyboardButtonRequestChat'

-- | This object defines the criteria used to request a suitable chat. The identifier of the selected chat will be shared with the bot when the corresponding button is pressed.
data KeyboardButtonRequestChat = KeyboardButtonRequestChat
  { keyboardButtonRequestChatRequestId :: RequestId -- ^ Signed 32-bit identifier of the request, which will be received back in the 'ChatShared' object. Must be unique within the message
  , keyboardButtonRequestChatChatIsChannel :: Bool -- ^ Pass 'True' to request a channel chat, pass 'False' to request a group or a supergroup chat. 
  , keyboardButtonRequestChatChatIsForum :: Maybe Bool -- ^ Pass 'True' to request a forum supergroup, pass 'False' to request a non-forum chat. If not specified, no additional restrictions are applied.
  , keyboardButtonRequestChatChatHasUsername :: Maybe Bool -- ^ Pass 'True' to request a supergroup or a channel with a username, pass 'False' to request a chat without a username. If not specified, no additional restrictions are applied.
  , keyboardButtonRequestChatChatIsCreated :: Maybe Bool -- ^ Pass 'True' to request a chat owned by the user. Otherwise, no additional restrictions are applied.
  , keyboardButtonRequestChatUserAdministratorRights :: Maybe ChatAdministratorRights -- ^ A JSON-serialized object listing the required administrator rights of the user in the chat. The rights must be a superset of @bot_administrator_rights@. If not specified, no additional restrictions are applied.
  , keyboardButtonRequestChatBotAdministratorRights :: Maybe ChatAdministratorRights -- ^ A JSON-serialized object listing the required administrator rights of the bot in the chat. The rights must be a subset of @user_administrator_rights@. If not specified, no additional restrictions are applied.
  , keyboardButtonRequestChatBotIsMember :: Maybe Bool -- ^ Pass 'True' to request a chat with the bot as a member. Otherwise, no additional restrictions are applied.
  , keyboardButtonRequestChatRequestTitle :: Maybe Bool -- ^ Pass 'True' to request the chat's title.
  , keyboardButtonRequestChatRequestUsername :: Maybe Bool -- ^ Pass 'True' to request the chat's username.
  , keyboardButtonRequestChatRequestPhoto :: Maybe Bool -- ^ Pass 'True' to request the chat's photo.
  }
  deriving (Generic, Show)

instance ToJSON   KeyboardButtonRequestChat where toJSON = gtoJSON
instance FromJSON KeyboardButtonRequestChat where parseJSON = gparseJSON
