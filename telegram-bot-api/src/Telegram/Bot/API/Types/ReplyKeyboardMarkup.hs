{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ReplyKeyboardMarkup where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.KeyboardButton
import Telegram.Bot.API.Internal.Utils

-- ** 'ReplyKeyboardMarkup'

-- | This object represents a custom keyboard with reply options (see Introduction to bots for details and examples).
data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { replyKeyboardMarkupKeyboard           :: [[KeyboardButton]] -- ^ Array of button rows, each represented by an Array of KeyboardButton objects.
  , replyKeyboardMarkupIsPersistent      :: Maybe Bool         -- ^ Requests clients to always show the keyboard when the regular keyboard is hidden. Defaults to 'False', in which case the custom keyboard can be hidden and opened with a keyboard icon.
  , replyKeyboardMarkupResizeKeyboard     :: Maybe Bool         -- ^ Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to false, in which case the custom keyboard is always of the same height as the app's standard keyboard.
  , replyKeyboardMarkupOneTimeKeyboard    :: Maybe Bool         -- ^ Requests clients to hide the keyboard as soon as it's been used. The keyboard will still be available, but clients will automatically display the usual letter-keyboard in the chat – the user can press a special button in the input field to see the custom keyboard again. Defaults to false.
  , replyKeyboardMarkupInputFieldSelector :: Maybe Text         -- ^ The placeholder to be shown in the input field when the keyboard is active; 1-64 characters.
  , replyKeyboardMarkupSelective          :: Maybe Bool         -- ^ Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
    --
    -- Example: A user requests to change the bot's language, bot replies to the request with a keyboard to select the new language. Other users in the group don't see the keyboard.
  }
  deriving (Generic, Show)

instance ToJSON   ReplyKeyboardMarkup where toJSON = gtoJSON
instance FromJSON ReplyKeyboardMarkup where parseJSON = gparseJSON
