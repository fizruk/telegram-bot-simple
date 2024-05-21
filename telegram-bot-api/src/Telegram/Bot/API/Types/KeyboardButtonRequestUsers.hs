{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.KeyboardButtonRequestUsers where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'KeyboardButtonRequestUsers'

-- | This object defines the criteria used to request suitable users. 
-- The identifiers of the selected users will be shared with the bot when the corresponding button is pressed.
-- [More about requesting users »](https://core.telegram.org/bots/features#chat-and-user-selection)
data KeyboardButtonRequestUsers = KeyboardButtonRequestUsers
  { keyboardButtonRequestUsersRequestId :: RequestId -- ^ Signed 32-bit identifier of the request, which will be received back in the 'UserShared' object. Must be unique within the message
  , keyboardButtonRequestUsersUserIsBot :: Maybe Bool -- ^ Pass 'True' to request a bot, pass 'False' to request a regular user. If not specified, no additional restrictions are applied.
  , keyboardButtonRequestUsersUserIsPremium :: Maybe Bool -- ^ Pass 'True' to request a premium user, pass 'False' to request a non-premium user. If not specified, no additional restrictions are applied.
  , keyboardButtonRequestUsersMaxQuantity :: Maybe Int -- ^ The maximum number of users to be selected; 1-10. Defaults to 1.
  , keyboardButtonRequestUsersRequestName :: Maybe Bool -- ^ Pass 'True' to request the users' first and last names.
  , keyboardButtonRequestUsersRequestUsername :: Maybe Bool -- ^ Pass 'True' to request the users' usernames.
  , keyboardButtonRequestUsersRequestPhoto :: Maybe Bool -- ^ Pass 'True' to request the users' photos.
  }
  deriving (Generic, Show)

instance ToJSON   KeyboardButtonRequestUsers where toJSON = gtoJSON
instance FromJSON KeyboardButtonRequestUsers where parseJSON = gparseJSON
