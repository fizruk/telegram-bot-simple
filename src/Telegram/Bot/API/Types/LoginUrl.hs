{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.LoginUrl where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'LoginUrl'

-- | This object represents a parameter of the inline keyboard button used to automatically authorize a user. Serves as a great replacement for the Telegram Login Widget when the user is coming from Telegram. All the user needs to do is tap/click a button and confirm that they want to log in:
--
-- https://core.telegram.org/file/811140015/1734/8VZFkwWXalM.97872/6127fa62d8a0bf2b3c
--
-- Telegram apps support these buttons as of version 5.7.
data LoginUrl = LoginUrl
  { loginUrlUrl                :: Text       -- ^ An HTTP URL to be opened with user authorization data added to the query string when the button is pressed. If the user refuses to provide authorization data, the original URL without information about the user will be opened. The data added is the same as described in Receiving authorization data.
  --
  -- **NOTE**: You **must** always check the hash of the received data to verify the authentication and the integrity of the data as described in Checking authorization.
  , loginUrlForwardText        :: Maybe Text -- ^ New text of the button in forwarded messages.
  , loginUrlBotUsername        :: Maybe Text -- ^ Username of a bot, which will be used for user authorization. See Setting up a bot for more details. If not specified, the current bot's username will be assumed. The url's domain must be the same as the domain linked with the bot. See Linking your domain to the bot for more details.
  , loginUrlRequestWriteAccess :: Maybe Bool -- ^ Pass 'True' to request the permission for your bot to send messages to the user.
  }
  deriving (Generic, Show)

deriveJSON' ''LoginUrl
