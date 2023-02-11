{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.User where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** User

-- | This object represents a Telegram user or bot.
--
-- <https://core.telegram.org/bots/api#user>
data User = User
  { userId           :: UserId     -- ^ Unique identifier for this user or bot.
  , userIsBot        :: Bool       -- ^ 'True', if this user is a bot.
  , userFirstName    :: Text       -- ^ User's or bot's first name.
  , userLastName     :: Maybe Text -- ^ User‘s or bot’s last name.
  , userUsername     :: Maybe Text -- ^ User‘s or bot’s username.
  , userLanguageCode :: Maybe Text -- ^ IETF language tag of the user's language.
  , userIsPremium    :: Maybe Bool -- ^ 'True', if this user is a Telegram Premium user.
  , userAddedToAttachmentMenu :: Maybe Bool -- ^ 'True', if this user added the bot to the attachment menu.
  , userCanJoinGroups :: Maybe Bool -- ^ 'True', if the bot can be invited to groups. Returned only in `getMe`.
  , userCanReadAllGroupMessages :: Maybe Bool -- ^ 'True', if privacy mode is disabled for the bot. Returned only in `getMe`.
  , userSupportsInlineQueries :: Maybe Bool -- ^ 'True', if the bot supports inline queries. Returned only in `getMe`.
  }
  deriving (Show, Generic)

deriveJSON' ''User
