{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module Telegram.Bot.API.GettingUpdates where

import           Data.Aeson                      (FromJSON (..), ToJSON (..), Value)
import           Data.Foldable                   (asum)
import           Data.Proxy
import           GHC.Generics                    (Generic)

import           Servant.API
import           Servant.Client                  hiding (Response)

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.MakingRequests
import           Telegram.Bot.API.Types
import           Telegram.Bot.API.InlineMode
import Telegram.Bot.API.Internal.TH (makeDefault)

-- ** 'Update'

newtype UpdateId = UpdateId Int
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | This object represents an incoming update.
-- At most __one__ of the optional parameters can be present in any given update.
data Update = Update
  { updateUpdateId          :: UpdateId -- ^ The update‘s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using Webhooks, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.
  , updateMessage           :: Maybe Message -- ^ New incoming message of any kind — text, photo, sticker, etc.
  , updateEditedMessage     :: Maybe Message -- ^ New version of a message that is known to the bot and was edited
  , updateChannelPost       :: Maybe Message -- ^ New incoming channel post of any kind — text, photo, sticker, etc.
  , updateMessageReaction   :: Maybe MessageReactionUpdated -- ^ A reaction to a message was changed by a user. The bot must be an administrator in the chat and must explicitly specify "message_reaction" in the list of allowed_updates to receive these updates. The update isn't received for reactions set by bots.
  , updateMessageReactionCount :: Maybe MessageReactionCountUpdated -- ^ Reactions to a message with anonymous reactions were changed. The bot must be an administrator in the chat and must explicitly specify "message_reaction_count" in the list of allowed_updates to receive these updates. The updates are grouped and can be sent with delay up to a few minutes.
  , updateEditedChannelPost :: Maybe Message -- ^ New version of a channel post that is known to the bot and was edited

  , updateInlineQuery       :: Maybe InlineQuery -- ^ New incoming inline query

  , updateChosenInlineResult :: Maybe ChosenInlineResult -- ^ The result of an inline query that was chosen by a user and sent to their chat partner. Please see our documentation on the feedback collecting for details on how to enable these updates for your bot.

  , updateCallbackQuery     :: Maybe CallbackQuery -- ^ New incoming callback query

  , updateShippingQuery     :: Maybe ShippingQuery -- ^ New incoming shipping query. Only for invoices with flexible price
  , updatePreCheckoutQuery  :: Maybe PreCheckoutQuery -- ^ New incoming pre-checkout query. Contains full information about checkout
  , updatePoll              :: Maybe Poll -- ^ New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot.
  , updatePollAnswer        :: Maybe PollAnswer -- ^ A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
  , updateMyChatMember      :: Maybe ChatMemberUpdated -- ^ The bot's chat member status was updated in a chat. For private chats, this update is received only when the bot is blocked or unblocked by the user.
  , updateChatMember        :: Maybe ChatMemberUpdated -- ^ A chat member's status was updated in a chat. The bot must be an administrator in the chat and must explicitly specify “chat_member” in the list of allowed_updates to receive these updates.
  , updateChatJoinRequest   :: Maybe ChatJoinRequest -- ^ A request to join the chat has been sent. The bot must have the can_invite_users administrator right in the chat to receive these updates.
  , updateChatBoost         :: Maybe ChatBoostUpdated -- ^ A chat boost was added or changed. The bot must be an administrator in the chat to receive these updates.
  , updateRemovedChatBoost  :: Maybe ChatBoostRemoved -- ^ A boost was removed from a chat. The bot must be an administrator in the chat to receive these updates.
  } deriving (Generic, Show)

instance ToJSON   Update where toJSON = gtoJSON
instance FromJSON Update where parseJSON = gparseJSON

updateChatId :: Update -> Maybe ChatId
updateChatId = fmap (chatId . messageChat) . extractUpdateMessage

extractUpdateMessage :: Update -> Maybe Message
extractUpdateMessage Update{..} = asum
  [ updateMessage
  , updateEditedMessage
  , updateChannelPost
  , updateEditedChannelPost
  , updateCallbackQuery >>= callbackQueryMessage
  ]

-- ** 'getUpdates'

type GetUpdatesAs a
  = "getUpdates" :> ReqBody '[JSON] GetUpdatesRequest :> Get '[JSON] (Response [a])

type GetUpdates = GetUpdatesAs Update

-- | Use this method to receive incoming updates using long polling.
-- An list of 'Update' objects is returned.
--
-- NOTE: This method will not work if an outgoing webhook is set up.
--
-- NOTE: In order to avoid getting duplicate updates, recalculate offset after each server response.
getUpdates :: GetUpdatesRequest -> ClientM (Response [Update])
getUpdates = client (Proxy @GetUpdates)

-- | More liberal version of `getUpdates` funcion.
--
-- It's useful  when you aren't sure that you can
-- parse all the types of updates, so you would recieve
-- a list of Value with updates, that you can parse later.
getUpdatesAsValue :: GetUpdatesRequest -> ClientM (Response [Value])
getUpdatesAsValue = client (Proxy @(GetUpdatesAs Value))

-- | Request parameters for 'getUpdates'.
data GetUpdatesRequest = GetUpdatesRequest
  { getUpdatesOffset         :: Maybe UpdateId -- ^ Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as getUpdates is called with an offset higher than its update_id. The negative offset can be specified to retrieve updates starting from -offset update from the end of the updates queue. All previous updates will forgotten.
  , getUpdatesLimit          :: Maybe Int -- ^ Limits the number of updates to be retrieved. Values between 1—100 are accepted. Defaults to 100.
  , getUpdatesTimeout        :: Maybe Seconds -- ^ Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling. Should be positive, short polling should be used for testing purposes only.
  , getUpdatesAllowedUpdates :: Maybe [UpdateType] -- ^ List the types of updates you want your bot to receive. For example, specify [“message”, “edited_channel_post”, “callback_query”] to only receive updates of these types. See GetUpdates for a complete list of available update types. Specify an empty list to receive all updates regardless of type (default). If not specified, the previous setting will be used. Please note that this parameter doesn't affect updates created before the call to the getUpdates, so unwanted updates may be received for a short period of time.
  } deriving (Generic)

instance ToJSON   GetUpdatesRequest where toJSON = gtoJSON
instance FromJSON GetUpdatesRequest where parseJSON = gparseJSON

data UpdateType
  = UpdateMessage
  | UpdateEditedMessage
  | UpdateChannelPost
  | UpdateEditedChannelPost
  | UpdateInlineQuery
  | UpdateChosenInlineResult
  | UpdateCallbackQuery
  | UpdateShippingQuery
  | UpdatePreCheckoutQuery
  deriving (Generic)

instance ToJSON   UpdateType where toJSON = gtoJSON
instance FromJSON UpdateType where parseJSON = gparseJSON

makeDefault ''GetUpdatesRequest
