{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Chat where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.ChatLocation
import Telegram.Bot.API.Types.ChatPhoto
import Telegram.Bot.API.Types.ChatPermissions
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.Message
import Telegram.Bot.API.Internal.Utils

-- ** Chat

-- | This object represents a chat.
--
-- <https://core.telegram.org/bots/api#chat>
data Chat = Chat
  { chatId               :: ChatId          -- ^ Unique identifier for this chat. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , chatType             :: ChatType        -- ^ Type of chat.
  , chatTitle            :: Maybe Text      -- ^ Title, for supergroups, channels and group chats
  , chatUsername         :: Maybe Text      -- ^ Username, for private chats, supergroups and channels if available
  , chatFirstName        :: Maybe Text      -- ^ First name of the other party in a private chat
  , chatLastName         :: Maybe Text      -- ^ Last name of the other party in a private chat
  , chatIsForum          :: Maybe Bool      -- ^ 'True', if the supergroup chat is a forum (has topics enabled).
  , chatPhoto            :: Maybe ChatPhoto -- ^ Chat photo. Returned only in getChat.
  , chatActiveUsernames  :: Maybe Text      -- ^ If non-empty, the list of all active chat usernames; for private chats, supergroups and channels. Returned only in 'getChat'.
  , chatEmojiStatusCustomEmojiId :: Maybe Text -- ^ Custom emoji identifier of emoji status of the other party in a private chat. Returned only in 'getChat'.
  , chatEmojiStatusExpirationDate :: Maybe Int -- ^ Expiration date of the emoji status of the chat or the other party in a private chat, in Unix time, if any. Returned only in 'getChat'.
  , chatBio              :: Maybe Text      -- ^ Bio of the other party in a private chat. Returned only in `getChat`.
  , chatHasPrivateForwards :: Maybe Bool    -- ^ 'True', if privacy settings of the other party in the private chat allows to use `tg://user?id=<user_id>` links only in chats with the user. Returned only in getChat.
  , chatHasRestrictedVoiceAndVideoMessages :: Maybe Bool -- ^ 'True', if the privacy settings of the other party restrict sending voice and video note messages in the private chat. Returned only in 'getChat'.
  , chatJoinToSendMessages :: Maybe Bool    -- ^ 'True', if users need to join the supergroup before they can send messages. Returned only in 'getChat'.
  , chatJoinByRequest    :: Maybe Bool      -- ^ 'True', if all users directly joining the supergroup need to be approved by supergroup administrators. Returned only in 'getChat'.
  , chatDescription      :: Maybe Text      -- ^ Description, for supergroups and channel chats. Returned only in getChat.
  , chatInviteLink       :: Maybe Text      -- ^ Chat invite link, for supergroups and channel chats. Returned only in getChat.
  , chatPinnedMessage    :: Maybe Message   -- ^ Pinned message, for supergroups. Returned only in getChat.
  , chatPermissions      :: Maybe ChatPermissions -- ^ Default chat member permissions, for groups and supergroups.
  , chatSlowModeDelay    :: Maybe Int       -- ^ For supergroups, the minimum allowed delay between consecutive messages sent by each unpriviledged user; in seconds.
  , chatMessageAutoDeleteTime :: Maybe POSIXTime -- ^ The time after which all messages sent to the chat will be automatically deleted; in seconds.
  , chatHasProtectedContent :: Maybe Bool   -- ^ 'True', if messages from the chat can't be forwarded to other chats.
  , chatStickerSetName   :: Maybe Text      -- ^ For supergroups, name of group sticker set. Returned only in getChat.
  , chatCanSetStickerSet :: Maybe Bool      -- ^ True, if the bot can change the group sticker set. Returned only in `getChat`.
  , chatLinkedChatId     :: Maybe ChatId    -- ^ Unique identifier for the linked chat, i.e. the discussion group identifier for a channel and vice versa; for supergroups and channel chats. This identifier may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , chatLocation         :: Maybe ChatLocation -- ^ For supergroups, the location to which the supergroup is connected. Returned only in getChat.
  }
  deriving (Generic, Show)

instance ToJSON   Chat where toJSON = gtoJSON
instance FromJSON Chat where parseJSON = gparseJSON

-- | Type of chat.
data ChatType
  = ChatTypePrivate
  | ChatTypeGroup
  | ChatTypeSupergroup
  | ChatTypeChannel
  | ChatTypeSender
  deriving (Generic, Show)

instance ToJSON   ChatType where
  toJSON = gtoJSON
instance FromJSON ChatType where
  parseJSON = gparseJSON
