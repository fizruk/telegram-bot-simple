{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatFullInfo where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Birthdate
import Telegram.Bot.API.Types.BusinessIntro
import Telegram.Bot.API.Types.BusinessLocation
import Telegram.Bot.API.Types.BusinessOpeningHours
import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.ChatLocation
import Telegram.Bot.API.Types.ChatPhoto
import Telegram.Bot.API.Types.ChatPermissions
import Telegram.Bot.API.Types.ChatType
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.Message
import Telegram.Bot.API.Types.ReactionType
import Telegram.Bot.API.Internal.Utils

-- ** Chat

-- | This object contains full information about a chat.
--
-- <https://core.telegram.org/bots/api#chatfullinfo>
data ChatFullInfo = ChatFullInfo
  { chatFullInfoId               :: ChatId          -- ^ Unique identifier for this chat. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , chatFullInfoIsBot            :: Maybe Bool      -- ^ 'True', if this user is a bot.
  , chatFullInfoType             :: ChatType        -- ^ Type of chat.
  , chatFullInfoTitle            :: Maybe Text      -- ^ Title, for supergroups, channels and group chats.
  , chatFullInfoUsername         :: Maybe Text      -- ^ Username, for private chats, supergroups and channels if available.
  , chatFullInfoFirstName        :: Maybe Text      -- ^ First name of the other party in a private chat.
  , chatFullInfoLastName         :: Maybe Text      -- ^ Last name of the other party in a private chat.
  , chatFullInfoIsForum          :: Maybe Bool      -- ^ 'True', if the supergroup chat is a forum (has topics enabled).
  , chatFullInfoAccentColorId    :: Maybe Int -- ^ Identifier of the accent color for the chat name and backgrounds of the chat photo, reply header, and link preview. See [accent colors](https://core.telegram.org/bots/api#accent-colors) for more details. Returned only in 'getChat'. Always returned in 'getChat'.
  , chatFullInfoMaxReactionCount :: Int -- ^ The maximum number of reactions that can be set on a message in the chat.
  , chatFullInfoPhoto            :: Maybe ChatPhoto -- ^ Chat photo. Returned only in getChat.
  , chatFullInfoActiveUsernames  :: Maybe [Text]      -- ^ If non-empty, the list of all active chat usernames; for private chats, supergroups and channels. Returned only in 'getChat'.
  , chatFullInfoBirthdate        :: Maybe Birthdate -- ^ For private chats, the date of birth of the user.
  , chatFullInfoBusinessIntro    :: Maybe BusinessIntro -- ^ For private chats with business accounts, the intro of the business.
  , chatFullInfoBusinessLocation :: Maybe BusinessLocation -- ^ For private chats with business accounts, the location of the business.
  , chatFullInfoBusinessOpeningHours :: Maybe BusinessOpeningHours -- ^ For private chats with business accounts, the opening hours of the business.
  , chatFullInfoPersonalChat     :: Maybe Chat -- ^ For private chats, the personal channel of the user. 
  , chatFullInfoAvailableReactions :: Maybe [ReactionType] -- ^ List of available reactions allowed in the chat. If omitted, then all emoji reactions are allowed. Returned only in 'getChat'.
  , chatFullInfoBackgroundCustomEmojiId :: Maybe Text -- ^ Custom emoji identifier of emoji chosen by the chat for the reply header and link preview background. Returned only in 'getChat'.
  , chatFullInfoProfileAccentColorId :: Maybe Int -- ^ Identifier of the accent color for the chat's profile background. See [profile accent colors](https://core.telegram.org/bots/api#profile-accent-colors) for more details. Returned only in getChat.
  , chatFullInfoProfileBackgroundCustomEmojiId :: Maybe Text -- ^ Custom emoji identifier of the emoji chosen by the chat for its profile background. Returned only in 'getChat'.
  , chatFullInfoEmojiStatusCustomEmojiId :: Maybe Text -- ^ Custom emoji identifier of emoji status of the other party in a private chat. Returned only in 'getChat'.
  , chatFullInfoEmojiStatusExpirationDate :: Maybe POSIXTime -- ^ Expiration date of the emoji status of the chat or the other party in a private chat, in Unix time, if any. Returned only in 'getChat'.
  , chatFullInfoBio              :: Maybe Text      -- ^ Bio of the other party in a private chat. Returned only in `getChat`.
  , chatFullInfoHasPrivateForwards :: Maybe Bool    -- ^ 'True', if privacy settings of the other party in the private chat allows to use `tg://user?id=<user_id>` links only in chats with the user. Returned only in getChat.
  , chatFullInfoHasRestrictedVoiceAndVideoMessages :: Maybe Bool -- ^ 'True', if the privacy settings of the other party restrict sending voice and video note messages in the private chat. Returned only in 'getChat'.
  , chatFullInfoJoinToSendMessages :: Maybe Bool    -- ^ 'True', if users need to join the supergroup before they can send messages. Returned only in 'getChat'.
  , chatFullInfoJoinByRequest    :: Maybe Bool      -- ^ 'True', if all users directly joining the supergroup need to be approved by supergroup administrators. Returned only in 'getChat'.
  , chatFullInfoDescription      :: Maybe Text      -- ^ Description, for supergroups and channel chats. Returned only in getChat.
  , chatFullInfoInviteLink       :: Maybe Text      -- ^ Chat invite link, for supergroups and channel chats. Returned only in getChat.
  , chatFullInfoPinnedMessage    :: Maybe Message   -- ^ Pinned message, for supergroups. Returned only in getChat.
  , chatFullInfoPermissions      :: Maybe ChatPermissions -- ^ Default chat member permissions, for groups and supergroups.
  , chatFullInfoSlowModeDelay    :: Maybe Int       -- ^ For supergroups, the minimum allowed delay between consecutive messages sent by each unpriviledged user; in seconds.
  , chatFullInfoUnrestrictBootCount :: Maybe Int -- ^ For supergroups, the minimum number of boosts that a non-administrator user needs to add in order to ignore slow mode and chat permissions.
  , chatFullInfoMessageAutoDeleteTime :: Maybe POSIXTime -- ^ The time after which all messages sent to the chat will be automatically deleted; in seconds.
  , chatFullInfoHasAggressiveAntiSpamEnabled :: Maybe Bool -- ^ 'True', if aggressive anti-spam checks are enabled in the supergroup. The field is only available to chat administrators. Returned only in 'getChat'.
  , chatFullInfoHasHiddenMembers :: Maybe Bool      -- ^ 'True', if non-administrators can only get the list of bots and administrators in the chat. Returned only in 'getChat'.
  , chatFullInfoHasProtectedContent :: Maybe Bool   -- ^ 'True', if messages from the chat can't be forwarded to other chats.
  , chatFullInfoHasVisibleHistory :: Maybe Bool     -- ^ 'True', if new chat members will have access to old messages; available only to chat administrators. Returned only in 'getChat'.
  , chatFullInfoStickerSetName   :: Maybe Text      -- ^ For supergroups, name of group sticker set. Returned only in getChat.
  , chatFullInfoCanSetStickerSet :: Maybe Bool      -- ^ True, if the bot can change the group sticker set. Returned only in `getChat`.
  , chatFullInfoCustomEmojiStickerSet :: Maybe Text -- ^ For supergroups, the name of the group's custom emoji sticker set. Custom emoji from this set can be used by all users and bots in the group.
  , chatFullInfoLinkedChatId     :: Maybe ChatId    -- ^ Unique identifier for the linked chat, i.e. the discussion group identifier for a channel and vice versa; for supergroups and channel chats. This identifier may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , chatFullInfoLocation         :: Maybe ChatLocation -- ^ For supergroups, the location to which the supergroup is connected. Returned only in getChat.
  }
  deriving (Generic, Show)

instance ToJSON   ChatFullInfo where toJSON = gtoJSON
instance FromJSON ChatFullInfo where parseJSON = gparseJSON
