{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatMember where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'ChatMember'

-- | This object contains information about one member of a chat.
data ChatMember = ChatMember
  { chatMemberUser                  :: User -- ^ Information about the user
  , chatMemberStatus                :: Text -- ^ The member's status in the chat. Can be “owner”, “administrator”, “member”, “restricted”, “left” or “banned”.

  -- banned, restricted
  , chatMemberUntilDate             :: Maybe POSIXTime -- ^ Restictred and banned only. Date when restrictions will be lifted for this user, unix time.

  -- owner, administrator
  , chatMemberIsAnonymous           :: Maybe Bool -- ^ Owners and administrators only. 'True', if the user's presence in the chat is hidden.
  , chatMemberCustomTitle           :: Maybe Text -- ^ Owners and administrators only. Custom title for this user.

  -- administrator
  , chatMemberCanBeEdited           :: Maybe Bool -- ^ Administrators only. 'True', if the bot is allowed to edit administrator privileges of that user
  , chatMemberCanManageChat         :: Maybe Bool -- ^ Administrators only. 'True', if the administrator can access the chat event log, chat statistics, message statistics in channels, see channel members, see anonymous administrators in supergroups and ignore slow mode. Implied by any other administrator privilege.
  , chatMemberCanDeleteMessages     :: Maybe Bool -- ^ Administrators only. 'True', if the administrator can delete messages of other users.
  , chatMemberCanManageVideoChats   :: Maybe Bool -- ^ Administrators only. 'True', if the administrator can manage video (previously, voice) chats.
  , chatMemberCanRestrictMembers    :: Maybe Bool -- ^ Administrators only. 'True', if the administrator can restrict, ban or unban chat members.
  , chatMemberCanPromoteMembers     :: Maybe Bool -- ^ Administrators only. 'True', if the administrator can add new administrators with a subset of his own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by the user).
  , chatMemberCanChangeInfo         :: Maybe Bool -- ^ Administrators only. 'True', if the administrator can change the chat title, photo and other settings.
  , chatMemberCanPostMessages       :: Maybe Bool -- ^ Administrators only. 'True', if the administrator can post in the channel, channels only.
  , chatMemberCanEditMessages       :: Maybe Bool -- ^ Administrators only. 'True', if the administrator can edit messages of other users and can pin messages, channels only.

  -- administrator, restricted
  , chatMemberCanInviteUsers        :: Maybe Bool -- ^ Administrators and restricted only. 'True', if the administrator can invite new users to the chat.
  , chatMemberCanPinMessages        :: Maybe Bool -- ^ Administrators and restricted only. 'True', if the administrator can pin messages, supergroups only.
  , chatMemberCanManageTopics       :: Maybe Bool -- ^ Administrators and restricted only. 'True', if the user is allowed to create, rename, close, and reopen forum topics; supergroups only.

  -- restricted
  , chatMemberIsMember              :: Maybe Bool -- ^ Restricted only. 'True', if the user is a member of the chat at the moment of the request.
  , chatMemberCanSendMessages       :: Maybe Bool -- ^ Restricted only. 'True', if the user can send text messages, contacts, locations and venues.
  , chatMemberCanSendAudios         :: Maybe Bool -- ^ Restricted only. 'True', if the user is allowed to send audios.
  , chatMemberCanSendDocuments      :: Maybe Bool -- ^ Restricted only. 'True', if the user is allowed to send documents.
  , chatMemberCanSendPhotos         :: Maybe Bool -- ^ Restricted only. 'True', if the user is allowed to send photos.
  , chatMemberCanSendVideos         :: Maybe Bool -- ^ Restricted only. 'True', if the user is allowed to send videos.
  , chatMemberCanSendVideoNotes     :: Maybe Bool -- ^ Restricted only. 'True', if the user is allowed to send video notes.
  , chatMemberCanSendVoiceNotes     :: Maybe Bool -- ^ Restricted only. 'True', if the user is allowed to send voice notes.
  , chatMemberCanSendPolls          :: Maybe Bool -- ^ Restricted only. 'True', if the user is allowed to send polls.
  , chatMemberCanSendOtherMessages  :: Maybe Bool -- ^ Restricted only. 'True', if the user can send animations, games, stickers and use inline bots, implies can_send_media_messages.
  , chatMemberCanAddWebPagePreviews :: Maybe Bool -- ^ Restricted only. 'True', if user may add web page previews to his messages, implies can_send_media_messages.
  }
  deriving (Generic, Show)

instance ToJSON   ChatMember where toJSON = gtoJSON
instance FromJSON ChatMember where parseJSON = gparseJSON
