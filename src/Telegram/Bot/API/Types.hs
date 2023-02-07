{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Telegram.Bot.API.Types where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, KeyValue ((.=)), withObject, (.:))
import Data.Aeson.Types (Parser, Pair, Object)
import Data.Aeson.Text (encodeToLazyText)
import Data.Coerce (coerce)
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.String
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Servant.API
import Servant.Multipart.API
import System.FilePath

import Telegram.Bot.API.Internal.Utils

type RequiredQueryParam = QueryParam' '[Required , Strict]

newtype Seconds = Seconds Int
  deriving (Eq, Show, Num, ToJSON, FromJSON)

-- * Available types

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

-- | Unique identifier for this user or bot.
newtype UserId = UserId Integer
  deriving (Eq, Show, ToJSON, FromJSON)

instance ToHttpApiData UserId where
  toUrlPiece = pack . show @Integer . coerce

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

-- | Unique identifier for this chat.
newtype ChatId = ChatId Integer
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData ChatId where
  toUrlPiece a = pack . show @Integer $ coerce a

-- | Type of chat.
data ChatType
  = ChatTypePrivate
  | ChatTypeGroup
  | ChatTypeSupergroup
  | ChatTypeChannel
  deriving (Generic, Show)

instance ToJSON   ChatType where
  toJSON = gtoJSON
instance FromJSON ChatType where
  parseJSON = gparseJSON

-- ** Message

-- | This object represents a message.
data Message = Message
  { messageMessageId             :: MessageId -- ^ Unique message identifier inside this chat.
  , messageMessageThreadId       :: Maybe MessageThreadId -- ^ Unique identifier of a message thread to which the message belongs; for supergroups only.
  , messageFrom                  :: Maybe User -- ^ Sender, empty for messages sent to channels.
  , messageSenderChat            :: Maybe Chat -- ^ Sender of the message, sent on behalf of a chat. For example, the channel itself for channel posts, the supergroup itself for messages from anonymous group administrators, the linked channel for messages automatically forwarded to the discussion group. For backward compatibility, the field from contains a fake sender user in non-channel chats, if the message was sent on behalf of a chat.
  , messageDate                  :: POSIXTime -- ^ Date the message was sent in Unix time.
  , messageChat                  :: Chat -- ^ Conversation the message belongs to.
  , messageForwardFrom           :: Maybe User -- ^ For forwarded messages, sender of the original message.
  , messageForwardFromChat       :: Maybe Chat -- ^ For messages forwarded from channels, information about the original channel.
  , messageForwardFromMessageId  :: Maybe MessageId -- ^ For messages forwarded from channels, identifier of the original message in the channel.
  , messageForwardSignature      :: Maybe Text -- ^ For messages forwarded from channels, signature of the post author if present.
  , messageForwardSenderName     :: Maybe Text -- ^ Sender's name for messages forwarded from users who disallow adding a link to their account in forwarded messages.
  , messageForwardDate           :: Maybe POSIXTime -- ^ For forwarded messages, date the original message was sent in Unix time
  , messageIsTopicMessage        :: Maybe Bool -- ^ 'True', if the message is sent to a forum topic.
  , messageIsAutomaticForward    :: Maybe Bool -- ^ 'True', if the message is a channel post that was automatically forwarded to the connected discussion group.
  , messageReplyToMessage        :: Maybe Message -- ^ For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
  , messageViaBot                :: Maybe User -- ^ Bot through which the message was sent.
  , messageEditDate              :: Maybe POSIXTime -- ^ Date the message was last edited in Unix time
  , messageHasProtectedContent   :: Maybe Bool -- ^ 'True', if the message can't be forwarded.
  , messageMediaGroupId          :: Maybe MediaGroupId -- ^ The unique identifier of a media message group this message belongs to
  , messageAuthorSignature       :: Maybe Text -- ^ Signature of the post author for messages in channels
  , messageText                  :: Maybe Text -- ^ For text messages, the actual UTF-8 text of the message, 0-4096 characters.
  , messageEntities              :: Maybe [MessageEntity] -- ^ For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
  , messageAnimation             :: Maybe Animation -- ^ Message is an animation, information about the animation. For backward compatibility, when this field is set, the document field will also be set.
  , messageAudio                 :: Maybe Audio -- ^ Message is an audio file, information about the file
  , messageDocument              :: Maybe Document -- ^ Message is a general file, information about the file.
  , messagePhoto                 :: Maybe [PhotoSize] -- ^ Message is a photo, available sizes of the photo
  , messageSticker               :: Maybe Sticker -- ^ Message is a sticker, information about the sticker
  , messageVideo                 :: Maybe Video -- ^ Message is a video, information about the video
  , messageVideoNote             :: Maybe VideoNote -- ^ Message is a video note, information about the video message
  , messageVoice                 :: Maybe Voice -- ^ Message is a voice message, information about the file
  , messageCaption               :: Maybe Text -- ^ Caption for the audio, document, photo, video or voice, 0-200 characters
  , messageCaptionEntities       :: Maybe [MessageEntity] -- ^ For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption.
  , messageHasMediaSpoiler       :: Maybe Bool -- ^ 'True', if the message media is covered by a spoiler animation.
  , messageContact               :: Maybe Contact -- ^ Message is a shared contact, information about the contact
  , messageDice                  :: Maybe Dice -- ^ Message is a dice with random value.
  , messageGame                  :: Maybe Game -- ^ Message is a game, information about the game. More about games »  , messageLocation              :: Maybe Location -- ^ Message is a shared location, information about the location
  , messagePoll                  :: Maybe Poll -- ^ Message is a native poll, information about the poll.
  , messageVenue                 :: Maybe Venue -- ^ Message is a venue, information about the venue
  , messageLocation              :: Maybe Location -- ^ Message is a shared location, information about the location.
  , messageNewChatMembers        :: Maybe [User] -- ^ New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
  , messageLeftChatMember        :: Maybe User -- ^ A member was removed from the group, information about them (this member may be the bot itself)
  , messageNewChatTitle          :: Maybe Text -- ^ A chat title was changed to this value
  , messageNewChatPhoto          :: Maybe [PhotoSize] -- ^ A chat photo was change to this value
  , messageDeleteChatPhoto       :: Maybe Bool -- ^ Service message: the chat photo was deleted
  , messageGroupChatCreated      :: Maybe Bool -- ^ Service message: the group has been created
  , messageSupergroupChatCreated :: Maybe Bool -- ^ Service message: the supergroup has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a supergroup when it is created. It can only be found in reply_to_message if someone replies to a very first message in a directly created supergroup.
  , messageChannelChatCreated    :: Maybe Bool -- ^ Service message: the channel has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a channel when it is created. It can only be found in reply_to_message if someone replies to a very first message in a channel.
  , messageAutoDeleteTimerChanged :: Maybe MessageAutoDeleteTimerChanged -- ^ Service message: auto-delete timer settings changed in the chat.
  , messageHasAggressiveAntiSpamEnabled :: Maybe Bool -- ^ 'True', if aggressive anti-spam checks are enabled in the supergroup. The field is only available to chat administrators. Returned only in 'getChat'.
  , messageHasHiddenMembers      :: Maybe Bool -- ^ 'True', if non-administrators can only get the list of bots and administrators in the chat. Returned only in 'getChat'.
  , messageMigrateToChatId       :: Maybe ChatId -- ^ The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , messageMigrateFromChatId     :: Maybe ChatId -- ^ The supergroup has been migrated from a group with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , messagePinnedMessage         :: Maybe Message -- ^ Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply.
  , messageInvoice               :: Maybe Invoice -- ^ Message is an invoice for a payment, information about the invoice.
  , messageSuccessfulPayment     :: Maybe SuccessfulPayment -- ^ Message is a service message about a successful payment, information about the payment.
  , messageUserShared            :: Maybe UserShared -- ^ Service message: a user was shared with the bot.
  , messageChatShared            :: Maybe ChatShared -- ^ Service message: a chat was shared with the bot.
  , messageConnectedWebsite      :: Maybe Text -- ^ The domain name of the website on which the user has logged in.
  , messageWriteAccessAllowed    :: Maybe WriteAccessAllowed -- ^ Service message: the user allowed the bot added to the attachment menu to write messages.
  , messagePassportData          :: Maybe PassportData -- ^ Telegram Passport data.
  , messageProximityAlertTriggered :: Maybe ProximityAlertTriggered -- ^ Service message. A user in the chat triggered another user's proximity alert while sharing Live Location.
  , messageForumTopicCreated     :: Maybe ForumTopicCreated -- ^ Service message: forum topic created.
  , messageForumTopicEdited     :: Maybe ForumTopicEdited -- ^ Service message: forum topic edited.
  , messageForumTopicClosed     :: Maybe ForumTopicClosed -- ^ Service message: forum topic closed.
  , messageForumTopicReopened     :: Maybe ForumTopicReopened -- ^ Service message: forum topic reopened.
  , messageVideoChatScheduled    :: Maybe VideoChatScheduled -- ^ Service message: video chat scheduled.
  , messageVideoChatStarted      :: Maybe VideoChatStarted -- ^ Service message: video chat started
  , messageVideoChatEnded        :: Maybe VideoChatEnded -- ^ Service message: video chat ended.
  , messageVideoChatParticipantsInvited :: Maybe VideoChatParticipantsInvited -- ^ Service message: new participants invited to a video chat.
  , messageWebAppData            :: Maybe WebAppData -- ^ Service message: data sent by a Web App.
  , messageReplyMarkup           :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message. `login_url` buttons are represented as ordinary `url` buttons.
  }
  deriving (Generic, Show)

-- | Unique message identifier inside this chat.
newtype MessageId = MessageId Integer
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData MessageId where
  toUrlPiece a = pack . show @Integer $ coerce a

-- | Unique identifier of a message thread to which the message belongs; for supergroups only.
newtype MessageThreadId = MessageThreadId Integer
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData MessageThreadId where
  toUrlPiece a = pack . show @Integer $ coerce a

-- | The unique identifier of a media message group a message belongs to.
newtype MediaGroupId = MediaGroupId Text
  deriving (Eq, Show, ToJSON, FromJSON)

-- | Signed 32-bit identifier of the request, which will be received back in the 'UserShared' or 'ChatShared' object. Must be unique within the message.
newtype RequestId = RequestId Integer
  deriving (Eq, Show, ToJSON, FromJSON)

-- ** MessageEntity

-- | This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
  { messageEntityType   :: MessageEntityType -- ^ Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), underline (underlined text), strikethrough, code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
  , messageEntityOffset :: Int -- ^ Offset in UTF-16 code units to the start of the entity
  , messageEntityLength :: Int -- ^ Length of the entity in UTF-16 code units
  , messageEntityUrl    :: Maybe Text -- ^ For “text_link” only, url that will be opened after user taps on the text
  , messageEntityUser   :: Maybe User -- ^ For “text_mention” only, the mentioned user
  , messageEntityLanguage :: Maybe Text -- ^ For “pre” only, the programming language of the entity text.
  , messageEntityCustomEmojiId :: Maybe Text -- ^ For “custom_emoji” only, unique identifier of the custom emoji. Use @getCustomEmojiStickers@ to get full information about the sticker.
  }
  deriving (Generic, Show)

-- | Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), underline (underlined text), strikethrough, code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames), cashtag, phone_number
data MessageEntityType
  = MessageEntityMention
  | MessageEntityHashtag
  | MessageEntityBotCommand
  | MessageEntityUrl
  | MessageEntityEmail
  | MessageEntityBold
  | MessageEntityItalic
  | MessageEntityUnderline -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_underline.html>
  | MessageEntityStrikethrough -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_strikethrough.html>
  | MessageEntityCode
  | MessageEntityPre
  | MessageEntityTextLink
  | MessageEntityTextMention
  | MessageEntityCashtag -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_cashtag.html>.
  | MessageEntityPhoneNumber -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_phone_number.html>.
  | MessageEntitySpoiler
  | MessageEntityCustomEmoji
  deriving (Eq, Show, Generic)

-- ** 'PhotoSize'

-- | This object represents one size of a photo or a file / sticker thumbnail.
data PhotoSize = PhotoSize
  { photoSizeFileId       :: FileId      -- ^ Unique identifier for this file.
  , photoSizeFileUniqueId :: FileId      -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , photoSizeWidth        :: Int       -- ^ Photo width
  , photoSizeHeight       :: Int       -- ^ Photo height
  , photoSizeFileSize     :: Maybe Int -- ^ File size
  }
  deriving (Generic, Show)

-- | Unique identifier for this file.
newtype FileId = FileId Text
  deriving (Eq, Show, ToJSON, FromJSON)

instance ToHttpApiData FileId where
  toUrlPiece = coerce

-- ** 'Animation'

-- | This object represents an animation file (GIF or H.264/MPEG-4 AVC video without sound).
data Animation = Animation
  { animationFileId       :: FileId          -- ^ Identifier for this file, which can be used to download or reuse the file.
  , animationFileUniqueId :: FileId          -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , animationWidth        :: Int           -- ^ Video width as defined by sender.
  , animationHeight       :: Int           -- ^ Video height as defined by sender.
  , animationDuration     :: Seconds         -- ^ Duration of the video in seconds as defined by sender.
  , animationThumb        :: Maybe PhotoSize -- ^ Animation thumbnail as defined by sender.
  , animationFileName     :: Maybe Text      -- ^ Original animation filename as defined by sender.
  , animationMimeType     :: Maybe Text      -- ^ MIME type of the file as defined by sender.
  , animationFileSize     :: Maybe Integer   -- ^ File size in bytes.
  }
  deriving (Generic, Show)

-- ** 'Audio'

-- | This object represents an audio file to be treated as music by the Telegram clients.
data Audio = Audio
  { audioFileId    :: FileId -- ^ Unique identifier for this file.
  , audioFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , audioDuration  :: Seconds -- ^ Duration of the audio in seconds as defined by sender.
  , audioPerformer :: Maybe Text -- ^ Performer of the audio as defined by sender or by audio tags.
  , audioTitle     :: Maybe Text -- ^ Title of the audio as defined by sender or by audio tags.
  , audioFileName  :: Maybe Text -- ^ Original filename as defined by sender.
  , audioMimeType  :: Maybe Text -- ^ MIME type of the file as defined by sender.
  , audioFileSize  :: Maybe Integer -- ^ File size in bytes.
  , audioThumb     :: Maybe PhotoSize -- ^ Thumbnail of the album cover to which the music file belongs.
  }
  deriving (Generic, Show)

-- ** 'Document'

-- | This object represents a general file (as opposed to photos, voice messages and audio files).
data Document = Document
  { documentFileId   :: FileId -- ^ Unique file identifier.
  , documentFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , documentThumb    :: Maybe PhotoSize -- ^ Document thumbnail as defined by sender.
  , documentFileName :: Maybe Text -- ^ Original filename as defined by sender.
  , documentMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender.
  , documentFileSize :: Maybe Integer -- ^ File size in bytes. 
  }
  deriving (Generic, Show)

-- ** 'Video'

-- | This object represents a video file.
data Video = Video
  { videoFileId       :: FileId -- ^ Unique identifier for this file.
  , videoFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , videoWidth        :: Int -- ^ Video width as defined by sender.
  , videoHeight       :: Int -- ^ Video height as defined by sender.
  , videoDuration     :: Seconds -- ^ Duration of the video in seconds as defined by sender.
  , videoThumb        :: Maybe PhotoSize -- ^ Video thumbnail.
  , videoFileName     :: Maybe Text -- ^ Original filename as defined by sender.
  , videoMimeType     :: Maybe Text -- ^ Mime type of a file as defined by sender.
  , videoFileSize     :: Maybe Integer -- ^ File size in bytes.
  }
  deriving (Generic, Show)

-- ** 'VideoNote'

-- | This object represents a video message (available in Telegram apps as of v.4.0).
data VideoNote = VideoNote
  { videoNoteFileId   :: FileId -- ^ Unique identifier for this file.
  , videoNoteFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , videoNoteLength   :: Int -- ^ Video width and height as defined by sender.
  , videoNoteDuration :: Seconds -- ^ Duration of the video in seconds as defined by sender.
  , videoNoteThumb    :: Maybe PhotoSize -- ^ Video thumbnail.
  , videoNoteFileSize :: Maybe Integer -- ^ File size in bytes.
  }
  deriving (Generic, Show)

-- ** 'Voice'

-- | This object represents a voice note.
data Voice = Voice
  { voiceFileId   :: FileId -- ^ Unique identifier for this file.
  , voiceFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , voiceDuration :: Seconds -- ^ Duration of the audio in seconds as defined by sender.
  , voiceMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender.
  , voiceFileSize :: Maybe Integer -- ^ File size in bytes.
  }
  deriving (Generic, Show)

-- ** 'Contact'

-- | This object represents a phone contact.
data Contact = Contact
  { contactPhoneNumber :: Text -- ^ Contact's phone number.
  , contactFirstName   :: Text -- ^ Contact's first name.
  , contactLastName    :: Maybe Text -- ^ Contact's last name.
  , contactUserId      :: Maybe UserId -- ^ Contact's user identifier in Telegram.
  , contactVcard       :: Maybe Text -- ^ Additional data about the contact in the form of a vCard.
  }
  deriving (Generic, Show)

-- ** 'Dice'

-- | This object represents an animated emoji that displays a random value.
data Dice = Dice
  { diceEmoji :: Text -- ^ Emoji on which the dice throw animation is based.
  , diceValue :: Int  -- ^ Value of the dice, 1-6 for “🎲”, “🎯” and “🎳” base emoji, 1-5 for “🏀” and “⚽” base emoji, 1-64 for “🎰” base emoji
  }
  deriving (Generic, Show)

-- ** 'PollOption'

-- | This object contains information about one answer option in a poll.
data PollOption = PollOption
  { pollOptionText       :: Text -- ^ Option text, 1-100 characters.
  , pollOptionVoterCount :: Int  -- ^ Number of users that voted for this option.
  }
  deriving (Generic, Show)

-- ** 'PollAnswer'

-- | This object represents an answer of a user in a non-anonymous poll.
data PollAnswer = PollAnswer
  { pollAnswerPollId    :: PollId -- ^ Unique poll identifier.
  , pollAnswerUser      :: User   -- ^ The user, who changed the answer to the poll.
  , pollAnswerOptionIds :: [Int]  -- ^ 0-based identifiers of answer options, chosen by the user. May be empty if the user retracted their vote.
  }
  deriving (Generic, Show)

-- | Unique poll identifier.
newtype PollId = PollId Text
  deriving (Eq, Show, ToJSON, FromJSON)

-- ** 'Poll'

data Poll = Poll
  { pollId                    :: PollId                -- ^ Unique poll identifier.
  , pollQuestion              :: Text                  -- ^ Poll question, 1-300 characters.
  , pollOptions               :: [PollOption]          -- ^ List of poll options.
  , pollTotalVoterCount       :: Int                 -- ^ Total number of users that voted in the poll.
  , pollIsClosed              :: Bool                  -- ^ 'True', if the poll is closed.
  , pollIsAnonymous           :: Bool                  -- ^ 'True', if the poll is anonymous.
  , pollType                  :: PollType              -- ^ Poll type, currently can be “regular” or “quiz”.
  , pollAllowsMultipleAnswers :: Bool                  -- ^ 'True', if the poll allows multiple answers.
  , pollCorrectOptionId       :: Maybe Int             -- ^ 0-based identifier of the correct answer option. Available only for polls in the quiz mode, which are closed, or was sent (not forwarded) by the bot or to the private chat with the bot.
  , pollExplanation           :: Maybe Text            -- ^ Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters.
  , pollExplanationEntities   :: Maybe [MessageEntity] -- ^ Special entities like usernames, URLs, bot commands, etc. that appear in the explanation.
  , pollOpenPeriod            :: Maybe Seconds         -- ^ Amount of time in seconds the poll will be active after creation.
  , pollCloseData             :: Maybe POSIXTime       -- ^ Point in time (Unix timestamp) when the poll will be automatically closed.
  }
  deriving (Generic, Show)

-- ** Location

-- | This object represents a point on the map.
data Location = Location
  { locationLongitude            :: Float         -- ^ Longitude as defined by sender.
  , locationLatitude             :: Float         -- ^ Latitude as defined by sender.
  , locationHorizontalAccuracy   :: Maybe Float   -- ^ The radius of uncertainty for the location, measured in meters; 0-1500.
  , locationLivePeriod           :: Maybe Seconds -- ^ Time relative to the message sending date, during which the location can be updated; in seconds. For active live locations only.
  , locationHeading              :: Maybe Int     -- ^ The direction in which user is moving, in degrees; 1-360. For active live locations only.
  , locationProximityAlertRadius :: Maybe Int     -- ^ Maximum distance for proximity alerts about approaching another chat member, in meters. For sent live locations only.
  }
  deriving (Generic, Show)

-- ** 'Venue'

-- | This object represents a venue.
data Venue = Venue
  { venueLocation        :: Location   -- ^ Venue location.
  , venueTitle           :: Text       -- ^ Name of the venue.
  , venueAddress         :: Text       -- ^ Address of the venue.
  , venueFoursquareId    :: Maybe Text -- ^ Foursquare identifier of the venue.
  , venueFoursquareType  :: Maybe Text -- ^ Foursquare type of the venue. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
  , venueGooglePlaceId   :: Maybe Text -- ^ Google Places identifier of the venue.
  , venueGooglePlaceType :: Maybe Text -- ^ Google Places type of the venue. (See supported types.)
  }
  deriving (Generic, Show)

-- ** 'ProximityAlertTriggered'

-- | This object represents the content of a service message, sent whenever a user in the chat triggers a proximity alert set by another user.
data ProximityAlertTriggered = ProximityAlertTriggered
  { proximityAlertTriggeredTraveler :: User  -- ^ User that triggered the alert.
  , proximityAlertTriggeredWatcher  :: User  -- ^ User that set the alert.
  , proximityAlertTriggeredDistance :: Int -- ^ The distance between the users.
  }
  deriving (Generic, Show)

-- ** 'MessageAutoDeleteTimerChanged'

-- | This object represents a service message about a change in auto-delete timer settings.
data MessageAutoDeleteTimerChanged = MessageAutoDeleteTimerChanged
  { messageAutoDeleteTimerChangedMessageAutoDeleteTime :: Seconds -- ^ New auto-delete time for messages in the chat; in seconds
  }
  deriving (Generic, Show)

-- ** 'ForumTopicCreated'

-- | This object represents a service message about a new forum topic created in the chat.
data ForumTopicCreated = ForumTopicCreated
  { forumTopicCreatedName              :: Text       -- ^ Name of the topic.
  , forumTopicCreatedIconColor         :: Integer    -- ^ Color of the topic icon in RGB format.
  , forumTopicCreatedIconCustomEmojiId :: Maybe Text -- ^ Unique identifier of the custom emoji shown as the topic icon.
  }
  deriving (Generic, Show)

-- ** 'ForumTopicClosed'

-- | This object represents a service message about a forum topic closed in the chat. Currently holds no information.
newtype ForumTopicClosed = ForumTopicClosed Object
  deriving (Generic, Show)

-- ** 'ForumTopicEdited'

-- | This object represents a service message about an edited forum topic.
data ForumTopicEdited = ForumTopicEdited
  { forumTopicEditedName              :: Maybe Text -- ^ New name of the topic, if it was edited.
  , forumTopicEditedIconCustomEmojiId :: Maybe Text -- ^ New identifier of the custom emoji shown as the topic icon, if it was edited; an empty string if the icon was removed.
  }
  deriving (Generic, Show)

-- ** 'ForumTopicReopened'

-- | This object represents a service message about a forum topic reopened in the chat. Currently holds no information.
newtype ForumTopicReopened = ForumTopicReopened Object
  deriving (Generic, Show)

-- ** 'GeneralForumTopicHidden'

-- | This object represents a service message about General forum topic hidden in the chat. Currently holds no information.
newtype GeneralForumTopicHidden = GeneralForumTopicHidden Object
  deriving (Generic, Show)

-- ** 'GeneralForumTopicUnhidden'

-- | This object represents a service message about General forum topic unhidden in the chat. Currently holds no information.
newtype GeneralForumTopicUnhidden = GeneralForumTopicUnhidden Object
  deriving (Generic, Show)

-- ** 'UserShared'

-- | This object contains information about the user whose identifier was shared with the bot using a 'KeyboardButtonRequestUser' button.
data UserShared = UserShared
  { userSharedRequestId :: RequestId -- ^ Identifier of the request.
  , userSharedUserId :: UserId -- ^ Identifier of the shared user. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier. The bot may not have access to the user and could be unable to use this identifier, unless the user is already known to the bot by some other means.
  }
  deriving (Generic, Show)

-- ** 'ChatShared'

-- | This object contains information about the chat whose identifier was shared with the bot using a 'KeyboardButtonRequestChat' button.
data ChatShared = ChatShared
  { chatSharedRequestId :: RequestId -- ^ Identifier of the request.
  , chatSharedChatId :: ChatId -- ^ Identifier of the shared chat. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier. The bot may not have access to the chat and could be unable to use this identifier, unless the chat is already known to the bot by some other means.
  }
  deriving (Generic, Show)

-- ** 'WriteAccessAllowed'

-- | This object represents a service message about a user allowing a bot added to the attachment menu to write messages. Currently holds no information.
newtype WriteAccessAllowed = WriteAccessAllowed Object
  deriving (Generic, Show)

-- ** 'VideoChatScheduled'

-- | This object represents a service message about a video chat scheduled in the chat.
data VideoChatScheduled = VideoChatScheduled
  { videoChatScheduledStartDate :: POSIXTime -- ^ Point in time (Unix timestamp) when the video chat is supposed to be started by a chat administrator.
  }
  deriving (Generic, Show)

-- ** 'VideoChatStarted'

-- | This object represents a service message about a video chat started in the chat. Currently holds no information.
data VideoChatStarted = VideoChatStarted
  deriving (Generic, Show)

instance ToJSON VideoChatStarted where
  toJSON = gtoJSON

instance FromJSON VideoChatStarted where
  parseJSON (Data.Aeson.Object _) = pure VideoChatStarted
  parseJSON _ = fail "Unable to parse VideoChatStarted: expected an empty object"

-- ** 'VideoChatEnded'

-- | This object represents a service message about a video chat ended in the chat.
data VideoChatEnded = VideoChatEnded
  { videoChatEndedDuration :: Seconds -- ^ Video chat duration in seconds.
  }
  deriving (Generic, Show)

-- ** 'VideoChatParticipantsInvited'
data VideoChatParticipantsInvited = VideoChatParticipantsInvited
  { videoChatParticipantsInvitedUsers :: Maybe [User] -- ^ New members that were invited to the video chat.
  }
  deriving (Generic, Show)

-- ** 'UserProfilePhotos'

-- | This object represent a user's profile pictures.
data UserProfilePhotos = UserProfilePhotos
  { userProfilePhotosTotalCount :: Int -- ^ Total number of profile pictures the target user has
  , userProfilePhotosPhotos     :: [[PhotoSize]] -- ^ Requested profile pictures (in up to 4 sizes each)
  }
  deriving (Generic, Show)

-- ** 'WebAppData'

data WebAppData = WebAppData
  { webAppDataData       :: Text -- ^ The data. Be aware that a bad client can send arbitrary data in this field.
  , webAppDataButtonText :: Text -- ^ Text of the @web_app@ keyboard button, from which the Web App was opened. Be aware that a bad client can send arbitrary data in this field.
  }
  deriving (Generic, Show)

-- ** 'File'

-- | This object represents a file ready to be downloaded.
-- The file can be downloaded via the link @https://api.telegram.org/file/bot<token>/<file_path>@.
-- It is guaranteed that the link will be valid for at least 1 hour.
-- When the link expires, a new one can be requested by calling getFile.
data File = File
  { fileFileId       :: FileId      -- ^ Unique identifier for this file.
  , fileFileUniqueId :: FileId      -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , fileFileSize     :: Maybe Integer -- ^ File size in bytes, if known.
  , fileFilePath     :: Maybe Text  -- ^ File path. Use https://api.telegram.org/file/bot<token>/<file_path> to get the file.
  }
  deriving (Generic, Show)

type ContentType = Text

data InputFile
  = InputFileId FileId
  | FileUrl Text
  | InputFile FilePath ContentType

instance ToJSON InputFile where
  toJSON (InputFileId i) = toJSON i
  toJSON (FileUrl t) = toJSON t
  toJSON (InputFile f _) = toJSON ("attach://" <> pack (takeFileName f))

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

-- ** 'KeyboardButtonRequestUser'

-- | This object defines the criteria used to request a suitable user. The identifier of the selected user will be shared with the bot when the corresponding button is pressed.
data KeyboardButtonRequestUser = KeyboardButtonRequestUser
  { keyboardButtonRequestUserRequestId :: RequestId -- ^ Signed 32-bit identifier of the request, which will be received back in the 'UserShared' object. Must be unique within the message
  , keyboardButtonRequestUserUserIsBot :: Maybe Bool -- ^ Pass 'True' to request a bot, pass 'False' to request a regular user. If not specified, no additional restrictions are applied.
  , keyboardButtonRequestUserUserIsPremium :: Maybe Bool -- ^ Pass 'True' to request a premium user, pass 'False' to request a non-premium user. If not specified, no additional restrictions are applied.
  }
  deriving (Generic, Show)

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
  }
  deriving (Generic, Show)

-- ** 'KeyboardButton'

newtype WebAppInfo = WebAppInfo { webAppInfoUrl :: Text }
  deriving (Generic, Show)

-- | This object represents one button of the reply keyboard.
-- For simple text buttons String can be used instead of this object
-- to specify text of the button. Optional fields are mutually exclusive.
data KeyboardButton = KeyboardButton
  { keyboardButtonText            :: Text       -- ^ Text of the button. If none of the optional fields are used, it will be sent as a message when the button is pressed.
  , keyboardButtonRequestUser     :: Maybe KeyboardButtonRequestUser -- ^ If specified, pressing the button will open a list of suitable users. Tapping on any user will send their identifier to the bot in a “user_shared” service message. Available in private chats only.
  , keyboardButtonRequestChat     :: Maybe KeyboardButtonRequestChat -- ^ If specified, pressing the button will open a list of suitable chats. Tapping on a chat will send its identifier to the bot in a “chat_shared” service message. Available in private chats only.
  , keyboardButtonRequestContact  :: Maybe Bool -- ^ If 'True', the user's phone number will be sent as a contact when the button is pressed. Available in private chats only.
  , keyboardButtonRequestLocation :: Maybe Bool -- ^ If 'True', the user's current location will be sent when the button is pressed. Available in private chats only.
  , keyboardButtonRequestPoll     :: Maybe PollType -- ^ If specified, the user will be asked to create a poll and send it to the bot when the button is pressed. Available in private chats only.
  , keyboardButtonWebApp          :: Maybe WebAppInfo -- ^ If specified, the described Web App will be launched when the button is pressed. The Web App will be able to send a “web_app_data” service message. Available in private chats only.
  }
  deriving (Generic, Show)

instance IsString KeyboardButton where
  fromString s = KeyboardButton (fromString s) Nothing Nothing Nothing Nothing Nothing Nothing

-- ** 'MenuButton'

-- | This object describes the bot's menu button in a private chat.
-- If a menu button other than @MenuButtonDefault@ is set for a private chat, then it is applied in the chat. Otherwise the default menu button is applied. By default, the menu button opens the list of bot commands.
data MenuButton
  = MenuButtonCommands -- ^ Represents a menu button, which opens the bot's list of commands.
  | MenuButtonWebApp -- ^ Represents a menu button, which launches a Web App.
      { menuButtonWebAppText :: Text
      , menuButtonWebAppWebApp :: WebAppInfo
      } 
  | MenuButtonDefault -- ^ Describes that no specific value for the menu button was set.
  deriving Generic

-- ** 'PollType'

data PollType =
  PollTypeQuiz | PollTypeRegular
  deriving (Generic, Show)

getPollType :: PollType -> Text
getPollType PollTypeQuiz = "quiz"
getPollType PollTypeRegular = "regular"

instance ToJSON PollType where
  toJSON = String . getPollType

instance FromJSON PollType where parseJSON = gparseJSON

-- ** 'ReplyKeyboardRemove'

-- | Upon receiving a message with this object,
-- Telegram clients will remove the current custom keyboard
-- and display the default letter-keyboard.
--
-- By default, custom keyboards are displayed until a new keyboard is sent by a bot.
-- An exception is made for one-time keyboards that are hidden immediately after
-- the user presses a button (see 'ReplyKeyboardMarkup').
data ReplyKeyboardRemove = ReplyKeyboardRemove
  { replyKeyboardRemoveRemoveKeyboard :: Bool -- ^ Requests clients to remove the custom keyboard (user will not be able to summon this keyboard; if you want to hide the keyboard from sight but keep it accessible, use one_time_keyboard in ReplyKeyboardMarkup)
  , replyKeyboardRemoveSelective      :: Maybe Bool -- ^ Use this parameter if you want to remove the keyboard for specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
  --
  -- Example: A user votes in a poll, bot returns confirmation message in reply to the vote and removes the keyboard for that user, while still showing the keyboard with poll options to users who haven't voted yet.
  }
  deriving (Generic, Show)

-- ** 'InlineKeyboardMarkup'

-- | This object represents an inline keyboard that appears
-- right next to the message it belongs to.
data InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboardMarkupInlineKeyboard :: [[InlineKeyboardButton]] -- ^ Array of button rows, each represented by an Array of InlineKeyboardButton objects
  }
  deriving (Generic, Show)
-- ^ 
-- **Note**: This will only work in Telegram versions released after 9 April, 2016. Older clients will display unsupported message.

-- ** 'InlineKeyboardButton'

-- | This object represents one button of an inline keyboard. You must use exactly one of the optional fields.
data InlineKeyboardButton = InlineKeyboardButton
  { inlineKeyboardButtonText              :: Text -- ^ Label text on the button
  , inlineKeyboardButtonUrl               :: Maybe Text -- ^ HTTP url to be opened when button is pressed
  , inlineKeyboardButtonCallbackData      :: Maybe Text -- ^ Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes
  , inlineKeyboardButtonWebApp            :: Maybe WebAppInfo -- ^ Description of the Web App that will be launched when the user presses the button. The Web App will be able to send an arbitrary message on behalf of the user using the method @answerWebAppQuery@. Available only in private chats between a user and the bot.
  , inlineKeyboardButtonSwitchInlineQuery :: Maybe Text -- ^ If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot‘s username and the specified inline query in the input field. Can be empty, in which case just the bot’s username will be inserted.
  , inlineKeyboardButtonSwitchInlineQueryCurrentChat :: Maybe Text -- ^ If set, pressing the button will insert the bot‘s username and the specified inline query in the current chat's input field. Can be empty, in which case only the bot’s username will be inserted.

  , inlineKeyboardButtonCallbackGame      :: Maybe CallbackGame -- ^ Description of the game that will be launched when the user presses the button.
  , inlineKeyboardButtonPay               :: Maybe Bool -- ^ Specify True, to send a Pay button.
  }
  deriving (Generic, Show)

labeledInlineKeyboardButton :: Text -> InlineKeyboardButton
labeledInlineKeyboardButton label = InlineKeyboardButton label Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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

-- ** 'CallbackQuery'

-- | This object represents an incoming callback query from a callback button
-- in an inline keyboard. If the button that originated the query was attached
-- to a message sent by the bot, the field message will be present.
-- If the button was attached to a message sent via the bot (in inline mode),
-- the field @inline_message_id@ will be present.
-- Exactly one of the fields data or game_short_name will be present.
data CallbackQuery = CallbackQuery
  { callbackQueryId              :: CallbackQueryId -- ^ Unique identifier for this query
  , callbackQueryFrom            :: User -- ^ Sender
  , callbackQueryMessage         :: Maybe Message -- ^ Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old
  , callbackQueryInlineMessageId :: Maybe MessageId -- ^ Identifier of the message sent via the bot in inline mode, that originated the query.
  , callbackQueryChatInstance    :: Text -- ^ Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent. Useful for high scores in games.
  , callbackQueryData            :: Maybe Text -- ^ Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.
  , callbackQueryGameShortName   :: Maybe Text -- ^ Short name of a Game to be returned, serves as the unique identifier for the game
  }
  deriving (Generic, Show)

newtype CallbackQueryId = CallbackQueryId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ** 'ForceReply'

-- | Upon receiving a message with this object,
-- Telegram clients will display a reply interface to the user
-- (act as if the user has selected the bot‘s message and tapped ’Reply').
-- This can be extremely useful if you want to create user-friendly
-- step-by-step interfaces without having to sacrifice privacy mode.
data ForceReply = ForceReply
  { forceReplyForceReply            :: Bool       -- ^ Shows reply interface to the user, as if they manually selected the bot‘s message and tapped ’Reply'
  , forceReplyInputFieldPlaceholder :: Maybe Text -- ^ The placeholder to be shown in the input field when the reply is active; 1-64 characters.
  , forceReplySelective             :: Maybe Bool -- ^ Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
  }
  deriving (Generic, Show)

-- ** Chat photo

-- | Chat photo. Returned only in getChat.
data ChatPhoto = ChatPhoto
  { chatPhotoSmallFileId       :: FileId -- ^ Unique file identifier of small (160x160) chat photo. This file_id can be used only for photo download.
  , chatPhotoSmallFileUniqueId :: FileId -- ^ Unique file identifier of small (160x160) chat photo, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , chatPhotoBigFileId         :: FileId -- ^ Unique file identifier of big (640x640) chat photo. This file_id can be used only for photo download.
  , chatPhotoBigFileUniqueId   :: FileId -- ^ Unique file identifier of big (640x640) chat photo, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  }
  deriving (Generic, Show)

-- ** 'ChatInviteLink'

-- | Represents an invite link for a chat.
data ChatInviteLink = ChatInviteLink
  { chatInviteLinkInviteLink              :: Text            -- ^ The invite link. If the link was created by another chat administrator, then the second part of the link will be replaced with “…”.
  , chatInviteLinkCreator                 :: User            -- ^ Creator of the link.
  , chatInviteLinkCreatesJoinRequest      :: Bool            -- ^ 'True', if users joining the chat via the link need to be approved by chat administrators.
  , chatInviteLinkIsPrimary               :: Bool            -- ^ 'True', if the link is primary.
  , chatInviteLinkIsRevoked               :: Bool            -- ^ 'True', if the link is revoked.
  , chatInviteLinkName                    :: Maybe Text      -- ^ Invite link name.
  , chatInviteLinkExpireDate              :: Maybe POSIXTime -- ^ Point in time (Unix timestamp) when the link will expire or has been expired.
  , chatInviteLinkMemberLimit             :: Maybe Int     -- ^ Maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999.
  , chatInviteLinkPendingJoinRequestCount :: Maybe Int     -- ^ Number of pending join requests created using this link.
  }
  deriving (Generic, Show)

-- ** 'ChatAdministratorRights'

-- | Represents the rights of an administrator in a chat.
data ChatAdministratorRights = ChatAdministratorRights
  { chatAdministratorRightsIsAnonymous         :: Bool -- ^ 'True', if the user's presence in the chat is hidden.
  , chatAdministratorRightsCanManageChat       :: Bool -- ^ 'True', if the administrator can access the chat event log, chat statistics, message statistics in channels, see channel members, see anonymous administrators in supergroups and ignore slow mode. Implied by any other administrator privilege.
  , chatAdministratorRightsCanDeleteMessages   :: Bool -- ^ 'True', if the administrator can delete messages of other users.
  , chatAdministratorRightsCanManageVideoChats :: Bool -- ^ 'True', if the administrator can manage video chats.
  , chatAdministratorRightsCanRestrictMembers  :: Bool -- ^ 'True', if the administrator can restrict, ban or unban chat members.
  , chatAdministratorRightsCanPromoteMembers   :: Bool -- ^ 'True', if the administrator can add new administrators with a subset of their own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by the user).
  , chatAdministratorRightsCanChangeInfo       :: Bool -- ^ 'True', if the user is allowed to change the chat title, photo and other settings.
  , chatAdministratorRightsCanInviteUsers      :: Bool -- ^ 'True', if the user is allowed to invite new users to the chat.
  , chatAdministratorRightsCanPostMessages     :: Maybe Bool -- ^ 'True', if the administrator can post in the channel; channels only.
  , chatAdministratorRightsCanEditMessages     :: Maybe Bool -- ^ 'True', if the administrator can edit messages of other users and can pin messages; channels only.
  , chatAdministratorRightsCanPinMessages      :: Maybe Bool -- ^ 'True', if the user is allowed to pin messages; groups and supergroups only
  , chatAdministratorRightsCanManageTopics     :: Maybe Bool -- ^ 'True', if the user is allowed to create, rename, close, and reopen forum topics; supergroups only.
  }
  deriving (Generic, Show)

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

-- ** 'ChatMemberUpdated'

-- | This object represents changes in the status of a chat member.
data ChatMemberUpdated = ChatMemberUpdated
  { chatMemberUpdatedChat          :: Chat                 -- ^ Chat the user belongs to.
  , chatMemberUpdatedFrom          :: User                 -- ^ Performer of the action, which resulted in the change.
  , chatMemberUpdatedDate          :: POSIXTime            -- ^ Date the change was done in Unix time.
  , chatMemberUpdatedOldChatMember :: ChatMember           -- ^ Previous information about the chat member.
  , chatMemberUpdatedNewChatMember :: ChatMember           -- ^ New information about the chat member.
  , chatMemberUpdatedInviteLink    :: Maybe ChatInviteLink -- ^ Chat invite link, which was used by the user to join the chat; for joining by invite link events only.
  }
  deriving (Generic, Show)

-- ** 'ChatJoinRequest'

-- | Represents a join request sent to a chat.
data ChatJoinRequest = ChatJoinRequest
  { chatJoinRequestChat       :: Chat                 -- ^ Chat to which the request was sent.
  , chatJoinRequestFrom       :: User                 -- ^ User that sent the join request.
  , chatJoinRequestUserChatId :: ChatId               -- ^ Identifier of a private chat with the user who sent the join request. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier. The bot can use this identifier for 24 hours to send messages until the join request is processed, assuming no other administrator contacted the user.
  , chatJoinRequestDate       :: POSIXTime            -- ^ Date the request was sent in Unix time.
  , chatJoinRequestBio        :: Maybe Text           -- ^ Bio of the user.
  , chatJoinRequestInviteLink :: Maybe ChatInviteLink -- ^ Chat invite link that was used by the user to send the join request.
  }
  deriving (Generic, Show)

-- ** 'ChatPermissions'

-- | Describes actions that a non-administrator user is allowed to take in a chat.
data ChatPermissions = ChatPermissions
  { chatPermissionsCanSendMessages :: Maybe Bool       -- ^ 'True', if the user is allowed to send text messages, contacts, locations and venues.
  , chatPermissionsCanSendAudios     :: Maybe Bool     -- ^ 'True', if the user is allowed to send audios.
  , chatPermissionsCanSendDocuments  :: Maybe Bool     -- ^ 'True', if the user is allowed to send documents.
  , chatPermissionsCanSendPhotos     :: Maybe Bool     -- ^ 'True', if the user is allowed to send photos.
  , chatPermissionsCanSendVideos     :: Maybe Bool     -- ^ 'True', if the user is allowed to send videos.
  , chatPermissionsCanSendVideoNotes :: Maybe Bool     -- ^ 'True', if the user is allowed to send video notes.
  , chatPermissionsCanSendVoiceNotes :: Maybe Bool     -- ^ 'True', if the user is allowed to send voice notes.
  , chatPermissionsCanSendPolls :: Maybe Bool          -- ^ 'True', if the user is allowed to send polls, implies can_send_messages.
  , chatPermissionsCanSendOtherMessages :: Maybe Bool  -- ^ 'True', if the user is allowed to send animations, games, stickers and use inline bots, implies can_send_media_messages.
  , chatPermissionsCanAddWebPagePreviews :: Maybe Bool -- ^ 'True', if the user is allowed to add web page previews to their messages, implies can_send_media_messages.
  , chatPermissionsCanChangeInfo :: Maybe Bool         -- ^ 'True', if the user is allowed to change the chat title, photo and other settings. Ignored in public supergroups
  , chatPermissionsCanInviteUsers :: Maybe Bool        -- ^ 'True', if the user is allowed to invite new users to the chat.
  , chatPermissionsCanPinMessages :: Maybe Bool        -- ^ 'True', if the user is allowed to pin messages. Ignored in public supergroups.
  , chatPermissionsCanManageTopics :: Maybe Bool       -- ^ 'True', if the user is allowed to create forum topics. If omitted defaults to the value of can_pin_messages.
  }
  deriving (Generic, Show)

-- ** 'ChatLocation'

-- | Represents a location to which a chat is connected.
data ChatLocation = ChatLocation
  { chatLocationLocation :: Location -- ^ The location to which the supergroup is connected. Can't be a live location..
  , chatLocationAddress :: Text      -- ^ Location address; 1-64 characters, as defined by the chat owner.
  }
  deriving (Generic, Show)

-- ** 'ResponseParameters'

-- | Contains information about why a request was unsuccessful.
data ResponseParameters = ResponseParameters
  { responseParametersMigrateToChatId :: Maybe ChatId -- ^ The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , responseParametersRetryAfter      :: Maybe Seconds -- ^ In case of exceeding flood control, the number of seconds left to wait before the request can be repeated
  }
  deriving (Show, Generic)

-- * Stickers

-- | The following methods and objects allow your bot to handle stickers and sticker sets.

-- ** 'Sticker'

-- | This object represents a sticker.
data Sticker = Sticker
  { stickerFileId       :: FileId             -- ^ Identifier for this file, which can be used to download or reuse the file.
  , stickerFileUniqueId :: FileId             -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , stickerWidth        :: Int              -- ^ Sticker width.
  , stickerHeight       :: Int              -- ^ Sticker height.
  , stickerIsAnimated   :: Bool               -- ^ 'True', if the sticker is animated.
  , stickerIsVideo      :: Bool               -- ^ 'True', if the sticker is a video sticker.
  , stickerThumb        :: Maybe PhotoSize    -- ^ Sticker thumbnail in the .WEBP or .JPG format.
  , stickerEmoji        :: Maybe Text         -- ^ Emoji associated with the sticker.
  , stickerSetName      :: Maybe Text         -- ^ Name of the sticker set to which the sticker belongs.
  , stickerPremiumAnimation :: Maybe File    -- ^ For premium regular stickers, premium animation for the sticker.
  , stickerMaskPosition :: Maybe MaskPosition -- ^ For mask stickers, the position where the mask should be placed.
  , stickerCustomEmojiId :: Maybe Text        -- ^ For custom emoji stickers, unique identifier of the custom emoji.
  , stickerFileSize     :: Maybe Integer      -- ^ File size in bytes.
  }
  deriving (Generic, Show)

-- ** 'StickerSet'

-- | This object represents a sticker set.
data StickerSet = StickerSet
  { stickerSetName          :: Text            -- ^ Sticker set name.
  , stickerSetTitle         :: Text            -- ^ Sticker set title.
  , stickerSetType          :: StickerSetType  -- ^ Type of stickers in the set, currently one of “regular”, “mask”, “custom_emoji”.
  , stickerSetIsAnimated    :: Bool            -- ^ 'True', if the sticker set contains animated stickers.
  , stickerSetIsVideo       :: Bool            -- ^ 'True', if the sticker is a video sticker.
  , stickerSetContainsMasks :: Maybe Bool      -- ^ True, if the sticker set contains masks.
  , stickerSetStickers      :: [Sticker]       -- ^ List of all set stickers.
  , stickerSetThumb         :: Maybe PhotoSize -- ^ Sticker set thumbnail in the .WEBP or .TGS format.
  }
  deriving (Generic, Show)

-- | Type of stickers in the set, currently one of “regular”, “mask”, “custom_emoji”.
data StickerSetType
  = StickerSetTypeRegular
  | StickerSetTypeMask
  | StickerSetTypeCustomEmoji
  deriving (Eq, Show, Generic)

-- ** 'MaskPosition'

-- | This object describes the position on faces where a mask should be placed by default.
data MaskPosition = MaskPosition
  { maskPositionPoint  :: Text  -- ^ The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
  , maskPositionXShift :: Float -- ^ Shift by X-axis measured in widths of the mask scaled to the face size, from left to right. For example, choosing -1.0 will place mask just to the left of the default mask position.
  , maskPositionYShift :: Float -- ^ Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom. For example, 1.0 will place the mask just below the default mask position.
  , maskPositionScale  :: Float -- ^ Mask scaling coefficient. For example, 2.0 means double size.
  }
  deriving (Generic, Show)

-- * Payments

-- ** 'LabeledPrice'

-- | This object represents a portion of the price for goods or services.
data LabeledPrice = LabelPrice
  { labeledPriceLabel  :: Text  -- ^ Portion label.
  , labeledPriceAmount :: Int -- ^ Price of the product in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  }
  deriving (Generic, Show)

-- ** 'Invoice'

-- | This object contains basic information about an invoice.
data Invoice = Invoice
  { invoiceTitle          :: Text  -- ^ Product name.
  , invoiceDescription    :: Text  -- ^ Product description.
  , invoiceStartParameter :: Text  -- ^ Unique bot deep-linking parameter that can be used to generate this invoice.
  , invoiceCurrency       :: Text  -- ^ Three-letter ISO 4217 currency code.
  , invoiceTotalAmount    :: Int -- ^ Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  }
  deriving (Generic, Show)

-- ** 'ShippingAddress'

-- | This object represents a shipping address.
data ShippingAddress = ShippingAddress
  { shippingAddressCountryCode :: Text -- ^ ISO 3166-1 alpha-2 country code.
  , shippingAddressState       :: Text -- ^ State, if applicable.
  , shippingAddressCity        :: Text -- ^ City.
  , shippingAddressStreetLine1 :: Text -- ^ First line for the address.
  , shippingAddressStreetLine2 :: Text -- ^ Second line for the address.
  , shippingAddressPostCode    :: Text -- ^ Address post code.
  }
  deriving (Generic, Show)

-- ** 'OrderInfo'

-- | This object represents information about an order.
data OrderInfo = OrderInfo
  { orderInfoName            :: Maybe Text            -- ^ User name.
  , orderInfoPhoneNumber     :: Maybe Text            -- ^ User's phone number.
  , orderInfoEmail           :: Maybe Text            -- ^ User email.
  , orderInfoShippingAddress :: Maybe ShippingAddress -- ^ User shipping address.
  }
  deriving (Generic, Show)

-- ** 'ShippingOption'

-- | This object represents one shipping option.
data ShippingOption = ShippingOption
  { shippingOptionId    :: ShippingOptionId -- ^ Shipping option identifier.
  , shippingOptionTitle :: Text             -- ^ Option title.
  , shippingOptionPrice :: [LabeledPrice]   -- ^ List of price portions.
  }
  deriving (Generic, Show)

newtype ShippingOptionId = ShippingOptionId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ** 'SuccessfulPayment'

-- | This object contains basic information about a successful payment.
data SuccessfulPayment = SuccessfulPayment
  { successfulPaymentCurrency                :: Text                   -- ^ Three-letter ISO 4217 currency code.
  , successfulPaymentTotalAmount             :: Int                  -- ^ Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  , successfulPaymentInvoicePayload          :: Text                   -- ^ Bot specified invoice payload.
  , successfulPaymentShippingOptionId        :: Maybe ShippingOptionId -- ^ Identifier of the shipping option chosen by the user.
  , successfulPaymentOrderInfo               :: Maybe OrderInfo        -- ^ Order info provided by the user.
  , successfulPaymentTelegramPaymentChargeId :: Text                   -- ^ Telegram payment identifier.
  , successfulPaymentProviderPaymentChargeId :: Text                   -- ^ Provider payment identifier.
  }
  deriving (Generic, Show)

-- ** 'ShippingQuery'

-- | This object contains information about an incoming shipping query.
data ShippingQuery = ShippingQuery
  { shippingQueryId              :: Text            -- ^ Unique query identifier.
  , shippingQueryFrom            :: User            -- ^ User who sent the query.
  , shippingQueryInvoicePayload  :: Text            -- ^ Bot specified invoice payload.
  , shippingQueryShippingAddress :: ShippingAddress -- ^ User specified shipping address.
  }
  deriving (Generic, Show)

-- ** 'PreCheckoutQuery'

-- | This object contains information about an incoming pre-checkout query.
data PreCheckoutQuery = PreCheckoutQuery
  { preCheckoutQueryId               :: Text                   -- ^ Unique query identifier.
  , preCheckoutQueryFrom             :: User                   -- ^ User who sent the query.
  , preCheckoutQueryCurrency         :: Text                   -- ^ Three-letter ISO 4217 currency code
  , preCheckoutQueryTotalAmount      :: Int                  -- ^ Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  , preCheckoutQueryInvoicePayload   :: Text                   -- ^ Bot specified invoice payload
  , preCheckoutQueryShippingOptionId :: Maybe ShippingOptionId -- ^ Identifier of the shipping option chosen by the user.
  , preCheckoutQueryOrderInfo        :: Maybe OrderInfo        -- ^ Order info provided by the user.
  }
  deriving (Generic, Show)

-- * Telegram Passport

-- | Telegram Passport is a unified authorization method for services that require personal identification. Users can upload their documents once, then instantly share their data with services that require real-world ID (finance, ICOs, etc.). Please see the manual for details.

-- ** 'PassportData'

-- | Contains information about Telegram Passport data shared with the bot by the user.
data PassportData = PassportData
  { passportDataData        :: [EncryptedPassportElement] -- ^ Array with information about documents and other Telegram Passport elements that was shared with the bot.
  , passportDataCredentials :: EncryptedCredentials       -- ^ Encrypted credentials required to decrypt the data.
  }
  deriving (Generic, Show)

-- ** 'PassportFile'

-- | This object represents a file uploaded to Telegram Passport. Currently all Telegram Passport files are in JPEG format when decrypted and don't exceed 10MB.
data PassportFile = PassportFile
  { passportFileFileId       :: FileId    -- ^ Identifier for this file, which can be used to download or reuse the file.
  , passportFileFileUniqueId :: FileId    -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , passportFileFileSize     :: Int     -- ^ File size in bytes.
  , passportFileFileDate     :: POSIXTime -- ^ Unix time when the file was uploaded.
  }
  deriving (Generic, Show)

-- ** 'EncryptedPassportElement'

-- | Contains information about documents or other Telegram Passport elements shared with the bot by the user.
data EncryptedPassportElement = EncryptedPassportElement
  { encryptedPassportElementType        :: PassportElementType  -- ^ One of “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport”, “address”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”, “phone_number”, “email”.
  , encryptedPassportElementData        :: Maybe Text           -- ^ Base64-encoded encrypted Telegram Passport element data provided by the user, available for “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport” and “address” types. Can be decrypted and verified using the accompanying 'EncryptedCredentials'.
  , encryptedPassportElementPhoneNumber :: Maybe Text           -- ^ User's verified phone number, available only for “phone_number” type.
  , encryptedPassportElementEmail       :: Maybe Text           -- ^ User's verified email address, available only for “email” type.
  , encryptedPassportElementFiles       :: Maybe [PassportFile] -- ^ Array of encrypted files with documents provided by the user, available for “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration” and “temporary_registration” types. Files can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementFrontSide   :: Maybe PassportFile   -- ^ Encrypted file with the front side of the document, provided by the user. Available for “passport”, “driver_license”, “identity_card” and “internal_passport”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementReverseSide :: Maybe PassportFile   -- ^ Encrypted file with the reverse side of the document, provided by the user. Available for “driver_license” and “identity_card”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementSelfie      :: Maybe PassportFile   -- ^ Encrypted file with the selfie of the user holding a document, provided by the user; available for “passport”, “driver_license”, “identity_card” and “internal_passport”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementTranslation :: Maybe [PassportFile] -- ^ Array of encrypted files with translated versions of documents provided by the user. Available if requested for “passport”, “driver_license”, “identity_card”, “internal_passport”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration” and “temporary_registration” types. Files can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementHash        :: Text                 -- ^ Base64-encoded element hash for using in 'PassportElementErrorUnspecified'.
  } deriving (Generic, Show)


-- | One of “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport”, “address”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”, “phone_number”, “email”.
data PassportElementType
  = PassportElementTypePersonalDetails
  | PassportElementTypePassport
  | PassportElementTypeDriverLicense
  | PassportElementTypeIdentityCard
  | PassportElementTypeInternalPassport
  | PassportElementTypeAddress
  | PassportElementTypeUtilityBill
  | PassportElementTypeBankStatement
  | PassportElementTypeRentalAgreement
  | PassportElementTypePassportRegistration
  | PassportElementTypeTemporaryRegistration
  | PassportElementTypePhoneNumber
  | PassportElementTypeEmail
  deriving (Generic, Show)

-- ** 'EncryptedCredentials'

-- | Contains data required for decrypting and authenticating EncryptedPassportElement. See the Telegram Passport Documentation for a complete description of the data decryption and authentication processes.
data EncryptedCredentials = EncryptedCredentials
  { encryptedCredentialsData   :: Text -- ^ Base64-encoded encrypted JSON-serialized data with unique user's payload, data hashes and secrets required for EncryptedPassportElement decryption and authentication.
  , encryptedCredentialsHash   :: Text -- ^ Base64-encoded data hash for data authentication.
  , encryptedCredentialsSecret :: Text -- ^ Base64-encoded secret, encrypted with the bot's public RSA key, required for data decryption
  }
  deriving (Generic, Show)

-- ** 'PassportElementError'

data PassportErrorSource
  = PassportErrorSourceData
  | PassportErrorSourceFrontSide
  | PassportErrorSourceReverseSide
  | PassportErrorSourceSelfie
  | PassportErrorSourceFile
  | PassportErrorSourceFiles
  | PassportErrorSourceTranslationFile
  | PassportErrorSourceTranslationFiles
  | PassportErrorSourceUnspecified
  deriving (Generic, Show)

data PassportElementError
  = PassportElementError
    { passportElementErroSource       :: PassportErrorSource -- ^ Error source, must be one of 'PassportErrorSource'.
    , passportElementErrorType        :: PassportElementType -- ^ The section of the user's Telegram Passport which has the error, one of 'PassportElementType'.
    , passportElementErrorName        :: Text                -- ^ Name of the data field which has the error.
    , passportElementErrorHash        :: Maybe Text          -- ^ Base64-encoded data hash.
    , passportElementErrorMessage     :: Text                -- ^ Error message.
    , passportElementErrorFileHash    :: Maybe Text          -- ^ Base64-encoded hash of the file with the reverse side of the document.
    , passportElementErrorFileHashes  :: Maybe [Text]        -- ^ List of base64-encoded file hashes.
    , passportElementErrorElementHash :: Maybe Text          -- ^ Base64-encoded element hash.
    }
    deriving (Generic, Show)

-- * Games

-- | Your bot can offer users HTML5 games to play solo or to compete against each other in groups and one-on-one chats. Create games via @BotFather using the /newgame command. Please note that this kind of power requires responsibility: you will need to accept the terms for each game that your bots will be offering.
-- 
-- Games are a new type of content on Telegram, represented by the Game and InlineQueryResultGame objects.
-- Once you've created a game via BotFather, you can send games to chats as regular messages using the sendGame method, or use inline mode with InlineQueryResultGame.
-- If you send the game message without any buttons, it will automatically have a 'Play GameName' button. When this button is pressed, your bot gets a CallbackQuery with the game_short_name of the requested game. You provide the correct URL for this particular user and the app opens the game in the in-app browser.
-- You can manually add multiple buttons to your game message. Please note that the first button in the first row must always launch the game, using the field callback_game in InlineKeyboardButton. You can add extra buttons according to taste: e.g., for a description of the rules, or to open the game's official community.
-- To make your game more attractive, you can upload a GIF animation that demostrates the game to the users via BotFather (see Lumberjack for example).
-- A game message will also display high scores for the current chat. Use setGameScore to post high scores to the chat with the game, add the edit_message parameter to automatically update the message with the current scoreboard.
-- Use getGameHighScores to get data for in-game high score tables.
-- You can also add an extra sharing button for users to share their best score to different chats.
-- For examples of what can be done using this new stuff, check the @gamebot and @gamee bots.

-- ** 'Game'

-- | This object represents a game. Use BotFather to create and edit games, their short names will act as unique identifiers.
data Game = Game
  { gameTitle        :: Text                  -- ^ Title of the game.
  , gameDescription  :: Text                  -- ^ Description of the game.
  , gamePhoto        :: [PhotoSize]           -- ^ Photo that will be displayed in the game message in chats.
  , gameText         :: Maybe Text            -- ^ Brief description of the game or high scores included in the game message. Can be automatically edited to include current high scores for the game when the bot calls setGameScore, or manually edited using editMessageText. 0-4096 characters.
  , gameTextEntities :: Maybe [MessageEntity] -- ^ Special entities that appear in text, such as usernames, URLs, bot commands, etc.
  , gameAnimation    :: Maybe Animation       -- ^ Animation that will be displayed in the game message in chats. Upload via @BotFather@.
  }
  deriving (Generic, Show)

-- ** 'CallbackGame'

-- | A placeholder, currently holds no information. Use BotFather to set up your game.
newtype CallbackGame = CallbackGame Object
  deriving (Generic, Show)

-- ** 'GameHighScore'

-- | This object represents one row of the high scores table for a game.
data GameHighScore = GameHighScore
  { gameHighScorePosition :: Int -- ^ Position in high score table for the game.
  , gameHighScoreUser     :: User  -- ^ User.
  , gameHighScoreScore    :: Int -- ^ Score.
  }
  deriving (Generic, Show)

-- ** 'CopyMessageId'

-- | This object represents result of copyMessage request.
data CopyMessageId = CopyMessageId
  { copyMessageIdMessageId :: MessageId -- ^ the MessageId of the sent message.
  }
  deriving (Generic, Show)


-- | Unique identifier for the target chat
-- or username of the target channel (in the format @\@channelusername@).
data SomeChatId
  = SomeChatId ChatId       -- ^ Unique chat ID.
  | SomeChatUsername Text   -- ^ Username of the target channel.
  deriving (Generic)

instance ToJSON   SomeChatId where toJSON = genericSomeToJSON
instance FromJSON SomeChatId where parseJSON = genericSomeParseJSON

instance ToHttpApiData SomeChatId where
  toUrlPiece (SomeChatId chatid) = toUrlPiece chatid
  toUrlPiece (SomeChatUsername name) = name
  
-- | This object represents a forum topic.
data ForumTopic = ForumTopic
  { forumTopicMessageThreadId   :: MessageThreadId -- ^ Unique identifier of the forum topic
  , forumTopicName              :: Text            -- ^ Name of the topic
  , forumTopicIconColor         :: Integer         -- ^ Color of the topic icon in RGB format.
  , forumTopicIconCustomEmojiId :: Maybe Text      -- ^ Unique identifier of the custom emoji shown as the topic icon.
  }
  deriving Generic

-- | This object represents a bot command.
data BotCommand = BotCommand
  { botCommandCommand :: Text -- ^ Text of the command; 1-32 characters. Can contain only lowercase English letters, digits and underscores.
  , botCommandDescription :: Text -- ^ Description of the command; 1-256 characters.
  }
  deriving (Generic, Show)

data BotCommandScope
  = BotCommandScopeDefault -- ^ Represents the default scope of bot commands. Default commands are used if no commands with a narrower scope are specified for the user.
  | BotCommandScopeAllPrivateChats -- ^ Represents the scope of bot commands, covering all private chats.
  | BotCommandScopeAllGroupChats -- ^ Represents the scope of bot commands, covering all group and supergroup chats.
  | BotCommandScopeAllChatAdministrators -- ^ Represents the scope of bot commands, covering all group and supergroup chat administrators.
  | BotCommandScopeChat SomeChatId -- ^ Represents the scope of bot commands, covering a specific chat.
  | BotCommandScopeChatAdministrators SomeChatId -- ^ Represents the scope of bot commands, covering all administrators of a specific group or supergroup chat.
  | BotCommandScopeChatMember SomeChatId UserId -- ^ Represents the scope of bot commands, covering a specific member of a group or supergroup chat.

addType :: Text -> [Pair] -> [Pair]
addType name xs = ("type" .= name) : xs

instance ToJSON BotCommandScope where
  toJSON = \case
    BotCommandScopeDefault ->
      object $ addType "default" []
    BotCommandScopeAllPrivateChats ->
      object $ addType "all_private_chats" []
    BotCommandScopeAllGroupChats ->
      object $ addType "all_group_chats" []
    BotCommandScopeAllChatAdministrators ->
      object $ addType "all_chat_administrators" []
    BotCommandScopeChat sci ->
      object $ addType "chat" ["chat_id" .= sci]
    BotCommandScopeChatAdministrators sci ->
      object $ addType "chat_administrators" ["chat_id" .= sci]
    BotCommandScopeChatMember sci ui ->
      object $ addType "chat_member" ["chat_id" .= sci, "user_id" .= ui]

instance FromJSON BotCommandScope where
  parseJSON = withObject "BotCommandScope" \o ->
    (o .: "type" :: Parser Text) >>= \case
    "default" ->                pure BotCommandScopeDefault
    "all_private_chats" ->      pure BotCommandScopeAllPrivateChats
    "all_group_chats" ->        pure BotCommandScopeAllGroupChats
    "all_chat_administrators"-> pure BotCommandScopeAllChatAdministrators
    "chat" ->                        BotCommandScopeChat <$> o .: "chat_id"
    "chat_administrators"->          BotCommandScopeChatAdministrators <$> o .: "chat_id"
    "chat_member"->                  BotCommandScopeChatMember <$> o .: "chat_id" <*> o .: "user_id"
    t -> fail $ Text.unpack ("Unknown type: " <> t)


-- | Generic fields for all InputMedia structures
data InputMediaGeneric = InputMediaGeneric
  { inputMediaGenericMedia :: InputFile -- ^ File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name.
  , inputMediaGenericCaption :: Maybe Text -- ^ Caption of the photo to be sent, 0-1024 characters after entities parsing.
  , inputMediaGenericParseMode :: Maybe Text -- ^ Mode for parsing entities in the photo caption. See formatting options <https:\/\/core.telegram.org\/bots\/api#formatting-options> for more details.
  , inputMediaGenericCaptionEntities :: Maybe [MessageEntity] -- ^ List of special entities that appear in the caption, which can be specified instead of parse_mode.
  }
  deriving Generic

data InputMediaGenericThumb = InputMediaGenericThumb
  { inputMediaGenericGeneric :: InputMediaGeneric
  , inputMediaGenericThumb :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. 
  }

data InputMedia
  = InputMediaPhoto -- ^ Represents a photo to be sent.
    { inputMediaPhotoGeneric :: InputMediaGeneric
    , inputMediaPhotoHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the video needs to be covered with a spoiler animation.
    }
  | InputMediaVideo -- ^ Represents a video to be sent.
    { inputMediaVideoGeneric :: InputMediaGenericThumb
    , inputMediaVideoWidth :: Maybe Integer -- ^ Video width
    , inputMediaVideoHeight :: Maybe Integer -- ^ Video height
    , inputMediaVideoDuration :: Maybe Integer -- ^ Video duration in seconds
    , inputMediaVideoSupportsStreaming :: Maybe Bool -- ^ Pass 'True', if the uploaded video is suitable for streaming.
    , inputMediaVideoHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the video needs to be covered with a spoiler animation.
    }
  | InputMediaAnimation -- ^ Represents an animation file (GIF or H.264/MPEG-4 AVC video without sound) to be sent.
    { inputMediaAnimationGeneric :: InputMediaGenericThumb
    , inputMediaAnimationWidth :: Maybe Integer -- ^ Animation width
    , inputMediaAnimationHeight :: Maybe Integer -- ^ Animation height
    , inputMediaAnimationDuration :: Maybe Integer -- ^ Animation duration in seconds
    , inputMediaAnimationHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the video needs to be covered with a spoiler animation.
    }
  | InputMediaAudio -- ^ Represents an audio file to be treated as music to be sent.
    { inputMediaAudioGeneric :: InputMediaGenericThumb
    , inputMediaAudioDuration :: Maybe Integer -- ^ Duration of the audio in seconds
    , inputMediaAudioPerformer :: Maybe Text -- ^ Performer of the audio
    , inputMediaAudioTitle :: Maybe Text -- ^ Title of the audio
    }
  | InputMediaDocument -- ^ Represents a general file to be sent.
    { inputMediaDocumentGeneric :: InputMediaGenericThumb
    , inputMediaDocumentDisableContentTypeDetection :: Maybe Bool -- ^ Disables automatic server-side content type detection for files uploaded using multipart/form-data. Always True, if the document is sent as part of an album.
    }

foldMap deriveJSON'
  [ ''User
  , ''Chat
  , ''Message
  , ''MessageEntityType
  , ''MessageEntity
  , ''PhotoSize
  , ''Audio
  , ''Document
  , ''StickerSetType
  , ''Sticker
  , ''Video
  , ''Voice
  , ''VideoNote
  , ''Contact
  , ''Location
  , ''Venue
  , ''UserProfilePhotos
  , ''File
  , ''ReplyKeyboardMarkup
  , ''KeyboardButton
  , ''KeyboardButtonRequestUser
  , ''KeyboardButtonRequestChat
  , ''ReplyKeyboardRemove
  , ''InlineKeyboardMarkup
  , ''InlineKeyboardButton
  , ''CallbackQuery
  , ''ForceReply
  , ''ChatPhoto
  , ''ChatMember
  , ''ChatMemberUpdated
  , ''ResponseParameters
  , ''MaskPosition
  , ''CallbackGame
  , ''Animation
  , ''Dice
  , ''Game
  , ''Poll
  , ''PollAnswer
  , ''ChatJoinRequest
  , ''PollOption
  , ''MessageAutoDeleteTimerChanged
  , ''ForumTopicCreated
  , ''ForumTopicEdited
  , ''ForumTopicClosed
  , ''ForumTopicReopened
  , ''GeneralForumTopicHidden
  , ''GeneralForumTopicUnhidden
  , ''UserShared
  , ''ChatShared
  , ''WriteAccessAllowed
  , ''Invoice
  , ''SuccessfulPayment
  , ''OrderInfo
  , ''ShippingAddress
  , ''PassportData
  , ''EncryptedPassportElement
  , ''PassportElementType
  , ''PassportFile
  , ''PassportElementError
  , ''PassportErrorSource
  , ''EncryptedCredentials
  , ''ProximityAlertTriggered
  , ''VideoChatScheduled
  , ''VideoChatEnded
  , ''VideoChatParticipantsInvited
  , ''ChatPermissions
  , ''ChatLocation
  , ''StickerSet
  , ''BotCommand
  , ''ForumTopic
  , ''ChatInviteLink
  , ''LabeledPrice
  , ''ShippingOption
  , ''ShippingQuery
  , ''PreCheckoutQuery
  , ''WebAppData
  , ''WebAppInfo
  , ''ChatAdministratorRights
  , ''CopyMessageId
  ]

instance ToJSON InputMediaGeneric where toJSON = gtoJSON

instance ToHttpApiData PassportElementError where
  toUrlPiece = TL.toStrict . encodeToLazyText

instance ToHttpApiData [PassportElementError] where
  toUrlPiece = TL.toStrict . encodeToLazyText

instance ToMultipart Tmp InputMediaGeneric where
  toMultipart InputMediaGeneric{..} = makeFile "media" inputMediaGenericMedia (MultipartData fields []) where
    fields = catMaybes
      [ inputMediaGenericCaption <&>
        \t -> Input "caption" t
      , inputMediaGenericParseMode <&>
        \t -> Input "parse_mode" t
      , inputMediaGenericCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
      ]

instance ToJSON InputMediaGenericThumb where
  toJSON InputMediaGenericThumb{..}
    = addJsonFields (toJSON inputMediaGenericGeneric)
      ["thumb" .= inputMediaGenericThumb]

instance ToMultipart Tmp InputMediaGenericThumb where
  toMultipart = \case
    InputMediaGenericThumb generic Nothing -> toMultipart generic
    InputMediaGenericThumb generic (Just thumb) -> makeFile "thumb" thumb (toMultipart generic)

instance ToJSON InputMedia where
  toJSON = \case
    InputMediaPhoto img spoiler ->
      addJsonFields (toJSON img) (addType "photo" [ "has_spoiler" .= spoiler])
    InputMediaVideo imgt width height duration streaming spoiler ->
      addJsonFields (toJSON imgt)
                (addType "video"
                [ "width" .= width
                , "height" .= height
                , "duration" .= duration
                , "support_streaming" .= streaming
                , "has_spoiler" .= spoiler
                ])
    InputMediaAnimation imgt width height duration spoiler ->
      addJsonFields (toJSON imgt)
                (addType "animation"
                [ "width" .= width
                , "height" .= height
                , "duration" .= duration
                , "has_spoiler" .= spoiler
                ])
    InputMediaAudio imgt duration performer title ->
      addJsonFields (toJSON imgt)
                (addType "audio"
                [ "duration" .= duration
                , "performer" .= performer
                , "title" .= title
                ])
    InputMediaDocument imgt dctd ->
      addJsonFields (toJSON imgt)
                (addType "document" ["disable_content_type_detection" .= dctd])



instance ToMultipart Tmp InputMedia where
  toMultipart = let
    in \case
    InputMediaPhoto img spoiler ->
      addMultipartFields
      (Input "type" "photo"
       : catMaybes
        [ spoiler <&>
          \t -> Input "has_spoiler" (bool "false" "true" t)
        ]) (toMultipart img)
    InputMediaVideo imgt width height duration streaming spoiler ->
      addMultipartFields
      (Input "type" "video"
      : catMaybes 
      [ width <&>
        \t -> Input "width" (TL.toStrict $ encodeToLazyText t)
      , height <&>
        \t -> Input "height" (TL.toStrict $ encodeToLazyText t)
      , duration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , streaming <&>
        \t -> Input "support_streaming" (bool "false" "true" t)
      , spoiler <&>
        \t -> Input "has_spoiler" (bool "false" "true" t)
      ]) (toMultipart imgt)
    InputMediaAnimation imgt width height duration spoiler ->
      addMultipartFields
      (Input "type" "animation"
      : catMaybes 
      [ width <&>
        \t -> Input "width" (TL.toStrict $ encodeToLazyText t)
      , height <&>
        \t -> Input "height" (TL.toStrict $ encodeToLazyText t)
      , duration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , spoiler <&>
        \t -> Input "has_spoiler" (bool "false" "true" t)
      ]) (toMultipart imgt)
    InputMediaAudio imgt duration performer title ->
      addMultipartFields
      (Input "type" "audio"
      : catMaybes 
      [ duration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , performer <&>
        \t -> Input "performer" t
      , title <&>
        \t -> Input "title" t
      ]) (toMultipart imgt)
    InputMediaDocument imgt dctd ->
      addMultipartFields
      (Input "type" "document"
      : catMaybes 
      [ dctd <&> 
         \t -> Input "disable_content_type_detection" (bool "false" "true" t)
      ]) (toMultipart imgt)

-- | Multipart file helper
makeFile :: Text -> InputFile ->  MultipartData Tmp ->  MultipartData Tmp
makeFile name (InputFile path ct) (MultipartData fields files) = 
  MultipartData 
    (Input name ("attach://" <> name) : fields) 
    (FileData name (pack $ takeFileName path) ct path : files)

makeFile name file (MultipartData fields files) = 
  MultipartData 
    (Input name (TL.toStrict $ encodeToLazyText file) : fields) 
    files

instance ToJSON MenuButton where
  toJSON = \case
    MenuButtonCommands ->
      object $ addType "commands" []
    MenuButtonWebApp txt wai ->
      object $ addType "web_app" ["text" .= txt, "web_app_info" .= wai]
    MenuButtonDefault ->
      object $ addType "default" []

instance FromJSON MenuButton where
  parseJSON = gparseJSON
