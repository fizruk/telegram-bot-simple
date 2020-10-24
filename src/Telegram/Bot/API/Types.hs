{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Telegram.Bot.API.Types where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Hashable (Hashable)
import Data.String
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Servant.API

import Telegram.Bot.API.Internal.Utils

type RequiredQueryParam = QueryParam' '[Required, Strict]

newtype Seconds = Seconds Int32
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
  , userLastName     :: Maybe Text -- ^ User‘s or bot’s last name
  , userUsername     :: Maybe Text -- ^ User‘s or bot’s username
  , userLanguageCode :: Maybe Text -- ^ IETF language tag of the user's language
  } deriving (Show, Generic)

-- | Unique identifier for this user or bot.
newtype UserId = UserId Int32
  deriving (Eq, Show, ToJSON, FromJSON)

instance ToHttpApiData UserId where toUrlPiece = pack . show @Int32 . coerce

-- ** Chat

-- | This object represents a chat.
--
-- <https://core.telegram.org/bots/api#chat>
data Chat = Chat
  { chatId                           :: ChatId          -- ^ Unique identifier for this chat. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , chatType                         :: ChatType        -- ^ Type of chat.
  , chatTitle                        :: Maybe Text      -- ^ Title, for supergroups, channels and group chats
  , chatUsername                     :: Maybe Text      -- ^ Username, for private chats, supergroups and channels if available
  , chatFirstName                    :: Maybe Text      -- ^ First name of the other party in a private chat
  , chatLastName                     :: Maybe Text      -- ^ Last name of the other party in a private chat
  , chatAllMembersAreAdministrators  :: Maybe Bool      -- ^ 'True' if a group has ‘All Members Are Admins’ enabled.
  , chatPhoto                        :: Maybe ChatPhoto -- ^ Chat photo. Returned only in getChat.
  , chatDescription                  :: Maybe Text      -- ^ Description, for supergroups and channel chats. Returned only in getChat.
  , chatInviteLink                   :: Maybe Text      -- ^ Chat invite link, for supergroups and channel chats. Returned only in getChat.
  , chatPinnedMessage                :: Maybe Message   -- ^ Pinned message, for supergroups. Returned only in getChat.
  , chatStickerSetName               :: Maybe Text      -- ^ For supergroups, name of group sticker set. Returned only in getChat.
  , chatCanSetStickerSet             :: Maybe Bool      -- ^ True, if the bot can change the group sticker set. Returned only in getChat.
  } deriving (Generic, Show)

-- | Unique identifier for this chat.
newtype ChatId = ChatId Integer
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData ChatId where toUrlPiece a = pack . show @Integer $ coerce a

-- | Type of chat.
data ChatType
  = ChatTypePrivate
  | ChatTypeGroup
  | ChatTypeSupergroup
  | ChatTypeChannel
  deriving (Generic, Show)

instance ToJSON   ChatType where toJSON = gtoJSON
instance FromJSON ChatType where parseJSON = gparseJSON

-- ** Message

-- | This object represents a message.
data Message = Message
  { messageMessageId :: MessageId -- ^ Unique message identifier inside this chat
  , messageFrom :: Maybe User -- ^ Sender, empty for messages sent to channels
  , messageDate :: POSIXTime -- ^ Date the message was sent in Unix time
  , messageChat :: Chat -- ^ Conversation the message belongs to
  , messageForwardFrom :: Maybe User -- ^ For forwarded messages, sender of the original message
  , messageForwardFromChat :: Maybe Chat -- ^ For messages forwarded from channels, information about the original channel
  , messageForwardFromMessageId :: Maybe MessageId -- ^ For messages forwarded from channels, identifier of the original message in the channel
  , messageForwardSignature :: Maybe Text -- ^ For messages forwarded from channels, signature of the post author if present
  , messageForwardDate :: Maybe POSIXTime -- ^ For forwarded messages, date the original message was sent in Unix time
  , messageReplyToMessage :: Maybe Message -- ^ For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
  , messageEditDate :: Maybe POSIXTime -- ^ Date the message was last edited in Unix time
  , messageMediaGroupId :: Maybe MediaGroupId -- ^ The unique identifier of a media message group this message belongs to
  , messageAuthorSignature :: Maybe Text -- ^ Signature of the post author for messages in channels
  , messageText :: Maybe Text -- ^ For text messages, the actual UTF-8 text of the message, 0-4096 characters.
  , messageEntities :: Maybe [MessageEntity] -- ^ For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
  , messageCaptionEntities :: Maybe [MessageEntity] -- ^ For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
  , messageAudio :: Maybe Audio -- ^ Message is an audio file, information about the file
  , messageDocument :: Maybe Document -- ^ Message is a general file, information about the file

--  , messageGame :: Maybe Game -- ^ Message is a game, information about the game. More about games »

  , messagePhoto :: Maybe [PhotoSize] -- ^ Message is a photo, available sizes of the photo

--  , messageSticker :: Maybe Sticker -- ^ Message is a sticker, information about the sticker

  , messageVideo :: Maybe Video -- ^ Message is a video, information about the video
  , messageVoice :: Maybe Voice -- ^ Message is a voice message, information about the file
  , messageVideoNote :: Maybe VideoNote -- ^ Message is a video note, information about the video message
  , messageCaption :: Maybe Text -- ^ Caption for the audio, document, photo, video or voice, 0-200 characters
  , messageContact :: Maybe Contact -- ^ Message is a shared contact, information about the contact
  , messageLocation :: Maybe Location -- ^ Message is a shared location, information about the location
  , messageVenue :: Maybe Venue -- ^ Message is a venue, information about the venue
  , messageNewChatMembers :: Maybe [User] -- ^ New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
  , messageLeftChatMember :: Maybe User -- ^ A member was removed from the group, information about them (this member may be the bot itself)
  , messageNewChatTitle :: Maybe Text -- ^ A chat title was changed to this value
  , messageNewChatPhoto :: Maybe [PhotoSize] -- ^ A chat photo was change to this value
  , messageDeleteChatPhoto :: Maybe Bool -- ^ Service message: the chat photo was deleted
  , messageGroupChatCreated :: Maybe Bool -- ^ Service message: the group has been created
  , messageSupergroupChatCreated :: Maybe Bool -- ^ Service message: the supergroup has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a supergroup when it is created. It can only be found in reply_to_message if someone replies to a very first message in a directly created supergroup.
  , messageChannelChatCreated :: Maybe Bool -- ^ Service message: the channel has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a channel when it is created. It can only be found in reply_to_message if someone replies to a very first message in a channel.
  , messageMigrateToChatId :: Maybe ChatId -- ^ The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , messageMigrateFromChatId :: Maybe ChatId -- ^ The supergroup has been migrated from a group with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , messagePinnedMessage :: Maybe Message -- ^ Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply.

--  , messageInvoice :: Maybe Invoice -- ^ Message is an invoice for a payment, information about the invoice. More about payments »
--  , messageSuccessfulPayment :: Maybe SuccessfulPayment -- ^ Message is a service message about a successful payment, information about the payment. More about payments »
  } deriving (Generic, Show)

-- | Unique message identifier inside this chat.
newtype MessageId = MessageId Int32
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData MessageId where toUrlPiece a = pack . show @Int32 $ coerce a

-- | The unique identifier of a media message group a message belongs to.
newtype MediaGroupId = MediaGroupId Text
  deriving (Eq, Show, ToJSON, FromJSON)

-- ** MessageEntity

-- | This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
  { messageEntityType :: MessageEntityType -- ^ Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), underline (underlined text), strikethrough, code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
  , messageEntityOffset :: Int32 -- ^ Offset in UTF-16 code units to the start of the entity
  , messageEntityLength :: Int32 -- ^ Length of the entity in UTF-16 code units
  , messageEntityUrl :: Maybe Text -- ^ For “text_link” only, url that will be opened after user taps on the text
  , messageEntityUser :: Maybe User -- ^ For “text_mention” only, the mentioned user
  } deriving (Generic, Show)

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
  deriving (Eq, Show, Generic)

instance ToJSON   MessageEntityType where toJSON = gtoJSON
instance FromJSON MessageEntityType where parseJSON = gparseJSON

-- ** 'PhotoSize'

-- | This object represents one size of a photo or a file / sticker thumbnail.
data PhotoSize = PhotoSize
  { photoSizeFileId   :: FileId      -- ^ Unique identifier for this file
  , photoSizeWidth    :: Int32       -- ^ Photo width
  , photoSizeHeight   :: Int32       -- ^ Photo height
  , photoSizeFileSize :: Maybe Int32 -- ^ File size
  } deriving (Generic, Show)

-- | Unique identifier for this file.
newtype FileId = FileId Text
  deriving (Eq, Show, ToJSON, FromJSON)

-- ** 'Audio'

-- | This object represents an audio file to be treated as music by the Telegram clients.
data Audio = Audio
  { audioFileId :: FileId -- ^ Unique identifier for this file
  , audioDuration :: Seconds -- ^ Duration of the audio in seconds as defined by sender
  , audioPerformer :: Maybe Text -- ^ Performer of the audio as defined by sender or by audio tags
  , audioTitle :: Maybe Text -- ^ Title of the audio as defined by sender or by audio tags
  , audioMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender
  , audioFileSize :: Maybe Int32 -- ^ File size
  } deriving (Generic, Show)

-- ** 'Document'

-- | This object represents a general file (as opposed to photos, voice messages and audio files).
data Document = Document
  { documentFileId :: FileId -- ^ Unique file identifier
  , documentThumb :: Maybe PhotoSize -- ^ Document thumbnail as defined by sender
  , documentFileName :: Maybe Text -- ^ Original filename as defined by sender
  , documentMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender
  , documentFileSize :: Maybe Int32 -- ^ File size
  } deriving (Generic, Show)

-- ** 'Video'

-- | This object represents a video file.
data Video = Video
  { videoFileId :: FileId -- ^ Unique identifier for this file
  , videoWidth :: Int32 -- ^ Video width as defined by sender
  , videoHeight :: Int32 -- ^ Video height as defined by sender
  , videoDuration :: Seconds -- ^ Duration of the video in seconds as defined by sender
  , videoThumb :: Maybe PhotoSize -- ^ Video thumbnail
  , videoMimeType :: Maybe Text -- ^ Mime type of a file as defined by sender
  , videoFileSize :: Maybe Int32 -- ^ File size
  } deriving (Generic, Show)

-- ** 'Voice'

-- | This object represents a voice note.
data Voice = Voice
  { voiceFileId :: FileId -- ^ Unique identifier for this file
  , voiceDuration :: Seconds -- ^ Duration of the audio in seconds as defined by sender
  , voiceMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender
  , voiceFileSize :: Maybe Int32 -- ^ File size
  } deriving (Generic, Show)

-- ** 'VideoNote'

-- | This object represents a video message (available in Telegram apps as of v.4.0).
data VideoNote = VideoNote
  { videoNoteFileId :: Text -- ^ Unique identifier for this file
  , videoNoteLength :: Int32 -- ^ Video width and height as defined by sender
  , videoNoteDuration :: Seconds -- ^ Duration of the video in seconds as defined by sender
  , videoNoteThumb :: Maybe PhotoSize -- ^ Video thumbnail
  , videoNoteFileSize :: Maybe Int32 -- ^ File size
  } deriving (Generic, Show)

-- ** 'Contact'

-- | This object represents a phone contact.
data Contact = Contact
  { contactPhoneNumber :: Text -- ^ Contact's phone number
  , contactFirstName :: Text -- ^ Contact's first name
  , contactLastName :: Maybe Text -- ^ Contact's last name
  , contactUserId :: Maybe UserId -- ^ Contact's user identifier in Telegram
  } deriving (Generic, Show)

-- ** Location

-- | This object represents a point on the map.
data Location = Location
  { locationLongitude :: Float -- ^ Longitude as defined by sender
  , locationLatitude  :: Float -- ^ Latitude as defined by sender
  } deriving (Generic, Show)

-- ** 'Venue'

-- | This object represents a venue.
data Venue = Venue
  { venueLocation :: Location -- ^ Venue location
  , venueTitle :: Text -- ^ Name of the venue
  , venueAddress :: Text -- ^ Address of the venue
  , venueFoursquareId :: Maybe Text -- ^ Foursquare identifier of the venue
  } deriving (Generic, Show)

-- ** 'UserProfilePhotos'

-- | This object represent a user's profile pictures.
data UserProfilePhotos = UserProfilePhotos
  { userProfilePhotosTotalCount :: Int32 -- ^ Total number of profile pictures the target user has
  , userProfilePhotosPhotos :: [[PhotoSize]] -- ^ Requested profile pictures (in up to 4 sizes each)
  } deriving (Generic, Show)

-- ** 'File'

-- | This object represents a file ready to be downloaded.
-- The file can be downloaded via the link @https://api.telegram.org/file/bot<token>/<file_path>@.
-- It is guaranteed that the link will be valid for at least 1 hour.
-- When the link expires, a new one can be requested by calling getFile.
data File = File
  { fileFileId :: FileId -- ^ Unique identifier for this file
  , fileFileSize :: Maybe Int32 -- ^ File size, if known
  , fileFilePath :: Maybe Text -- ^ File path. Use https://api.telegram.org/file/bot<token>/<file_path> to get the file.
  } deriving (Generic, Show)

-- ** 'ReplyKeyboardMarkup'

-- | This object represents a custom keyboard with reply options (see Introduction to bots for details and examples).
data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { replyKeyboardMarkupKeyboard :: [[KeyboardButton]] -- ^ Array of button rows, each represented by an Array of KeyboardButton objects
  , replyKeyboardMarkupResizeKeyboard :: Maybe Bool -- ^ Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to false, in which case the custom keyboard is always of the same height as the app's standard keyboard.
  , replyKeyboardMarkupOneTimeKeyboard :: Maybe Bool -- ^ Requests clients to hide the keyboard as soon as it's been used. The keyboard will still be available, but clients will automatically display the usual letter-keyboard in the chat – the user can press a special button in the input field to see the custom keyboard again. Defaults to false.
  , replyKeyboardMarkupSelective :: Maybe Bool -- ^ Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
  } deriving (Generic, Show)

-- ** 'KeyboardButton'

-- | This object represents one button of the reply keyboard.
-- For simple text buttons String can be used instead of this object
-- to specify text of the button. Optional fields are mutually exclusive.
data KeyboardButton = KeyboardButton
  { keyboardButtonText :: Text -- ^ Text of the button. If none of the optional fields are used, it will be sent as a message when the button is pressed
  , keyboardButtonRequestContact :: Maybe Bool -- ^ If True, the user's phone number will be sent as a contact when the button is pressed. Available in private chats only
  , keyboardButtonRequestLocation :: Maybe Bool -- ^ If True, the user's current location will be sent when the button is pressed. Available in private chats only
  } deriving (Generic, Show)

instance IsString KeyboardButton where
  fromString s = KeyboardButton (fromString s) Nothing Nothing

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
  , replyKeyboardRemoveSelective :: Maybe Bool -- ^ Use this parameter if you want to remove the keyboard for specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
  } deriving (Generic, Show)

-- ** 'InlineKeyboardMarkup'

-- | This object represents an inline keyboard that appears
-- right next to the message it belongs to.
data InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboardMarkupInlineKeyboard :: [[InlineKeyboardButton]] -- ^ Array of button rows, each represented by an Array of InlineKeyboardButton objects
  } deriving (Generic, Show)

-- ** 'InlineKeyboardButton'

-- | This object represents one button of an inline keyboard. You must use exactly one of the optional fields.
data InlineKeyboardButton = InlineKeyboardButton
  { inlineKeyboardButtonText :: Text -- ^ Label text on the button
  , inlineKeyboardButtonUrl :: Maybe Text -- ^ HTTP url to be opened when button is pressed
  , inlineKeyboardButtonCallbackData :: Maybe Text -- ^ Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes
  , inlineKeyboardButtonSwitchInlineQuery :: Maybe Text -- ^ If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot‘s username and the specified inline query in the input field. Can be empty, in which case just the bot’s username will be inserted.
  , inlineKeyboardButtonSwitchInlineQueryCurrentChat :: Maybe Text -- ^ If set, pressing the button will insert the bot‘s username and the specified inline query in the current chat's input field. Can be empty, in which case only the bot’s username will be inserted.

--  , inlineKeyboardButtonCallbackGame :: Maybe CallbackGame -- ^ Description of the game that will be launched when the user presses the button.

  , inlineKeyboardButtonPay :: Maybe Bool -- ^ Specify True, to send a Pay button.
  } deriving (Generic, Show)

labeledInlineKeyboardButton :: Text -> InlineKeyboardButton
labeledInlineKeyboardButton label = InlineKeyboardButton label Nothing Nothing Nothing Nothing Nothing

-- ** 'CallbackQuery'

-- | This object represents an incoming callback query from a callback button
-- in an inline keyboard. If the button that originated the query was attached
-- to a message sent by the bot, the field message will be present.
-- If the button was attached to a message sent via the bot (in inline mode),
-- the field @inline_message_id@ will be present.
-- Exactly one of the fields data or game_short_name will be present.
data CallbackQuery = CallbackQuery
  { callbackQueryId :: CallbackQueryId -- ^ Unique identifier for this query
  , callbackQueryFrom :: User -- ^ Sender
  , callbackQueryMessage :: Maybe Message -- ^ Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old
  , callbackQueryInlineMessageId :: Maybe MessageId -- ^ Identifier of the message sent via the bot in inline mode, that originated the query.
  , callbackQueryChatInstance :: Text -- ^ Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent. Useful for high scores in games.
  , callbackQueryData :: Maybe Text -- ^ Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.
  , callbackQueryGameShortName :: Maybe Text -- ^ Short name of a Game to be returned, serves as the unique identifier for the game
  } deriving (Generic, Show)

newtype CallbackQueryId = CallbackQueryId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ** 'ForceReply'

-- | Upon receiving a message with this object,
-- Telegram clients will display a reply interface to the user
-- (act as if the user has selected the bot‘s message and tapped ’Reply').
-- This can be extremely useful if you want to create user-friendly
-- step-by-step interfaces without having to sacrifice privacy mode.
data ForceReply = ForceReply
  { forceReplyForceReply :: Bool -- ^ Shows reply interface to the user, as if they manually selected the bot‘s message and tapped ’Reply'
  , forceReplySelective :: Maybe Bool -- ^ Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
  } deriving (Generic, Show)

-- ** Chat photo

-- | Chat photo. Returned only in getChat.
data ChatPhoto = ChatPhoto
  { chatPhotoSmallFileId :: FileId -- ^ Unique file identifier of small (160x160) chat photo. This file_id can be used only for photo download.
  , chatPhotoBigFileId   :: FileId -- ^ Unique file identifier of big (640x640) chat photo. This file_id can be used only for photo download.
  } deriving (Generic, Show)

-- ** 'ChatMember'

-- | This object contains information about one member of a chat.
data ChatMember = ChatMember
  { chatMemberUser :: User -- ^ Information about the user
  , chatMemberStatus :: Text -- ^ The member's status in the chat. Can be “creator”, “administrator”, “member”, “restricted”, “left” or “kicked”
  , chatMemberUntilDate :: Maybe POSIXTime -- ^ Restictred and kicked only. Date when restrictions will be lifted for this user, unix time
  , chatMemberCanBeEdited :: Maybe Bool -- ^ Administrators only. True, if the bot is allowed to edit administrator privileges of that user
  , chatMemberCanChangeInfo :: Maybe Bool -- ^ Administrators only. True, if the administrator can change the chat title, photo and other settings
  , chatMemberCanPostMessages :: Maybe Bool -- ^ Administrators only. True, if the administrator can post in the channel, channels only
  , chatMemberCanEditMessages :: Maybe Bool -- ^ Administrators only. True, if the administrator can edit messages of other users and can pin messages, channels only
  , chatMemberCanDeleteMessages :: Maybe Bool -- ^ Administrators only. True, if the administrator can delete messages of other users
  , chatMemberCanInviteUsers :: Maybe Bool -- ^ Administrators only. True, if the administrator can invite new users to the chat
  , chatMemberCanRestrictMembers :: Maybe Bool -- ^ Administrators only. True, if the administrator can restrict, ban or unban chat members
  , chatMemberCanPinMessages :: Maybe Bool -- ^ Administrators only. True, if the administrator can pin messages, supergroups only
  , chatMemberCanPromoteMembers :: Maybe Bool -- ^ Administrators only. True, if the administrator can add new administrators with a subset of his own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by the user)
  , chatMemberCanSendMessages :: Maybe Bool -- ^ Restricted only. True, if the user can send text messages, contacts, locations and venues
  , chatMemberCanSendMediaMessages :: Maybe Bool -- ^ Restricted only. True, if the user can send audios, documents, photos, videos, video notes and voice notes, implies can_send_messages
  , chatMemberCanSendOtherMessages :: Maybe Bool -- ^ Restricted only. True, if the user can send animations, games, stickers and use inline bots, implies can_send_media_messages
  , chatMemberCanAddWebPagePreviews :: Maybe Bool -- ^ Restricted only. True, if user may add web page previews to his messages, implies can_send_media_messages
  } deriving (Generic, Show)

-- ** 'ResponseParameters'

-- | Contains information about why a request was unsuccessful.
data ResponseParameters = ResponseParameters
  { responseParametersMigrateToChatId :: Maybe ChatId -- ^ The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , responseParametersRetryAfter :: Maybe Seconds -- ^ In case of exceeding flood control, the number of seconds left to wait before the request can be repeated
  } deriving (Show, Generic)

deriveJSON' ''User
deriveJSON' ''Chat
deriveJSON' ''Message
deriveJSON' ''MessageEntity
deriveJSON' ''PhotoSize
deriveJSON' ''Audio
deriveJSON' ''Document
deriveJSON' ''Video
deriveJSON' ''Voice
deriveJSON' ''VideoNote
deriveJSON' ''Contact
deriveJSON' ''Location
deriveJSON' ''Venue
deriveJSON' ''UserProfilePhotos
deriveJSON' ''File
deriveJSON' ''ReplyKeyboardMarkup
deriveJSON' ''KeyboardButton
deriveJSON' ''ReplyKeyboardRemove
deriveJSON' ''InlineKeyboardMarkup
deriveJSON' ''InlineKeyboardButton
deriveJSON' ''CallbackQuery
deriveJSON' ''ForceReply
deriveJSON' ''ChatPhoto
deriveJSON' ''ChatMember
deriveJSON' ''ResponseParameters
