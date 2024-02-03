{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE LambdaCase #-}

module Telegram.Bot.API.Stickers where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Text
import Data.Bool
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)
import Servant.Multipart.API
import Servant.Multipart.Client

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests (Response)
import Telegram.Bot.API.Types
import Data.Maybe (catMaybes)
import Data.Functor
import Telegram.Bot.API.Internal.TH (makeDefault)


-- | Type of uploaded sticker file. Static or animated.
data StickerType
  = PngSticker -- ^ PNG image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data.
  | TgsSticker -- ^ TGS animation with the sticker, uploaded using multipart/form-data. See <https:\/\/core.telegram.org\/animated_stickers#technical-requirements> for technical requirements.
  | WebmSticker -- ^ WEBM video with the sticker, uploaded using multipart/form-data. See <https:\/\/core.telegram.org\/stickers#video-sticker-requirements> for technical requirements.


-- | Sticker file with static/animated label.
data StickerFile = StickerFile {stickerFileSticker :: InputFile, stickerFileLabel :: StickerType}

-- ** 'sendSticker'

-- | Request parameters for 'sendSticker'.
data SendStickerRequest = SendStickerRequest
  { sendStickerChatId                   :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendStickerMessageThreadId          :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendStickerEmoji                    :: Maybe Text -- ^ Emoji associated with the sticker; only for just uploaded stickers.
  , sendStickerSticker                  :: InputFile -- ^ Sticker to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a .WEBP file from the Internet, or upload a new one using multipart/form-data.
  , sendStickerDisableNotification      :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendStickerProtectContent           :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendStickerReplyToMessageId         :: Maybe MessageId -- ^	If the message is a reply, ID of the original message
  , sendStickerReplyParameters          :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendStickerReplyMarkup              :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendStickerRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendStickerRequest where
  toMultipart SendStickerRequest{..} =
    makeFile "sticker" sendStickerSticker (MultipartData fields []) where
    fields =
      [ Input "chat_id" $ case sendStickerChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendStickerMessageThreadId <&>
        \t -> Input "message_thread_id" (T.pack $ show t)
      , sendStickerDisableNotification <&>
        \t -> Input "disable_notification" (bool "false" "true" t)
      , sendStickerProtectContent <&>
        \t -> Input "protect_content" (bool "false" "true" t)
      , sendStickerReplyToMessageId <&>
        \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
      , sendStickerReplyParameters <&>
        \t -> Input "reply_parameters" (TL.toStrict $ encodeToLazyText t)
      , sendStickerReplyMarkup <&>
        \t -> Input "reply_markup" (TL.toStrict $ encodeToLazyText t)
      ]

type SendStickerContent
  = "sendSticker"
  :> MultipartForm Tmp SendStickerRequest
  :> Post '[JSON] (Response Message)

type SendStickerLink
  = "sendSticker"
  :> ReqBody '[JSON] SendStickerRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send static .WEBP or animated .TGS stickers.
--   On success, the sent Message is returned.
sendSticker :: SendStickerRequest -> ClientM (Response Message)
sendSticker r =
  case sendStickerSticker r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @SendStickerContent) (boundary, r)
    _ -> client (Proxy @SendStickerLink) r

-- ** 'getCustomEmojiStickers'

-- | Request parameters for 'getCustomEmojiStickers'.
data GetCustomEmojiStickersRequest = GetCustomEmojiStickersRequest
  { getCustomEmojiStickersRequestCustomEmojiIds :: [Text] -- ^ List of custom emoji identifiers. At most 200 custom emoji identifiers can be specified.
  }
  deriving Generic

instance ToJSON GetCustomEmojiStickersRequest where toJSON = gtoJSON

type GetCustomEmojiStickers
  = "getCustomEmojiStickers"
  :> ReqBody '[JSON] GetCustomEmojiStickersRequest
  :> Post '[JSON] (Response [Sticker])

getCustomEmojiStickers :: GetCustomEmojiStickersRequest -> ClientM (Response [Sticker])
getCustomEmojiStickers = client (Proxy @GetCustomEmojiStickers)

-- ** 'uploadStickerFile'

-- | Request parameters for 'uploadStickerFile'.
data UploadStickerFileRequest = UploadStickerFileRequest
  { uploadStickerFileUserId        :: UserId -- ^ User identifier of sticker file owner
  , uploadStickerFileSticker       :: InputFile -- ^ A file with the sticker in .WEBP, .PNG, .TGS, or .WEBM format. See https://core.telegram.org/stickers for technical requirements.
  , uploadStickerFileStickerFormat :: Text -- ^ Format of the sticker, must be one of “static”, “animated”, “video”
  } deriving Generic

instance ToJSON UploadStickerFileRequest where toJSON = gtoJSON

instance ToMultipart Tmp UploadStickerFileRequest where
  toMultipart UploadStickerFileRequest{..} =
    makeFile "sticker" uploadStickerFileSticker (MultipartData fields []) where
    fields = [ Input "user_id" $ T.pack . show $ uploadStickerFileUserId
             , Input "sticker_format" $ T.pack . show $ uploadStickerFileStickerFormat
             ]

type UploadStickerFileContent
  = "uploadStickerFile"
  :> MultipartForm Tmp UploadStickerFileRequest
  :> Post '[JSON] (Response File)

type UploadStickerFileLink
  = "uploadStickerFile"
  :> ReqBody '[JSON] UploadStickerFileRequest
  :> Post '[JSON] (Response File)

-- | Use this method to upload f file in .WEBP, .PNG, .TGS, or .WEBM format
--   with a sticker for later use in createNewStickerSet
--   and addStickerToSet methods (can be used multiple times).
--   Returns the uploaded File on success.
uploadStickerFile :: UploadStickerFileRequest -> ClientM (Response File)
uploadStickerFile r =
  case uploadStickerFileSticker r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @UploadStickerFileContent) (boundary, r)
    _ -> client (Proxy @UploadStickerFileLink) r


-- ** 'createNewStickerSet'

-- | Request parameters for 'createNewStickerSet'.
data CreateNewStickerSetRequest = CreateNewStickerSetRequest
  { createNewStickerSetUserId        :: UserId -- ^ User identifier of created sticker set owner
  , createNewStickerSetName          :: T.Text -- ^ Short name of sticker set, to be used in t.me/addstickers/ URLs (e.g., animals). Can contain only english letters, digits and underscores. Must begin with a letter, can't contain consecutive underscores and must end in “_by_<bot username>”. <bot_username> is case insensitive. 1-64 characters.
  , createNewStickerSetTitle         :: T.Text -- ^ Sticker set title, 1-64 characters
  , createNewStickerSetStickers      :: [InputSticker] -- ^ A JSON-serialized list of 1-50 initial stickers to be added to the sticker set.
  , createNewStickerFormat           :: Text -- ^ Format of stickers in the set, must be one of “static”, “animated”, “video”.
  , createNewStickerSetType             :: Maybe StickerSetType -- ^ Type of stickers in the set, pass “regular”, “mask”, or “custom_emoji”. By default, a regular sticker set is created.
  , createNewStickerSetNeedsRepainting :: Maybe Bool -- ^ 'True' if stickers in the sticker set must be repainted to the color of text when used in messages, the accent color if used as emoji status, white on chat photos, or another appropriate color based on context; for custom emoji sticker sets only.
  } deriving Generic

instance ToJSON CreateNewStickerSetRequest where toJSON = gtoJSON

type CreateNewStickerSet
  = "createNewStickerSet"
  :> ReqBody '[JSON] CreateNewStickerSetRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to create a new sticker
--   set owned by a user. The bot will be able
--   to edit the sticker set thus created. You
--   must use exactly one of the fields png_sticker or tgs_sticker.
--   Returns True on success.
createNewStickerSet :: CreateNewStickerSetRequest -> ClientM (Response Bool)
createNewStickerSet = client (Proxy @CreateNewStickerSet)

-- ** 'addStickerToSet'

-- | Request parameters for 'addStickerToSet'.
data AddStickerToSetRequest = AddStickerToSetRequest
  { addStickerToSetUserId       :: UserId -- ^ User identifier of sticker set owner
  , addStickerToSetName         :: T.Text -- ^ Sticker set name
  , addStickerToSetStickers     :: InputSticker -- ^ A JSON-serialized object with information about the added sticker. If exactly the same sticker had already been added to the set, then the set isn't changed.
  } deriving Generic

instance ToJSON AddStickerToSetRequest where toJSON = gtoJSON

type AddStickerToSet
  = "addStickerToSet"
  :> ReqBody '[JSON] AddStickerToSetRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to add a new sticker to a set
--   created by the bot. You must use exactly one of
--   the fields png_sticker or tgs_sticker. Animated
--   stickers can be added to animated sticker sets and
--   only to them. Animated sticker sets can have up to 50
--   stickers. Static sticker sets can have up to 120 stickers.
--   Returns True on success.
addStickerToSet :: AddStickerToSetRequest -> ClientM (Response Bool)
addStickerToSet = client (Proxy @AddStickerToSet)


-- ** 'getStickerSet'

type GetStickerSet
  = "getStickerSet"
  :> RequiredQueryParam "name" T.Text
  :> Get '[JSON] (Response StickerSet)

-- | Use this method to get a sticker set. On success, a StickerSet object is returned.
getStickerSet :: T.Text -- ^ Name of the sticker set
  -> ClientM (Response StickerSet)
getStickerSet = client (Proxy @GetStickerSet)

-- ** 'setStickerPositionInSet'

type SetStickerPositionInSet
  = "setStickerPositionInSet"
  :> RequiredQueryParam "sticker" T.Text
  :> RequiredQueryParam "position" Integer
  :> Post '[JSON] (Response Bool)

-- | Use this method to move a sticker in a set created by the bot to a specific position.
--   Returns True on success.
setStickerPositionInSet :: T.Text -- ^ File identifier of the sticker
  -> Integer -- ^ New sticker position in the set, zero-based
  -> ClientM (Response Bool)
setStickerPositionInSet = client (Proxy @SetStickerPositionInSet)


-- ** 'deleteStickerFromSet'

type DeleteStickerFromSet
  = "deleteStickerFromSet"
  :> RequiredQueryParam "sticker" T.Text
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete a sticker from a set created by the bot.
--   Returns True on success.
deleteStickerFromSet :: T.Text -- ^ File identifier of the sticker
  -> ClientM (Response Bool)
deleteStickerFromSet = client (Proxy @DeleteStickerFromSet)

-- ** 'setStickerSetThumbnail'

-- | Request parameters for 'setStickerSetThumbnail'.
data SetStickerSetThumbnailRequest = SetStickerSetThumbnailRequest
  { setStickerSetThumbnailName   :: T.Text -- ^ Sticker set name
  , setStickerSetThumbnailUserId :: UserId -- ^ User identifier of the sticker set owner
  , setStickerSetThumbnailThumbnail  :: InputFile -- ^ A PNG image with the thumbnail, must be up to 128 kilobytes in size and have width and height exactly 100px, or a TGS animation with the thumbnail up to 32 kilobytes in size; see <https:\/\/core.telegram.org\/animated_stickers#technical-requirements> for animated sticker technical requirements. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. Animated sticker set thumbnail can't be uploaded via HTTP URL.
  } deriving Generic

instance ToJSON SetStickerSetThumbnailRequest where toJSON = gtoJSON

instance ToMultipart Tmp SetStickerSetThumbnailRequest where
  toMultipart SetStickerSetThumbnailRequest{..} =
    makeFile "png_sticker" setStickerSetThumbnailThumbnail (MultipartData fields []) where
    fields =
      [ Input "user_id" $ T.pack . show $ setStickerSetThumbnailUserId
      , Input "name" setStickerSetThumbnailName
      ]

type SetStickerSetThumbnailContent
  = "setStickerSetThumbnail"
  :> MultipartForm Tmp SetStickerSetThumbnailRequest
  :> Post '[JSON] (Response Bool)

type SetStickerSetThumbnailLink
  = "setStickerSetThumbnail"
  :> ReqBody '[JSON] SetStickerSetThumbnailRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set the thumbnail of a sticker set.
--   Animated thumbnails can be set for animated sticker sets only.
--   Returns True on success.
setStickerSetThumbnail :: SetStickerSetThumbnailRequest -> ClientM (Response Bool)
setStickerSetThumbnail r =
  case setStickerSetThumbnailThumbnail r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @SetStickerSetThumbnailContent) (boundary, r)
    _ -> client (Proxy @SetStickerSetThumbnailLink) r

foldMap makeDefault
  [ ''SendStickerRequest
  , ''GetCustomEmojiStickersRequest
  , ''UploadStickerFileRequest
  , ''CreateNewStickerSetRequest
  , ''AddStickerToSetRequest
  , ''SetStickerSetThumbnailRequest
  ]
