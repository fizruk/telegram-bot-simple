{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE LambdaCase #-}

module Telegram.Bot.API.Stickers where

import Control.Monad.IO.Class
import Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif
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
import Data.Maybe (catMaybes, maybeToList)
import Data.Functor
import Telegram.Bot.API.Internal.TH (makeDefault)


-- | Type of uploaded sticker file. Static or animated.
data StickerType
  = PngSticker -- ^ PNG image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data.
  | TgsSticker -- ^ TGS animation with the sticker, uploaded using multipart/form-data. See <https:\/\/core.telegram.org\/animated_stickers#technical-requirements> for technical requirements.
  | WebmSticker -- ^ WEBM video with the sticker, uploaded using multipart/form-data. See <https:\/\/core.telegram.org\/stickers#video-sticker-requirements> for technical requirements.

stickerLabel :: StickerType -> T.Text
stickerLabel = \case
  PngSticker -> "png_sticker"
  TgsSticker -> "tgs_sticker"
  WebmSticker -> "webm_sticker"

-- | Sticker file with static/animated label.
data StickerFile = StickerFile {stickerFileSticker :: InputFile, stickerFileLabel :: StickerType}

-- ** 'sendSticker'

-- | Request parameters for 'sendSticker'.
data SendStickerRequest = SendStickerRequest
  { sendStickerChatId                   :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendStickerMessageThreadId          :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendStickerSticker                  :: InputFile -- ^ Sticker to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a .WEBP file from the Internet, or upload a new one using multipart/form-data.
  , sendStickerDisableNotification      :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendStickerProtectContent           :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendStickerReplyToMessageId         :: Maybe MessageId -- ^	If the message is a reply, ID of the original message
  , sendStickerAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
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
      , sendStickerAllowSendingWithoutReply <&>
        \t -> Input "allow_sending_without_reply" (bool "false" "true" t)
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

-- ** 'uploadStickerFile'

-- | Request parameters for 'uploadStickerFile'.
data UploadStickerFileRequest = UploadStickerFileRequest
  { uploadStickerFileUserId     :: UserId -- ^ User identifier of sticker file owner
  , uploadStickerFilePngSticker :: InputFile -- ^ PNG image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px.
  } deriving Generic

instance ToJSON UploadStickerFileRequest where toJSON = gtoJSON

instance ToMultipart Tmp UploadStickerFileRequest where
  toMultipart UploadStickerFileRequest{..} =
    makeFile "png_sticker" uploadStickerFilePngSticker (MultipartData fields []) where
    fields = [ Input "user_id" $ T.pack . show $ uploadStickerFileUserId ]

type UploadStickerFileContent
  = "uploadStickerFile"
  :> MultipartForm Tmp UploadStickerFileRequest
  :> Post '[JSON] (Response File)

type UploadStickerFileLink
  = "uploadStickerFile"
  :> ReqBody '[JSON] UploadStickerFileRequest
  :> Post '[JSON] (Response File)

-- | Use this method to upload a .PNG file
--   with a sticker for later use in createNewStickerSet
--   and addStickerToSet methods (can be used multiple times).
--   Returns the uploaded File on success.
uploadStickerFile :: UploadStickerFileRequest -> ClientM (Response File)
uploadStickerFile r =
  case uploadStickerFilePngSticker r of
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
  , createNewStickerSetSticker       :: StickerFile -- ^ Sticker file to upload
  , createNewStickerSetEmojis        :: T.Text -- ^ One or more emoji corresponding to the sticker
  , createNewStickerSetContainsMasks :: Maybe Bool -- ^ Pass True, if a set of mask stickers should be created
  , createNewStickerSetMaskPosition  :: Maybe MaskPosition -- ^ A JSON-serialized object for position where the mask should be placed on faces
  } deriving Generic

instance ToJSON CreateNewStickerSetRequest where
  toJSON CreateNewStickerSetRequest{..} = object
    [ "user_id" .= createNewStickerSetUserId
    , "name" .= createNewStickerSetName
    , "title" .= createNewStickerSetTitle
#if MIN_VERSION_aeson(2,0,0)
    , fromText (stickerLabel stickerFileLabel) .= stickerFileSticker
#else
    , stickerLabel stickerFileLabel .= stickerFileSticker
#endif
    , "emojis" .= createNewStickerSetEmojis
    , "contains_mask" .= createNewStickerSetContainsMasks
    , "mask_position" .= createNewStickerSetMaskPosition
    ]
    where
      StickerFile{..} = createNewStickerSetSticker

instance ToMultipart Tmp CreateNewStickerSetRequest where
  toMultipart CreateNewStickerSetRequest{..} =
    makeFile (stickerLabel stickerFileLabel) stickerFileSticker (MultipartData fields []) where
    fields =
      [ Input "user_id" $ T.pack . show $ createNewStickerSetUserId
      , Input "name" createNewStickerSetName
      , Input "title" createNewStickerSetTitle
      , Input "emojis" createNewStickerSetEmojis
      ] <> catMaybes
      [ createNewStickerSetContainsMasks <&>
        \t -> Input "contains_masks" (bool "false" "true" t)
      , createNewStickerSetMaskPosition <&>
        \t -> Input "mask_position" (TL.toStrict $ encodeToLazyText t)
      ]
    StickerFile {..} = createNewStickerSetSticker

type CreateNewStickerSetContent
  = "createNewStickerSet"
  :> MultipartForm Tmp CreateNewStickerSetRequest
  :> Post '[JSON] (Response Bool)

type CreateNewStickerSetLink
  = "createNewStickerSet"
  :> ReqBody '[JSON] CreateNewStickerSetRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to create a new sticker
--   set owned by a user. The bot will be able
--   to edit the sticker set thus created. You
--   must use exactly one of the fields png_sticker or tgs_sticker.
--   Returns True on success.
createNewStickerSet :: CreateNewStickerSetRequest -> ClientM (Response Bool)
createNewStickerSet r =
  case stickerFileSticker $ createNewStickerSetSticker r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @CreateNewStickerSetContent) (boundary, r)
    _ -> client (Proxy @CreateNewStickerSetLink) r

-- ** 'addStickerToSet'

-- | Request parameters for 'addStickerToSet'.
data AddStickerToSetRequest = AddStickerToSetRequest
  { addStickerToSetUserId       :: UserId -- ^ User identifier of sticker set owner
  , addStickerToSetName         :: T.Text -- ^ Sticker set name
  , addStickerToSetSticker      :: StickerFile -- ^ Sticker file to upload
  , addStickerToSetEmojis       :: T.Text -- ^ One or more emoji corresponding to the sticker
  , addStickerToSetMaskPosition :: Maybe MaskPosition -- ^ A JSON-serialized object for position where the mask should be placed on faces
  } deriving Generic

instance ToJSON AddStickerToSetRequest where
  toJSON AddStickerToSetRequest{..} = object
    [ "user_id" .= addStickerToSetUserId
    , "name" .= addStickerToSetName
#if MIN_VERSION_aeson(2,0,0)
    , fromText (stickerLabel stickerFileLabel) .= stickerFileSticker
#else
    , stickerLabel stickerFileLabel .= stickerFileSticker
#endif
    , "emojis" .= addStickerToSetEmojis
    , "mask_position" .= addStickerToSetMaskPosition
    ]
    where
      StickerFile{..} = addStickerToSetSticker

instance ToMultipart Tmp AddStickerToSetRequest where
  toMultipart AddStickerToSetRequest{..} =
    makeFile (stickerLabel stickerFileLabel) stickerFileSticker (MultipartData fields []) where
    fields =
      [ Input "user_id" $ T.pack . show $ addStickerToSetUserId
      , Input "name" addStickerToSetName
      , Input "emojis" addStickerToSetEmojis
      ] <> maybeToList
      ( addStickerToSetMaskPosition <&>
        \t -> Input "mask_position" (TL.toStrict $ encodeToLazyText t)
      )
    StickerFile {..} = addStickerToSetSticker

type AddStickerToSetContent
  = "addStickerToSet"
  :> MultipartForm Tmp AddStickerToSetRequest
  :> Post '[JSON] (Response Bool)

type AddStickerToSetLink
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
addStickerToSet r =
  case stickerFileSticker $ addStickerToSetSticker r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @AddStickerToSetContent) (boundary, r)
    _ -> client (Proxy @AddStickerToSetLink) r


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

-- ** 'setStickerSetThumb'

-- | Request parameters for 'setStickerSetThumb'.
data SetStickerSetThumbRequest = SetStickerSetThumbRequest
  { setStickerSetThumbName   :: T.Text -- ^ Sticker set name
  , setStickerSetThumbUserId :: UserId -- ^ User identifier of the sticker set owner
  , setStickerSetThumbThumb  :: InputFile -- ^ A PNG image with the thumbnail, must be up to 128 kilobytes in size and have width and height exactly 100px, or a TGS animation with the thumbnail up to 32 kilobytes in size; see <https:\/\/core.telegram.org\/animated_stickers#technical-requirements> for animated sticker technical requirements. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. Animated sticker set thumbnail can't be uploaded via HTTP URL.
  } deriving Generic

instance ToJSON SetStickerSetThumbRequest where toJSON = gtoJSON

instance ToMultipart Tmp SetStickerSetThumbRequest where
  toMultipart SetStickerSetThumbRequest{..} =
    makeFile "png_sticker" setStickerSetThumbThumb (MultipartData fields []) where
    fields =
      [ Input "user_id" $ T.pack . show $ setStickerSetThumbUserId
      , Input "name" setStickerSetThumbName
      ]

type SetStickerSetThumbContent
  = "setStickerSetThumb"
  :> MultipartForm Tmp SetStickerSetThumbRequest
  :> Post '[JSON] (Response Bool)

type SetStickerSetThumbLink
  = "setStickerSetThumb"
  :> ReqBody '[JSON] SetStickerSetThumbRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set the thumbnail of a sticker set.
--   Animated thumbnails can be set for animated sticker sets only.
--   Returns True on success.
setStickerSetThumb :: SetStickerSetThumbRequest -> ClientM (Response Bool)
setStickerSetThumb r =
  case setStickerSetThumbThumb r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @SetStickerSetThumbContent) (boundary, r)
    _ -> client (Proxy @SetStickerSetThumbLink) r

foldMap makeDefault
  [ ''SendStickerRequest
  , ''GetCustomEmojiStickersRequest
  , ''UploadStickerFileRequest
  , ''CreateNewStickerSetRequest
  , ''AddStickerToSetRequest
  , ''SetStickerSetThumbRequest
  ]
