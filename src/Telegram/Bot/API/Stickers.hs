{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}

{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Telegram.Bot.API.Stickers where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Text
import Data.Bool
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)
import Servant.Multipart
import Servant.Multipart.Client
import System.FilePath

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests (Response)
import Telegram.Bot.API.Types
import Telegram.Bot.API.Methods

data SendStickerRequest = SendStickerRequest
  { sendStickerChatId                   :: SomeChatId
  , sendStickerSticker                  :: InputFile
  , sendStickerDisableNotification      :: Maybe Bool
  , sendStickerReplyToMessageId         :: Maybe MessageId
  , sendStickerAllowSendingWithoutReply :: Maybe Bool
  , sendStickerReplyMarkup              :: Maybe InlineKeyboardMarkup
  }
  deriving Generic

instance ToJSON SendStickerRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendStickerRequest where
  toMultipart SendStickerRequest{..} = MultipartData fields files where
    fields =
      [ Input "sticker" $ T.pack "attach://file"
      , Input "chat_id" $ case sendStickerChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <>
          maybe id (\t -> (Input "disable_notification" (bool "false" "true" t):)) sendStickerDisableNotification
        ( maybe id (\t -> (Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t):)) sendStickerReplyToMessageId
        $ maybe id (\t -> (Input "allow_sending_without_reply" (bool "false" "true" t):)) sendStickerAllowSendingWithoutReply
        $ maybe id (\t -> (Input "reply_markup" (TL.toStrict $ encodeToLazyText t):)) sendStickerReplyMarkup
        [])
    files
      = [FileData "file" (T.pack $ takeFileName path) ct path]

    InputFile path ct = sendStickerSticker

type SendStickerContent
  = "sendSticker"
  :> MultipartForm Tmp SendStickerRequest
  :> Post '[JSON] (Response Message)

type SendStickerLink
  = "sendSticker"
  :> ReqBody '[JSON] SendStickerRequest
  :> Post '[JSON] (Response Message)

sendSticker :: SendStickerRequest -> ClientM (Response Message)
sendSticker r =
  case sendStickerSticker r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @SendStickerContent) (boundary, r)
    _ -> client (Proxy @SendStickerLink) r


data UploadStickerFileRequest = UploadStickerFileRequest
  { uploadStickerFileUserId :: UserId
  , uploadStickerFilePngSticker :: InputFile
  } deriving Generic

instance ToJSON UploadStickerFileRequest where toJSON = gtoJSON

instance ToMultipart Tmp UploadStickerFileRequest where
  toMultipart UploadStickerFileRequest{..} = MultipartData fields files where
    fields =
      [ Input "png_sticker" $ T.pack $ "attach://file"
      , Input "user_id" $ T.pack . show $ userId
      ]
    files
      = [FileData "file" (T.pack $ takeFileName path) ct path]

    UserId userId     = uploadStickerFileUserId
    InputFile path ct = uploadStickerFilePngSticker

type UploadStickerFileContent
  = "uploadStickerFile"
  :> MultipartForm Tmp UploadStickerFileRequest
  :> Post '[JSON] (Response File)

type UploadStickerFileLink
  = "uploadStickerFile"
  :> ReqBody '[JSON] UploadStickerFileRequest
  :> Post '[JSON] (Response File)

uploadNewStickerFile :: UploadStickerFileRequest -> ClientM (Response File)
uploadNewStickerFile r =
  case uploadStickerFilePngSticker r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @UploadStickerFileContent) (boundary, r)
    _ -> client (Proxy @UploadStickerFileLink) r


data CreateNewStickerSetRequest = CreateNewStickerSetRequest
  { createNewStickerSetUserId :: UserId
  , createNewStickerSetName :: T.Text
  , createNewStickerSetTitle :: T.Text
  , createNewStickerSetPngSticker :: InputFile
  , createNewStickerSetEmojis :: T.Text
  , createNewStickerSetContainsMasks :: Maybe Bool
  , createNewStickerSetMaskPosition :: Maybe MaskPosition
  } deriving Generic

instance ToJSON CreateNewStickerSetRequest where toJSON = gtoJSON

instance ToMultipart Tmp CreateNewStickerSetRequest where
  toMultipart CreateNewStickerSetRequest{..} = MultipartData fields files where
    fields =
      [ Input "png_sticker" $ T.pack "attach://file"
      , Input "user_id" $ T.pack . show $ userId
      , Input "name" createNewStickerSetName
      , Input "title" createNewStickerSetTitle
      , Input "emojis" createNewStickerSetEmojis
      ] <>
          maybe id (\t -> (Input "contains_masks" (bool "false" "true" t):)) createNewStickerSetContainsMasks
        ( maybe id (\t -> (Input "mask_position" (TL.toStrict $ encodeToLazyText t):)) createNewStickerSetMaskPosition
        [])
    files
      = [FileData "file" (T.pack $ takeFileName path) ct path]

    UserId userId     = createNewStickerSetUserId
    InputFile path ct = createNewStickerSetPngSticker

type CreateNewStickerSetContent
  = "createNewStickerSet"
  :> MultipartForm Tmp CreateNewStickerSetRequest
  :> Post '[JSON] (Response Bool)

type CreateNewStickerSetLink
  = "createNewStickerSet"
  :> ReqBody '[JSON] CreateNewStickerSetRequest
  :> Post '[JSON] (Response Bool)

createNewStickerSet :: CreateNewStickerSetRequest -> ClientM (Response Bool)
createNewStickerSet r =
  case createNewStickerSetPngSticker r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @CreateNewStickerSetContent) (boundary, r)
    _ -> client (Proxy @CreateNewStickerSetLink) r

data AddStickerToSetRequest = AddStickerToSetRequest
  { addStickerToSetUserId :: UserId
  , addStickerToSetName :: T.Text
  , addStickerToSetPngSticker :: InputFile
  , addStickerToSetEmojis :: T.Text
  , addStickerToSetMaskPosition :: Maybe MaskPosition
  } deriving Generic

instance ToJSON AddStickerToSetRequest where toJSON = gtoJSON

instance ToMultipart Tmp AddStickerToSetRequest where
  toMultipart AddStickerToSetRequest{..} = MultipartData fields files where
    fields =
      [ Input "png_sticker" $ T.pack "attach://file"
      , Input "user_id" $ T.pack . show $ userId
      , Input "name" addStickerToSetName
      , Input "emojis" addStickerToSetEmojis
      ] <>
        maybe [] (\t -> [Input "mask_position" (TL.toStrict $ encodeToLazyText t)]) addStickerToSetMaskPosition
        
    files
      = [FileData "file" (T.pack $ takeFileName path) ct path]

    UserId userId     = addStickerToSetUserId
    InputFile path ct = addStickerToSetPngSticker

type AddStickerToSetContent
  = "addStickerToSet"
  :> MultipartForm Tmp AddStickerToSetRequest
  :> Post '[JSON] (Response Bool)

type AddStickerToSetLink
  = "addStickerToSet"
  :> ReqBody '[JSON] AddStickerToSetRequest
  :> Post '[JSON] (Response Bool)

addStickerToSet :: AddStickerToSetRequest -> ClientM (Response Bool)
addStickerToSet r =
  case addStickerToSetPngSticker r of
    InputFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @AddStickerToSetContent) (boundary, r)
    _ -> client (Proxy @AddStickerToSetLink) r


type GetStickerSet
  = "getStickerSet"
  :> RequiredQueryParam "name" T.Text
  :> Get '[JSON] (Response StickerSet)

getStickerSet :: T.Text -> ClientM (Response StickerSet)
getStickerSet = client (Proxy @GetStickerSet)


type SetStickerPositionInSet
  = "setStickerPositionInSet"
  :> RequiredQueryParam "sticker" T.Text
  :> RequiredQueryParam "position" Integer
  :> Post '[JSON] (Response Bool)

setStickerPositionInSet :: T.Text -> Integer -> ClientM (Response Bool)
setStickerPositionInSet = client (Proxy @SetStickerPositionInSet)


type DeleteStickerFromSet
  = "deleteStickerFromSet"
  :> RequiredQueryParam "sticker" T.Text
  :> Post '[JSON] (Response Bool)

deleteStickerFromSet ::  T.Text -> ClientM (Response Bool)
deleteStickerFromSet = client (Proxy @DeleteStickerFromSet)

