{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetChatPhoto where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Servant.API
import Servant.Multipart.API
import Servant.Multipart.Client
import Servant.Client hiding (Response)

import qualified Data.Text as T

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'setChatPhoto'

-- | Request parameters for 'setChatPhoto'.
data SetChatPhotoRequest = SetChatPhotoRequest
  { setChatPhotoChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , setChatPhotoPhoto :: InputFile -- ^ 	New chat photo, uploaded using multipart/form-data
  }

instance ToMultipart Tmp SetChatPhotoRequest where
  toMultipart SetChatPhotoRequest{..} =
    makeFile "photo" setChatPhotoPhoto (MultipartData fields []) where
    fields =
      [ Input "chat_id" $ case setChatPhotoChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ]

type SetChatPhoto = "setChatPhoto"
  :> MultipartForm Tmp SetChatPhotoRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set a new profile
--   photo for the chat. Photos can't be changed
--   for private chats. The bot must be an
--   administrator in the chat for this to work
--   and must have the appropriate administrator rights.
--   Returns True on success.
--
-- *Note*: Only 'InputFile' case might be used in 'SetChatPhotoRequest'.
-- Rest cases will be rejected by Telegram.
setChatPhoto :: SetChatPhotoRequest ->  ClientM (Response Bool)
setChatPhoto r =do
      boundary <- liftIO genBoundary
      client (Proxy @SetChatPhoto) (boundary, r)

makeDefault ''SetChatPhotoRequest
