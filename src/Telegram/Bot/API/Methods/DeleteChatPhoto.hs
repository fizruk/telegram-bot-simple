{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.DeleteChatPhoto where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'deleteChatPhoto'

type DeleteChatPhoto = "deleteChatPhoto"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete a chat photo.
--   Photos can't be changed for private chats.
--   The bot must be an administrator in the chat 
--   for this to work and must have the appropriate 
--   administrator rights. 
--   Returns True on success.
deleteChatPhoto :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
deleteChatPhoto = client (Proxy @DeleteChatPhoto)
