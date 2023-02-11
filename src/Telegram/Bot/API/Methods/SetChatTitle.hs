{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetChatTitle where

import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'setChatTitle'

type SetChatTitle = "setChatTitle"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "title" Text
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the title of
--   a chat. Titles can't be changed for private
--   chats. The bot must be an administrator in 
--   the chat for this to work and must have the 
--   appropriate administrator rights. 
--   Returns True on success.
setChatTitle :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Text -- ^ New chat title, 0-255 characters
  -> ClientM (Response Bool)
setChatTitle = client (Proxy @SetChatTitle)
