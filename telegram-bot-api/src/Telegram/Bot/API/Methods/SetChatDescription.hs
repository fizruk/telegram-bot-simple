{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetChatDescription where

import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'setChatDescription'

type SetChatDescription = "setChatDescription"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> QueryParam "description" Text
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the description 
--   of a group, a supergroup or a channel. The 
--   bot must be an administrator in the chat 
--   for this to work and must have the appropriate 
--   administrator rights. 
--   Returns True on success.
setChatDescription :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Maybe Text -- ^ New chat description, 0-255 characters
  -> ClientM (Response Bool)
setChatDescription = client (Proxy @SetChatDescription)
