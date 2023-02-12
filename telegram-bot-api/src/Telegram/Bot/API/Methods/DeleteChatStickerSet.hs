{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.DeleteChatStickerSet where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'deleteChatStickerSet'

type DeleteChatStickerSet = "deleteChatStickerSet"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete a group sticker 
--   set from a supergroup. The bot must be an 
--   administrator in the chat for this to work 
--   and must have the appropriate administrator 
--   rights. Use the field can_set_sticker_set 
--   optionally returned in getChat requests 
--   to check if the bot can use this method. 
--   Returns True on success.
deleteChatStickerSet :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
deleteChatStickerSet = client (Proxy @DeleteChatStickerSet)
