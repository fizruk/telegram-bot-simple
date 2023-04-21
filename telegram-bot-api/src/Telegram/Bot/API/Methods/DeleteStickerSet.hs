{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.DeleteStickerSet where

import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'deleteStickerSet'

type DeleteStickerSet = "deleteStickerSet"
  :> RequiredQueryParam "name" Text
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete a sticker set that was created by the bot.
--   Returns 'True' on success.
deleteStickerSet :: Text -- ^ Sticker set name
  -> ClientM (Response Bool)
deleteStickerSet = client (Proxy @DeleteStickerSet)
