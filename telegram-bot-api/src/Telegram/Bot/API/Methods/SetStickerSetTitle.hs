{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetStickerSetTitle where

import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'setStickerSetTitle'
type SetStickerSetTitle = "setStickerSetTitle"
    :> RequiredQueryParam "name" Text
    :> RequiredQueryParam "title" Text
    :> Post '[JSON] (Response Bool)

-- | Use this method to set the title of a created sticker set.
--   Returns True on success.
setStickerSetTitle :: Text -- ^ Sticker set name
  -> Text -- ^ Sticker set title, 1-64 characters
  -> ClientM (Response Bool)
setStickerSetTitle = client (Proxy @SetStickerSetTitle)
