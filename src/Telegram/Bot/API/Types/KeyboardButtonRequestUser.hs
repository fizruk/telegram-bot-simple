{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.KeyboardButtonRequestUser where

import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'KeyboardButtonRequestUser'

-- | This object defines the criteria used to request a suitable user. The identifier of the selected user will be shared with the bot when the corresponding button is pressed.
data KeyboardButtonRequestUser = KeyboardButtonRequestUser
  { keyboardButtonRequestUserRequestId :: RequestId -- ^ Signed 32-bit identifier of the request, which will be received back in the 'UserShared' object. Must be unique within the message
  , keyboardButtonRequestUserUserIsBot :: Maybe Bool -- ^ Pass 'True' to request a bot, pass 'False' to request a regular user. If not specified, no additional restrictions are applied.
  , keyboardButtonRequestUserUserIsPremium :: Maybe Bool -- ^ Pass 'True' to request a premium user, pass 'False' to request a non-premium user. If not specified, no additional restrictions are applied.
  }
  deriving (Generic, Show)

deriveJSON' ''KeyboardButtonRequestUser
