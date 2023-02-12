{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Types.MenuButton where

import Data.Aeson (KeyValue ((.=)), FromJSON (..), ToJSON (..), object)
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'MenuButton'

-- | This object describes the bot's menu button in a private chat.
-- If a menu button other than @MenuButtonDefault@ is set for a private chat, then it is applied in the chat. Otherwise the default menu button is applied. By default, the menu button opens the list of bot commands.
data MenuButton
  = MenuButtonCommands -- ^ Represents a menu button, which opens the bot's list of commands.
  | MenuButtonWebApp -- ^ Represents a menu button, which launches a Web App.
      { menuButtonWebAppText :: Text
      , menuButtonWebAppWebApp :: WebAppInfo
      } 
  | MenuButtonDefault -- ^ Describes that no specific value for the menu button was set.
  deriving Generic

instance ToJSON MenuButton where
  toJSON = \case
    MenuButtonCommands ->
      object $ addType "commands" []
    MenuButtonWebApp txt wai ->
      object $ addType "web_app" ["text" .= txt, "web_app_info" .= wai]
    MenuButtonDefault ->
      object $ addType "default" []

instance FromJSON MenuButton where
  parseJSON = gparseJSON
