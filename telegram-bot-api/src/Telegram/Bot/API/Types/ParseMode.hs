{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ParseMode where

import Data.Aeson
import GHC.Generics (Generic)

data ParseMode
  = Markdown
  | HTML
  | MarkdownV2
  deriving (Generic, Show)

instance ToJSON   ParseMode
instance FromJSON ParseMode
