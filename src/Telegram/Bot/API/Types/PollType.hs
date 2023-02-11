{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Types.PollType where

import Data.Aeson (Value(String), FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'PollType'

data PollType =
  PollTypeQuiz | PollTypeRegular
  deriving (Generic, Show)

getPollType :: PollType -> Text
getPollType PollTypeQuiz = "quiz"
getPollType PollTypeRegular = "regular"

instance ToJSON PollType where
  toJSON = String . getPollType

instance FromJSON PollType where parseJSON = gparseJSON
