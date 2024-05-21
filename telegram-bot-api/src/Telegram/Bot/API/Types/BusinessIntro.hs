{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BusinessIntro where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Sticker (Sticker)

-- ** 'BusinessIntro'

-- | Contains information about the start page settings of a Telegram Business account.
data BusinessIntro = BusinessIntro
  { businessIntroTitle :: Maybe Text -- ^ Title text of the business intro.
  , businessIntroMessage :: Maybe Text -- ^ Message text of the business intro.
  , businessIntroSticker :: Maybe Sticker -- ^ Sticker of the business intro.
  }
  deriving (Generic, Show)

instance ToJSON   BusinessIntro where toJSON = gtoJSON
instance FromJSON BusinessIntro where parseJSON = gparseJSON
