{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.LinkPreviewOptions where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'LinkPreviewOptions'

data LinkPreviewOptions = LinkPreviewOptions
  { linkPreviewOptionsIsDisabled :: Maybe Bool -- ^ 'True', if the link preview is disabled.
  , linkPreviewOptionsUrl :: Maybe Text -- ^ URL to use for the link preview. If empty, then the first URL found in the message text will be used.
  , linkPreviewOptionsPreferSmallMedia :: Maybe Bool -- ^ 'True', if the media in the link preview is suppposed to be shrunk; ignored if the URL isn't explicitly specified or media size change isn't supported for the preview.
  , linkPreviewOptionsPreferLargeMedia :: Maybe Bool -- ^ 'True', if the media in the link preview is suppposed to be enlarged; ignored if the URL isn't explicitly specified or media size change isn't supported for the preview.
  , linkPreviewOptionsShowAboveText :: Maybe Bool -- ^ 'True', if the link preview must be shown above the message text; otherwise, the link preview will be shown below the message text.
  }
  deriving (Generic, Show)

instance ToJSON   LinkPreviewOptions where toJSON = gtoJSON
instance FromJSON LinkPreviewOptions where parseJSON = gparseJSON
