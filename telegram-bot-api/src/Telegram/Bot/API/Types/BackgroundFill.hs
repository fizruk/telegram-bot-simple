{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Types.BackgroundFill where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.=), (.:), withObject)
import Data.Aeson.Types (Parser)
import Data.Text
import GHC.Generics (Generic)

import qualified Data.Text as Text

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'BackgroundFill'

-- | This object describes the way a background is filled based on the selected colors. Currently, it can be one of
--
-- * BackgroundFillSolid
-- * BackgroundFillGradient
-- * BackgroundFillFreeformGradient
--
data BackgroundFill
  -- | The background is filled using the selected color.
  = BackgroundFillSolid
      { backgroundFillSolidType :: Text -- ^ Type of the background fill, always “solid”.
      , backgroundFillSolidColor :: Int  -- ^ The color of the background fill in the RGB24 format.
      }
  -- | The background is a gradient fill.
  | BackgroundFillGradient
      { backgroundFillGradientType :: Text -- ^ Type of the background fill, always “gradient”.
      , backgroundFillGradientTopColor :: Int -- ^ Top color of the gradient in the RGB24 format.
      , backgroundFillGradientBottomColor :: Int -- ^ Bottom color of the gradient in the RGB24 format.
      , backgroundFillGradientRotationAngle :: Int -- ^ Clockwise rotation angle of the background fill in degrees; @0-359@.
      }
  -- | The background is a freeform gradient that rotates after every message in the chat.
  | BackgroundFillFreeformGradient
      { backgroundFillFreeformGradientType :: Text -- ^ Type of the background fill, always “freeform_gradient”.
      , backgroundFillFreeformGradientColors :: [Int] -- ^ A list of the 3 or 4 base colors that are used to generate the freeform gradient in the RGB24 format.
      }
  deriving (Generic, Show)

instance ToJSON BackgroundFill where
  toJSON = \case
    BackgroundFillSolid _t c -> addJsonFields
      (Object mempty)
      (addType "solid" ["color" .= c])
    BackgroundFillGradient _t tc bc ra -> addJsonFields
      (Object mempty)
      (addType "gradient" ["top_color" .= tc, "bottom_color" .= bc, "rotation_angle" .= ra])
    BackgroundFillFreeformGradient _t cs -> addJsonFields
      (Object mempty)
      (addType "freeform_gradient" ["colors" .= cs])

instance FromJSON BackgroundFill where
  parseJSON = withObject "BackgroundFill" \o ->
    (o .: "type" :: Parser Text) >>= \case
    "solid" -> BackgroundFillSolid
      <$> o .: "type"
      <*> o .: "color"
    "gradient" ->BackgroundFillGradient
      <$> o .: "type"
      <*> o .: "top_color"
      <*> o .: "bottom_color"
      <*> o .: "rotation_angle"
    "freeform_gradient" -> BackgroundFillFreeformGradient
      <$> o .: "type"
      <*> o .: "colors"
    t -> fail $ Text.unpack ("Unknown type: " <> t)
