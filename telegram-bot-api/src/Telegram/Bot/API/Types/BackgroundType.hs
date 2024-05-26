{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Types.BackgroundType where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.=), (.:), (.:?), withObject)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as Text

import Telegram.Bot.API.Types.BackgroundFill
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.Document
import Telegram.Bot.API.Internal.Utils

-- ** 'BackgroundType'

-- | This object describes the type of a background. Currently, it can be one of
--
-- * BackgroundTypeFill
-- * BackgroundTypeWallpaper
-- * BackgroundTypePattern
-- * BackgroundTypeChatTheme
--
data BackgroundType
  -- | The background is automatically filled based on the selected colors.
  = BackgroundTypeFill
      { backgroundTypeFillType :: Text -- ^ Type of the background, always “fill”.
      , backgroundTypeFillFill :: BackgroundFill -- ^ The background fill.
      , backgroundTypeFillDarkThemeDimming :: Int -- ^ Dimming of the background in dark themes, as a percentage; @0-100@.
      }
  -- | The background is a wallpaper in the JPEG format.
  | BackgroundTypeWallpaper
      { backgroundTypeWallpaperType :: Text -- ^ Type of the background, always “wallpaper”.
      , backgroundTypeWallpaperDocument :: Document -- ^ Document with the wallpaper.
      , backgroundTypeWallpaperDarkThemeDimming :: Int -- ^ Dimming of the background in dark themes, as a percentage; @0-100@.
      , backgroundTypeWallpaperIsBlurred :: Maybe Bool -- ^ 'True', if the wallpaper is downscaled to fit in a @450x450@ square and then box-blurred with radius 12.
      , backgroundTypeWallpaperIsMoving :: Maybe Bool -- ^ 'True', if the background moves slightly when the device is tilted.
      }
  -- | The background is a PNG or TGV (gzipped subset of SVG with MIME type “application/x-tgwallpattern”) pattern to be combined with the background fill chosen by the user.
  | BackgroundTypePattern
      { backgroundTypePatternType :: Text -- ^ Type of the background, always “pattern”.
      , backgroundTypePatternDocument :: Document -- ^ Document with the pattern.
      , backgroundTypePatternFill :: BackgroundFill -- ^ The background fill that is combined with the pattern.
      , backgroundTypePatternIntensity :: Int -- ^ Intensity of the pattern when it is shown above the filled background; @0-100@.
      , backgroundTypePatternIsInverted :: Maybe Bool -- ^ 'True', if the background fill must be applied only to the pattern itself. All other pixels are black in this case. For dark themes only.
      , backgroundTypePatternIsMoving :: Maybe Bool -- ^ 'True', if the background moves slightly when the device is tilted.
      }
  -- | The background is taken directly from a built-in chat theme.
  | BackgroundTypeChatTheme
      { backgroundTypeChatThemeType :: Text -- ^ Type of the background, always “chat_theme”.
      , backgroundTypeChatThemeThemeName :: Text -- ^ Name of the chat theme, which is usually an emoji.
      }
  deriving (Generic, Show)

instance ToJSON BackgroundType where
  toJSON = \case
     BackgroundTypeFill _t f dtd -> addJsonFields
       (Object mempty)
       (addType "fill" ["fill" .= f, "dark_theme_dimming" .= dtd])
     BackgroundTypeWallpaper _t d dtd ib im -> addJsonFields
       (Object mempty)
       (addType "wallpaper" ["document" .= d, "dark_theme_dimming" .= dtd, "is_blurred" .= ib, "is_moving" .= im])
     BackgroundTypePattern _t d f i ii im -> addJsonFields
       (Object mempty)
       (addType "pattern" ["document" .= d, "fill" .= f, "intensity" .= i, "is_inverted" .= ii, "is_moving" .= im])
     BackgroundTypeChatTheme _t tn -> addJsonFields
       (Object mempty)
       (addType "chat_theme" ["theme_name" .= tn])

instance FromJSON BackgroundType where
  parseJSON = withObject "BackgroundType" \o ->
    (o .: "type" :: Parser Text) >>= \case
    "fill" -> BackgroundTypeFill
      <$> o .: "type"
      <*> o .: "fill"
      <*> o .: "dark_theme_dimming"
    "wallpaper" -> BackgroundTypeWallpaper
      <$> o .: "type"
      <*> o .: "document"
      <*> o .: "dark_theme_dimming"
      <*> o .:? "is_blurred"
      <*> o .:? "is_moving"
    "pattern" -> BackgroundTypePattern
      <$> o .: "type"
      <*> o .: "document"
      <*> o .: "fill"
      <*> o .: "intensity"
      <*> o .:? "is_inverted"
      <*> o .:? "is_moving"
    "chat_theme" -> BackgroundTypeChatTheme
      <$> o .: "type"
      <*> o .: "theme_name"
    t -> fail $ Text.unpack ("Unknown type: " <> t)
