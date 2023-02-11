{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.MaskPosition where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'MaskPosition'

-- | This object describes the position on faces where a mask should be placed by default.
data MaskPosition = MaskPosition
  { maskPositionPoint  :: Text  -- ^ The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
  , maskPositionXShift :: Float -- ^ Shift by X-axis measured in widths of the mask scaled to the face size, from left to right. For example, choosing -1.0 will place mask just to the left of the default mask position.
  , maskPositionYShift :: Float -- ^ Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom. For example, 1.0 will place the mask just below the default mask position.
  , maskPositionScale  :: Float -- ^ Mask scaling coefficient. For example, 2.0 means double size.
  }
  deriving (Generic, Show)

deriveJSON' ''MaskPosition
