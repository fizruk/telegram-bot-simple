{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
module Telegram.Bot.Simple.Instances where

import Data.Text (Text)

import Telegram.Bot.Simple.Eff
import Telegram.Bot.Simple.Reply (replyText)

instance RunBot a a where
  runBot effect = Just <$> effect

instance RunBot () a where
  runBot effect = Nothing <$ effect

instance RunBot Text a where
  runBot  effect = runBot do
    t <- effect
    replyText t