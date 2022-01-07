{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
module Telegram.Bot.Simple.Instances where

import Data.Text (Text)

import Telegram.Bot.Simple.Eff
import Telegram.Bot.Simple.Reply (replyText)

instance RunBot a a where
  runBot update effect = Just <$> runBotM update effect

instance RunBot () a where
  runBot update effect = Nothing <$ runBotM  update effect

instance RunBot Text a where
  runBot update effect = runBot update do
    t <- effect
    replyText t