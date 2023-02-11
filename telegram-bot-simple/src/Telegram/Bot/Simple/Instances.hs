{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
module Telegram.Bot.Simple.Instances where

import Data.Text (Text)

import Telegram.Bot.Simple.Eff
import Telegram.Bot.Simple.Reply (replyText)

instance GetAction a a where
  getNextAction effect = Just <$> effect

instance GetAction () a where
  getNextAction effect = Nothing <$ effect

instance GetAction Text a where
  getNextAction effect = getNextAction do
    t <- effect
    replyText t
