{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Telegram.Bot.Simple.Conversation where

import           Data.Bifunctor
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Maybe                 (fromMaybe)

import           Telegram.Bot.API
import           Telegram.Bot.Simple.BotApp

conversationBot
  :: (Eq conversation, Hashable conversation)
  => (Update -> Maybe conversation)
  -> BotApp model action
  -> BotApp (HashMap (Maybe conversation) model) (Maybe conversation, action)
conversationBot toConversation BotApp{..} = BotApp
  { botInitialModel = conversationInitialModel
  , botAction       = conversationAction
  , botHandler      = conversationHandler
  , botJobs         = conversationJobs
  }
  where
    conversationInitialModel = HashMap.empty

    conversationAction update conversations = do
      conversation <- toConversation update
      let model = fromMaybe botInitialModel (HashMap.lookup (Just conversation) conversations)
      (Just conversation,) <$> botAction update model

    conversationHandler (conversation, action) conversations =
      bimap (conversation,) (\m -> HashMap.insert conversation m conversations) $
        botHandler action model
      where
        model = fromMaybe botInitialModel (HashMap.lookup conversation conversations)

    conversationJobs = map toConversationJob botJobs

    toConversationJob BotJob{..} = BotJob
      { botJobSchedule = botJobSchedule
      , botJobTask = HashMap.traverseWithKey $
          \conversation -> first (conversation,) . botJobTask
      }

