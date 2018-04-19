{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Control.Applicative
import Data.Foldable (asum)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Item = Text

data Model = Model
  { todoLists   :: HashMap Text [Item]
  , currentList :: Text
  }
  deriving (Show)


data Conversation = Conversation
 { convChatId    :: ChatId
 , convUserId    :: Maybe UserId
 , convMessageId :: MessageId
 }
 deriving (Show)

instance Eq Conversation where
  (==) x y = (convChatId x) == (convChatId y)

instance Hashable Conversation where
  hashWithSalt _ c = let ChatId chId = convChatId c in fromInteger chId
       

updateConversations :: Update -> Maybe Conversation
updateConversations Update{..} = do
  Message{..} <- asum
    [ updateMessage
    , updateEditedMessage
    , updateChannelPost
    , updateEditedChannelPost
    , callbackMessage
    ]
  return Conversation 
    { convChatId = chatId messageChat
    , convUserId = case messageFrom of
                    Just user -> Just (userId user)
                    Nothing   -> Nothing
    , convMessageId = messageMessageId
    }
  where
    callbackMessage = case updateCallbackQuery of
      Just cb -> callbackQueryMessage cb
      Nothing -> Nothing

defaultListName :: Text
defaultListName = "Default"

initialModel :: Model
initialModel = Model
  { todoLists = HashMap.fromList [(defaultListName, [])]
  , currentList = defaultListName
  }

data Action
  = NoOp
  | Start
  | AddItem Item
  | RemoveItem Item
  | SwitchToList Text
  | ShowAll
  | Show Text
  | ShowChatId
  | CallBack Text
  deriving (Show, Read)

todoBot3 :: BotApp (Maybe Conversation, Model) Action
todoBot3 = BotApp
  { botInitialModel = (Nothing, initialModel)
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: (Maybe Conversation, Model) -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
           AddItem      <$> plainText
       <|> CallBack     <$> callbackQueryDataRead
       <|> Start        <$  command "start"
       <|> AddItem      <$> command "add"
       <|> RemoveItem   <$> command "remove"
       <|> SwitchToList <$> command "switch_to_list"
       <|> Show         <$> command "show"
       <|> ShowAll      <$  command "show_all"
       <|> ShowChatId   <$  command "chat_id"

    handleAction :: Action -> (Maybe Conversation, Model) -> Eff Action (Maybe Conversation, Model)
    handleAction action fullModel@(mconv, model) = case action of
      NoOp -> pure fullModel
      Start -> fullModel <# do
        reply (toReplyMessage startMessage)
          { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
        return NoOp
      AddItem item -> (mconv, addItem item model) <# do
        reply (toReplyMessage "Ok, got it!")
          { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
        return NoOp
      RemoveItem item -> (mconv, removeItem item model) <# do
        replyText "Item removed!"
        return NoOp
      SwitchToList name -> (mconv, model { currentList = name }) <# do
        replyText ("Switched to list «" <> name <> "»!")
        return NoOp
      ShowAll -> fullModel <# do
        reply (toReplyMessage "Available todo lists")
          { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup listsKeyboard) }
        return NoOp
      Show "" -> fullModel <# do
        return (Show defaultListName)
      Show name -> fullModel <# do
        let items = concat (HashMap.lookup name (todoLists model))
        if null items 
        then reply (toReplyMessage (emptyList name)) 
          { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard)}
        else reply (toReplyMessage (Text.unlines items))
          { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup (InlineKeyboardMarkup [[callbackButton "Refresh" ("Refresh " <> name)]]))}
        return NoOp
      ShowChatId -> fullModel <# do
        replyText $ Text.pack $ show (convChatId conv)
        return NoOp
      CallBack txt -> fullModel <# do
        replyCallbackQuery Nothing
        case Text.words txt of
          ("Refresh":xs) -> do
            let items = concat (HashMap.lookup (head xs) (todoLists model))
            if null items 
              then replyEditMessageText (convMessageId conv) (emptyList (head xs)) (Just (SomeReplyKeyboardMarkup startKeyboard))
              else replyEditMessageText (convMessageId conv) (Text.unlines items) (
                Just (SomeInlineKeyboardMarkup (InlineKeyboardMarkup [[callbackButton "Refresh" ("Refresh " <> (head xs))]])))
            return NoOp
          _ -> return (Show txt)
      where
        Just conv = mconv
        emptyList n = "The list «" <> n <> "» is empty. Maybe try these starter options?"
        listsKeyboard = InlineKeyboardMarkup
          (map (\name -> [callbackButton name name]) (HashMap.keys (todoLists model)))

    startMessage = Text.unlines
      [ "Hello! I am your personal TODO bot :)"
      , ""
      , "You can add new items to your todo list just by typing it!"
      , "You can also use /add command to do that explicitely."
      , "To remove an item use /remove command."
      , ""
      , "You can manage multiple todo lists:"
      , "Switch to a new named list with /switch_to_list <list>."
      , "Show all available lists with /show_all."
      , "Show items for a specific list with /show <list>."
      , ""
      , "Here are some starter options, try adding something to the list."
      ]

    startKeyboard :: ReplyKeyboardMarkup
    startKeyboard = ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard =
          [ [ "Buy milk", "Get job done" ]
          , [ "Build a house", "Plant a tree" ]
          ]
      , replyKeyboardMarkupResizeKeyboard = Just True
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      }

addItem :: Item -> Model -> Model
addItem item model = model
  { todoLists = HashMap.insertWith (++) (currentList model) [item] (todoLists model) }

removeItem :: Item -> Model -> Model
removeItem item model = model
  { todoLists = HashMap.adjust (filter (/= item)) (currentList model) (todoLists model)}

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateConversations todoBot3) env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token
  return ()
