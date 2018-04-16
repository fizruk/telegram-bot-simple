{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Control.Applicative
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics (Generic)

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Telegram.Bot.Simple.ReplyDocuments

type Item = Text

data Model = Model
  { todoLists   :: HashMap Text [Item]
  , currentList :: Text
  }

data Conversation = Conversation
 { convChatId    :: ChatId
 , convUserId    :: Maybe UserId
 , convMessageId :: MessageId
 }
 deriving (Eq, Show, Generic, Hashable)

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
  | GetFile Text
  | Edit (MessageId, Text)
  | CallBack Text
  deriving (Show, Read)

testCallbackQuery :: UpdateParser CallbackQuery
testCallbackQuery = UpdateParser (\x -> updateCallbackQuery x)

mkParserUpdate :: (Update -> Maybe Update) -> UpdateParser Update
mkParserUpdate = UpdateParser

callbackQueryDataReadAction :: UpdateParser Text
callbackQueryDataReadAction =  mkParser $ \update -> do
  query <- updateCallbackQuery update
  data_ <- callbackQueryData query
  pure data_

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
       <|> CallBack     <$> callbackQueryDataReadAction
       <|> Start        <$  command "start"
       <|> AddItem      <$> command "add"
       <|> RemoveItem   <$> command "remove"
       <|> SwitchToList <$> command "switch_to_list"
       <|> Show         <$> command "show"
       <|> ShowAll      <$  command "show_all"
       <|> ShowChatId   <$  command "chat_id"
       <|> GetFile      <$> command "file"
       <|> Edit         <$> editCommand "test_edit"

    handleAction :: Action -> (Maybe Conversation, Model) -> Eff Action (Maybe Conversation, Model)
    handleAction action fullModel = case action of
      NoOp -> pure fullModel
      Start -> fullModel <# do
        reply (toReplyMessage startMessage)
          { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
        return NoOp
      AddItem item -> (mconv, addItem item model) <# do
        reply $ (toReplyMessage "Ok, got it!") 
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
          then reply (toReplyMessage ("The list «" <> name <> "» is empty. Maybe try these starter options?"))
                 { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
          else replyText (Text.unlines items)
        return NoOp
      ShowChatId -> fullModel <# do
        replyText $ case mconv of
          Just conv -> Text.pack $ show (convChatId conv)
          Nothing -> "No chat id!"
        return NoOp
      GetFile path -> fullModel <# do
        replyReplyDocument $ (toReplyDocument (Text.unpack path))
          { replyDocumentCaption = Just "Отчёт"
          , replyDocumentReplyMarkup = Just (SomeInlineKeyboardMarkup inlineStartKeyboard)
          }
        return NoOp
      Edit (mId, _) -> fullModel <# do
        replyEditMessageText mId "Editted message"
        return NoOp
      CallBack _ -> fullModel <# do
        replyCallbackQuery (Just "CB")
        case mconv of
          Just conv -> replyEditMessageText (convMessageId conv) "I edited!!!"
          Nothing   -> replyText "No conversation!"
        return NoOp
      where
        (mconv, model) = fullModel
        listsKeyboard = InlineKeyboardMarkup
          (map (\name -> [callbackButton name "ShowAll"]) (HashMap.keys (todoLists model)))

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

    inlineStartKeyboard :: InlineKeyboardMarkup
    inlineStartKeyboard = InlineKeyboardMarkup
      { inlineKeyboardMarkupInlineKeyboard =
          [ map (\name -> callbackButton name ("Test buttons")) ["Show Statistic"
            ,"Get report"
            ]
          , map (\name -> actionButton name Start) ["Settings"
            ,"Help"
            ]
          ]
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
