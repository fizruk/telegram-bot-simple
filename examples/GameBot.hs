{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.STM
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isJust)
import Dhall
import Network.Wai.Handler.Warp (run)
import Options.Applicative hiding (command, action)
import Prettyprinter.Internal   (pretty)
import Servant
import Servant.HTML.Blaze
import Test.QuickCheck (generate, shuffle)
import Text.Blaze.Html
import Web.Cookie

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Options.Applicative as Optparse (command)

import Telegram.Bot.API
import Telegram.Bot.API.Games
import Telegram.Bot.API.InlineMode.InlineQueryResult
import Telegram.Bot.API.InlineMode.InputMessageContent
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Model = ()

data Action
  = NoOp
  | AGame ChatId Text
  | AInlineGame InlineQueryId Text
  | AFeedback SomeChatId MessageId
  | ACallback CallbackQuery

gameBot :: BotApp Model Action
gameBot = BotApp
  { botInitialModel = ()
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction  _ update
  | isJust $ parseUpdate (command "game") update = game update
  | isJust $ parseUpdate (command "feedback") update = do
      msg <- updateMessage update
      let msgId = messageMessageId msg
          chat  = SomeChatId $ chatId $ messageChat msg
      pure $ AFeedback chat msgId
  | isJust $ updateMessageText update = game update
  | isJust $ updateInlineQuery update = do
      query <- updateInlineQuery update
      let queryId = inlineQueryId query
          msg     = inlineQueryQuery query
      Just $ AInlineGame queryId msg
  | isJust $ updateCallbackQuery update = ACallback <$> updateCallbackQuery update
  | otherwise = Nothing
  where
    game upd = AGame <$> (updateChatId upd) <*> (Just "http://localhost:8080/game")

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoOp -> pure model
  AFeedback sourceChatId msgId -> model <# do
    let shouldNotify  = Just True
        targetChatId  = SomeChatId (ChatId (-711299241))
        fwdMsgRequest = ForwardMessageRequest targetChatId sourceChatId shouldNotify msgId
    _ <- liftClientM (forwardMessage fwdMsgRequest)
    return ()

  AInlineGame queryId msg -> model <# do
    let inlineQueryResult =
          InlineQueryResult
            InlineQueryResultGame
            (InlineQueryResultId msg)
            (Just msg)
            (Just gameMsg)
        gameMsg = (defaultInputTextMessageContent gameMessageText) { inputMessageContentParseMode = Just "HTML" }
        answerInlineQueryRequest = AnswerInlineQueryRequest queryId [inlineQueryResult]

    _ <- liftClientM (answerInlineQuery answerInlineQueryRequest)
    return ()
  AGame targetChatId msg -> model <# do
    let sendGameRequest = SendGameRequest
          { sendGameRequestChatId                   = coerce targetChatId
          , sendGameRequestGameShortName            = "samplegame"
          , sendGameRequestDisableNotification      = Nothing
          , sendGameRequestReplyToMessageId         = Nothing
          , sendGameRequestAllowSendingWithoutReply = Nothing
          , sendGameRequestReplyMarkup              = Nothing
          }
    _ <- liftClientM $ sendGame sendGameRequest
    return ()
  ACallback callback -> model <# do
    let queryId = coerce (callbackQueryId callback)
        queryData = callbackQueryData callback
        answerCallbackQueryRequest = AnswerCallbackQueryRequest
          { answerCallbackQueryRequestCallbackQueryId = queryId
          , answerCallbackQueryRequestText            = queryData
          , answerCallbackQueryRequestShowAlert       = Nothing
          , answerCallbackQueryRequestUrl             = Just "http://localhost:8080/game"
          , answerCallbackQueryRequestCacheTime       = Nothing
          }
    _ <- liftClientM $ answerCallbackQuery answerCallbackQueryRequest
    return ()
    
  where
    gameMessageText = "<a href=\"http://localhost:8080/game\">Haskell Quiz</a>"
data Command = CmdBot | CmdServer

-- * Main

main :: IO ()
main = execParser (info (commands <**> helper) idm) >>= \case
  CmdBot -> runTelegramBot
  CmdServer -> runServer

commands :: Parser Command
commands = subparser
    (Optparse.command "bot"    (info botOpts    (progDesc botDesc)) <>
     Optparse.command "server" (info serverOpts (progDesc serverDesc)))
  where
    botDesc = "Run Telegram Game Bot"
    serverDesc = "Run HTML5 Game Server"

    botOpts = pure CmdBot
    serverOpts = pure CmdServer

-- * Bot

runTelegramBot :: IO ()
runTelegramBot = do
  botSettings <- loadBotSettings
  let token = Token (botToken botSettings)
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId gameBot) env

data BotSettings = BotSettings
  { botToken :: Text
  , gameUrl  :: Text
  }
  deriving (Generic, FromDhall)

loadBotSettings :: IO BotSettings
loadBotSettings = input Dhall.auto "./examples/game-bot-settings.dhall"

-- * Server

runServer :: IO ()
runServer = do
  serverSettings <- loadServerSettings
  let port = fromIntegral (serverPort serverSettings)
  run port (serverApp serverSettings)

data ServerSettings = ServerSettings
  { serverPort       :: Natural
  , questionsPerGame :: Natural
  , usersPath        :: Text
  , questionsPath    :: Text
  , analyticsPath    :: Text
  } deriving (Generic, FromDhall)

serverSettingsPath :: Text
serverSettingsPath = "./examples/game-server-settings.dhall"

loadServerSettings :: IO ServerSettings
loadServerSettings = input Dhall.auto serverSettingsPath

type WithCookie x = Headers '[ Header "Set-Cookie" SetCookie ] x

type API
  =  Header "Cookie" Text
  :> (     Get '[HTML] (WithCookie Html)
     :<|> "next"  :> ReqBody '[JSON] Int :> Post '[HTML] (WithCookie Html)
     :<|> "score" :> Get '[HTML] (WithCookie Html)
     )

api :: Proxy API
api = Proxy

server :: ServerSettings -> Server API
server settings = \cookie ->
  (    startHandler settings cookie
  :<|> nextQuestionHandler settings cookie
  :<|> getScoreHandler settings cookie
  )

-- *** Questions

data Choice = Choice
  { choiceText      :: Text
  , choiceNumber    :: Integer
  , choiceIsCorrect :: Bool
  }
  deriving (Eq, Show, Generic, Hashable, Ord, FromDhall, ToDhall)

data Question
  = QuestionBool
      { questionBoolText         :: Text
      , questionBoolAnswerIsTrue :: Bool
      , questionBoolExplanation  :: Text
      }
  | QuestionChoice
      { questionChoiceText        :: Text
      , questionChoiceChoices     :: [Choice]
      , questionChoiceExplanation :: Text
      }
  deriving (Eq, Show, Generic, Hashable, Ord, FromDhall, ToDhall)

questionExists :: Text -> HashSet Question -> Bool
questionExists questionTxt = not . HashSet.null . HashSet.filter exists
  where
    exists (QuestionBool txt _isTrue _) = txt == questionTxt
    exists (QuestionChoice txt _choices _) = txt == questionTxt

explainError :: Question -> Text
explainError QuestionBool{..} = questionBoolExplanation
explainError QuestionChoice{..} = questionChoiceExplanation

validateQuestion :: Question -> Bool
validateQuestion (QuestionBool _ _ _) = True
validateQuestion (QuestionChoice _ choices _) = checkConsistency choices
  where
    checkConsistency = (== 1) . length . filter choiceIsCorrect

shuffleQuestionsIO :: Int -> HashSet Question -> IO [Question]
shuffleQuestionsIO limit questions
  = generate $ take limit <$> shuffle (HashSet.toList questions)

solveQuestion :: Int -> Question -> Bool
solveQuestion result QuestionBool{..} = intToBool result == questionBoolAnswerIsTrue
  where
    intToBool 0 = False
    intToBool _ = True
solveQuestion result QuestionChoice{..}
  = not . null . filter (byNumber result) $ questionChoiceChoices
  where
    byNumber x Choice{..} = choiceNumber == fromIntegral x

-- ** Answer

data Answer = Answer
  { answerQuestion           :: Question
  , answerIsRight            :: Bool
  , answerExplanationOnError :: Text
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

registerAnswer :: Int -> Maybe Question -> [Answer] -> [Answer]
registerAnswer userAnswer prevQuestion oldAnswers = case prevQuestion of
  Nothing -> oldAnswers
  Just q  -> newAnswer q : oldAnswers
  where
    newAnswer q = Answer q (solveQuestion userAnswer q) (explainError q)

-- *** UserData

newtype GameUserId = GameUserId Text
  deriving (Eq, Show, Generic, Hashable, FromDhall, ToDhall)

data UserData = UserData
  { userDataCurrentQuestion :: Maybe Question
  , userDataQuestions       :: [Question]
  , userDataAnswers         :: [Answer]
  , userDataTotalQuestions  :: Integer
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

findUserData :: GameUserId -> HashMap GameUserId UserData -> Maybe UserData
findUserData = HashMap.lookup

initUserData :: [Question] -> UserData
initUserData [] = UserData Nothing [] [] 0
initUserData total@(q : qs) = UserData
  { userDataCurrentQuestion = Just q
  , userDataQuestions       = qs
  , userDataAnswers         = []
  , userDataTotalQuestions  = fromIntegral $ length total
  }

alterUserData :: UserData -> Int -> Maybe UserData
alterUserData old userAnswer = case userDataQuestions old of
  []     -> Nothing
  q : qs ->
    Just $ old
      { userDataCurrentQuestion = Just q
      , userDataQuestions       = qs
      , userDataAnswers         =
          registerAnswer userAnswer (userDataCurrentQuestion old) (userDataAnswers old)
      }

-- *** Analytics

data Analytics = Analytics
  { rootPageCounter         :: Integer
  , nextQuestionPageCounter :: Integer
  , scorePageCounter        :: Integer
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

incrementRootPage :: Analytics -> Analytics
incrementRootPage a = a { rootPageCounter = 1 + rootPageCounter a }

incrementNextQuestionPage :: Analytics -> Analytics
incrementNextQuestionPage a = a { nextQuestionPageCounter = 1 + nextQuestionPageCounter a }

incrementScorePageCounter :: Analytics -> Analytics
incrementScorePageCounter a = a { scorePageCounter = 1 + scorePageCounter a }

-- *** Env

data Env = Env
  { settings       :: ServerSettings
  , userState      :: TVar (HashMap GameUserId UserData)
  , questionsState :: TVar (HashSet Question)
  , analytics      :: TVar Analytics
  }

loadEnv :: ServerSettings -> IO Env
loadEnv settings@ServerSettings{..} = do
  userState <- newTVarIO =<< loadUserState
  questionsState <- newTVarIO =<< loadQuestionsState
  analytics <- newTVarIO =<< loadAnalytics
  pure Env{..}
  where
    loadUserState      = input Dhall.auto usersPath
    loadQuestionsState = input Dhall.auto questionsPath
    loadAnalytics      = input Dhall.auto analyticsPath

storeEnv :: Env -> IO ()
storeEnv Env{..} = do
  let ServerSettings{..} = settings
  storeState @(HashMap GameUserId UserData) usersPath userState
  storeState @(HashSet Question) questionsPath questionsState
  storeState @Analytics analyticsPath analytics
  where
    storeState :: forall a. ToDhall a => Text -> TVar a -> IO ()
    storeState path state = do
      stateData <- readTVarIO state
      Text.writeFile (Text.unpack path) (renderDhall stateData)
    renderDhall :: forall a. ToDhall a => a -> Text
    renderDhall = Text.pack . show . pretty . Dhall.embed Dhall.inject

-- *** Handlers

startHandler :: ServerSettings -> Maybe Text -> Handler (WithCookie Html)
startHandler = e

nextQuestionHandler :: ServerSettings -> Maybe Text -> Int -> Handler (WithCookie Html)
nextQuestionHandler = e

getScoreHandler :: ServerSettings -> Maybe Text -> Handler (WithCookie Html)
getScoreHandler = e

serverApp :: ServerSettings -> Application
serverApp settings = serve api (server settings)

e :: a
e = error "TBD"
