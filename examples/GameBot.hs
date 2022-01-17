{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isJust)
import Data.Text.Encoding (encodeUtf8)
import Dhall hiding (maybe)
import Network.HTTP.Types (hLocation)
import Network.Wai.Handler.Warp (run)
import Options.Applicative hiding (command, action)
import Prettyprinter.Internal   (pretty)
import Servant
import Servant.HTML.Blaze
import System.Random (randomIO)
import Test.QuickCheck (generate, shuffle)
import Text.Blaze.Html
import Web.Cookie

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID
import qualified Options.Applicative as Optparse (command)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
  env <- loadEnv serverSettings
  run port (serverApp env)

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
     :<|> "game"  :>
        (    Get '[HTML] (WithCookie Html)
        :<|> ReqBody '[JSON] Int :> Post '[HTML] (WithCookie Html)
        )
     )

api :: Proxy API
api = Proxy

server :: Env -> Server API
server env = \cookie ->
  (    startHandler env cookie
  :<|> (firstQuestionHandler env cookie :<|> nextQuestionHandler env cookie)
  )

serverApp :: Env -> Application
serverApp env = serve api (server env)

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

shuffleQuestions :: Int -> HashSet Question -> IO [Question]
shuffleQuestions limit questions
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

createUser :: Handler GameUserId
createUser = liftIO (GameUserId . UUID.toText <$> randomIO)

getOrCreateUser :: Maybe Text -> Handler GameUserId
getOrCreateUser Nothing       = createUser
getOrCreateUser (Just cookie) =
  case parseUser cookie of
    Nothing   -> createUser
    Just user -> pure user

parseUser :: Text -> Maybe GameUserId
parseUser = fmap GameUserId
  . HashMap.lookup "HUID"
  . HashMap.fromList
  . fmap (fmap (Text.drop 1) . Text.span (/= '='))
  . Text.splitOn ";"
  . Text.filter (not . isSpace)

userToSetCookie :: GameUserId -> SetCookie
userToSetCookie user = defaultSetCookie
  { setCookieName = "HUID"
  , setCookieValue = encodeUtf8 (coerce user)
  }

findUserData :: GameUserId -> HashMap GameUserId UserData -> Maybe UserData
findUserData = HashMap.lookup

initUserData :: [Question] -> Maybe UserData
initUserData [] = Nothing
initUserData total@(q : qs) = Just $ UserData
  { userDataCurrentQuestion = Just q
  , userDataQuestions       = qs
  , userDataAnswers         = []
  , userDataTotalQuestions  = fromIntegral $ length total
  }

alterUserData :: Int -> UserData -> Maybe UserData
alterUserData userAnswer old = case userDataQuestions old of
  []     -> Nothing
  q : qs ->
    Just $ old
      { userDataCurrentQuestion = Just q
      , userDataQuestions       = qs
      , userDataAnswers         =
          registerAnswer userAnswer (userDataCurrentQuestion old) (userDataAnswers old)
      }

data GameState = GameOver UserData | GameNotFound | GameInProgress UserData
  deriving Eq

-- *** Analytics

data Analytics = Analytics
  { rootPageCounter         :: Integer
  , nextQuestionPageCounter :: Integer
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

incrementRootPage :: Analytics -> Analytics
incrementRootPage a = a { rootPageCounter = 1 + rootPageCounter a }

incrementNextQuestionPage :: Analytics -> Analytics
incrementNextQuestionPage a = a { nextQuestionPageCounter = 1 + nextQuestionPageCounter a }

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

withUser
  :: Maybe Text
  -> (GameUserId -> Handler (WithCookie Html))
  -> Handler (WithCookie Html)
withUser Nothing _action = redirectToRoot
withUser (Just cookie) action = maybe redirectToRoot action (parseUser cookie)

startHandler :: Env -> Maybe Text -> Handler (WithCookie Html)
startHandler _env mCookie = do
  user <- getOrCreateUser mCookie
  pure
    $ addHeader @"Set-Cookie" (userToSetCookie user)
    $ renderStartPage

firstQuestionHandler :: Env -> Maybe Text -> Handler (WithCookie Html)
firstQuestionHandler env mCookie = withUser mCookie (firstQuestionForUser env)
  where
    ServerSettings{..} = settings env
    limit = fromIntegral questionsPerGame

    firstQuestionForUser Env{..} user = do
      mNewUserData <- liftIO $ do
        newQuestions <- shuffleQuestions limit =<< readTVarIO questionsState
        atomically $ do
          modifyTVar' analytics incrementNextQuestionPage
          let newUserData = initUserData newQuestions
          case newUserData of
            Nothing       -> pure ()
            Just userData -> modifyTVar' userState $! HashMap.insert user userData
          pure newUserData
      case mNewUserData of
        Nothing -> redirectToRoot
        Just newUserData -> pure
          $ addHeader @"Set-Cookie" (userToSetCookie user)
          $ renderQuestionPage newUserData

nextQuestionHandler :: Env -> Maybe Text -> Int -> Handler (WithCookie Html)
nextQuestionHandler env mCookie n = withUser mCookie (nextQuestionForUser env n)
  where
    nextQuestionForUser Env{..} numAnswer user = do
      newGameState <- liftIO $ atomically $ do
        modifyTVar' analytics incrementNextQuestionPage
        oldUserState <- readTVar userState
        case findUserData user oldUserState of
          Nothing -> pure GameNotFound
          Just oldUserData -> case alterUserData numAnswer oldUserData of
            Nothing -> do
              writeTVar userState $! HashMap.delete user oldUserState
              pure (GameOver oldUserData)
            Just newUserData -> do
              writeTVar userState $! HashMap.insert user newUserData oldUserState
              pure (GameInProgress newUserData)
      case newGameState of
        GameNotFound -> redirectToStart user
        GameOver oldUserData -> pure
          $ addHeader @"Set-Cookie" (userToSetCookie user)
          $ renderUserScore oldUserData
        GameInProgress newUserData -> pure
          $ addHeader @"Set-Cookie" (userToSetCookie user)
          $ renderQuestionPage newUserData

-- *** Redirects

redirectToRoot :: Handler (WithCookie Html)
redirectToRoot = noHeader @"Set-Cookie" <$> throwError (err301WithLoc "/")

redirectToStart :: GameUserId -> Handler (WithCookie Html)
redirectToStart user
  =   addHeader @"Set-Cookie" (userToSetCookie user)
  <$> throwError (err301WithLoc "/game")

err301WithLoc :: ByteString -> ServerError
err301WithLoc loc = err301 { errHeaders = [(hLocation, loc)] }

-- *** Renderers

withGameTemplate :: Html -> Html
withGameTemplate content = toHtml $ H.html $ do
  H.head $ do
    H.title $ "Game"
  H.body $ content

renderStartPage :: Html
renderStartPage = withGameTemplate $ do
  H.div $ "Game"
  H.form ! A.action "/game" ! A.method "get" $ do
    H.button ! A.type_ "submit" $ "Play"

renderQuestionPage :: UserData -> Html
renderQuestionPage UserData{..} = withGameTemplate $ do
  case userDataCurrentQuestion of
    Nothing -> do
      H.div $ "No more questions left."
      H.form ! A.action "/game" ! A.method "get" $ do
        H.button ! A.type_ "submit" $ "Play again"
    Just QuestionBool{..} -> do
      H.pre $ toMarkup questionBoolText
      H.br
      H.div $ toMarkup
        $ show (length userDataAnswers + 1) <> "/" <> show userDataTotalQuestions
      H.form ! A.action "/game" ! A.method "post" $ do
        H.div $ do
          H.label ! A.for "f" $ "False"
          H.input ! A.id "f" ! A.type_ "radio" ! A.name "q" ! A.value "0" 
        H.div $ do
          H.label ! A.for "f" $ "True"
          H.input ! A.id "f" ! A.type_ "radio" ! A.name "q" ! A.value "1" 
        H.div $ do
          H.button ! A.type_ "submit" $ "Next question"
    Just QuestionChoice{..} -> do
      H.pre $ toMarkup questionChoiceText
      H.br
      H.div $ toMarkup
        $ show (length userDataAnswers + 1) <> "/" <> show userDataTotalQuestions
      H.form ! A.action "/game" ! A.method "post" $ do
        forM_ questionChoiceChoices $
          \Choice{..} -> H.div $ do
            let choiceId = H.toValue (show choiceNumber)
            H.label ! A.for choiceId $ toMarkup choiceText
            H.input ! A.id choiceId ! A.type_ "radio" ! A.name "q" ! A.value choiceId
        H.div $ do
          H.button ! A.type_ "submit" $ "Next question"      

renderUserScore = e

-- *** Helpers

e :: a
e = error "TBD"
