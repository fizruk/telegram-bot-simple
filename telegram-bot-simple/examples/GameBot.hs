{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (void, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isJust)
import Data.Text.Encoding (encodeUtf8)
import Dhall hiding (maybe, void)
import Network.HTTP.Types (hLocation)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setInstallShutdownHandler, setPort)
import Options.Applicative hiding (command, action)
import Prettyprinter.Internal   (pretty)
import Servant
import Servant.HTML.Blaze
import System.Random (randomIO)
import Test.QuickCheck (generate, shuffle)
import Text.Blaze.Html
import Web.Internal.FormUrlEncoded (ToForm, FromForm)
import Web.Cookie

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID
import qualified Options.Applicative as Optparse (command)
import qualified System.Signal as Sig
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Telegram.Bot.API
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

gameBot :: BotSettings -> BotApp Model Action
gameBot settings = BotApp
  { botInitialModel = ()
  , botAction = flip (updateToAction settings)
  , botHandler = handleAction settings
  , botJobs = []
  }

updateToAction :: BotSettings -> Model -> Update -> Maybe Action
updateToAction BotSettings{..}  _ update
  | isJust $ parseUpdate (command "game") update = game update
  | isJust $ parseUpdate (command "feedback") update = do
      msg <- updateMessage update
      let msgId = messageMessageId msg
          chat  = SomeChatId $ chatId $ messageChat msg
      pure $ AFeedback chat msgId
  | isJust $ updateInlineQuery update = do
      query <- updateInlineQuery update
      let queryId = inlineQueryId query
          msg     = inlineQueryQuery query
      Just $ AInlineGame queryId msg
  | isJust $ updateCallbackQuery update = ACallback <$> updateCallbackQuery update
  | otherwise = Nothing
  where
    game upd = AGame <$> (updateChatId upd) <*> (Just gameUrl)

handleAction :: BotSettings -> Action -> Model -> Eff Action Model
handleAction BotSettings{..} action model = case action of
  NoOp -> pure model
  AFeedback sourceChatId msgId -> model <# do
    let shouldNotify  = Just True
        targetChatId  = SomeChatId (ChatId (fromIntegral supportChatId))
        fwdMsgRequest = ForwardMessageRequest targetChatId Nothing sourceChatId shouldNotify Nothing msgId
    _ <- runTG fwdMsgRequest
    return ()

  AInlineGame queryId msg -> model <# do
    let inlineQueryResult =
          InlineQueryResult
            InlineQueryResultGame
            (InlineQueryResultId msg)
            (Just msg)
            (Just gameMsg)
            Nothing
        gameMsg = (defaultInputTextMessageContent gameMessageText) { inputMessageContentParseMode = Just "HTML" }
        answerInlineQueryRequest = defAnswerInlineQuery  queryId [inlineQueryResult]

    _ <- runTG answerInlineQueryRequest
    return ()
  AGame targetChatId _msg -> model <# do
    let sendGameRequest = defSendGame (coerce targetChatId) gameId
    waitAndRetry sendGameRequest
    return ()
  ACallback callback -> model <# do
    let queryId = coerce (callbackQueryId callback)
        queryData = callbackQueryData callback
        answerCallbackQueryRequest = (defAnswerCallbackQuery queryId)
          { answerCallbackQueryText            = queryData
          , answerCallbackQueryUrl             = Just gameUrl
          }
    waitAndRetry answerCallbackQueryRequest
    return ()

  where
    gameMessageText = "<a href=\"" <> gameUrl <> "\">" <> gameName <> "</a>"

data Command = CmdBot | CmdServer

waitAndRetry :: RunTG a (Response b) => a -> BotM ()
waitAndRetry request = do
  result <- runTG request
  if responseOk result
    then pure ()
    else case responseParameters result >>= responseParametersRetryAfter of
      Nothing -> pure ()
      Just sec -> do
        liftIO . threadDelay $ coerce sec * 1000000
        _ <- runTG request
        return ()

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
  startBot_ (conversationBot updateChatId (gameBot botSettings)) env

data BotSettings = BotSettings
  { botToken      :: Text
  , gameUrl       :: Text
  , gameId        :: Text
  , gameName      :: Text
  , supportChatId :: Integer
  }
  deriving (Generic, FromDhall)

loadBotSettings :: IO BotSettings
loadBotSettings = input Dhall.auto "./examples/game-bot-settings.dhall"

-- * Server

runServer :: IO ()
runServer = do
  serverSettings <- loadServerSettings
  env <- loadEnv serverSettings
  let shutdownHandler closeSocket = void $ Sig.installHandler Sig.sigTERM handler
        where
          shutdownAction = storeEnv env
          handler _sig = shutdownAction >> closeSocket
      warpSettings = defaultSettings
        & setPort port
        & setInstallShutdownHandler shutdownHandler
      port = fromIntegral (serverPort serverSettings)
  runSettings warpSettings (serverApp env)

data PictureSettings = PictureSettings
  { theGood :: Text
  , theBad  :: Text
  , theUgly :: Text
  , goodScore :: Double
  , badScore  :: Double
  } deriving (Generic, FromDhall)

data ServerSettings = ServerSettings
  { serverPort       :: Natural
  , serverUrlPrefix  :: Text
  , questionsPerGame :: Natural
  , usersPath        :: Text
  , questionsPath    :: Text
  , analyticsPath    :: Text
  , pageStyle        :: Text
  , quizDescription  :: Text
  , quizFooterBegin  :: Text
  , quizFooterCode   :: Text
  , quizFooterEnd    :: Text
  , quizPicture      :: Maybe PictureSettings
  } deriving (Generic, FromDhall)

serverSettingsPath :: Text
serverSettingsPath = "./examples/game-server-settings.dhall"

loadServerSettings :: IO ServerSettings
loadServerSettings = input Dhall.auto serverSettingsPath

data AnswerInt = AnswerInt { q :: Int } deriving (Eq, Show, Generic)

instance ToForm AnswerInt

instance FromForm AnswerInt

type WithCookie x = Headers '[ Header "Set-Cookie" SetCookie ] x

type API
  =  Header "Cookie" Text
  :> (     Get '[HTML] (WithCookie Html)
     :<|> "game"  :>
        (    Get '[HTML] (WithCookie Html)
        :<|> ReqBody' '[Required, Strict] '[FormUrlEncoded] AnswerInt :> Post '[HTML] (WithCookie Html)
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

questionText :: Question -> Text
questionText (QuestionBool txt _ _)  = txt
questionText (QuestionChoice txt _ _) = txt

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
    checkAnswerConsistency = (== 1) . length . filter choiceIsCorrect
    checkIdConsistency x = (HashSet.size . HashSet.fromList . fmap choiceNumber) x == length x
    checkConsistency x = checkIdConsistency x && checkAnswerConsistency x

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

alterUserData :: Int -> UserData -> GameState
alterUserData userAnswer old = case (userDataCurrentQuestion old, userDataQuestions old) of
  (Nothing, []) -> GameNotFound
  (Just q, [])  -> GameOver $ old
    { userDataCurrentQuestion = Just q
    , userDataQuestions       = []
    , userDataAnswers         =
        registerAnswer userAnswer (userDataCurrentQuestion old) (userDataAnswers old)
    }
  (_, q : qs) -> GameInProgress $ old
    { userDataCurrentQuestion = Just q
    , userDataQuestions       = qs
    , userDataAnswers         =
        registerAnswer userAnswer (userDataCurrentQuestion old) (userDataAnswers old)
    }

gameDataFromState :: GameState -> Maybe UserData
gameDataFromState = \case
  GameNotFound -> Nothing
  GameOver game -> Just game
  GameInProgress game -> Just game

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
  :: ServerSettings
  -> Maybe Text
  -> (GameUserId -> Handler (WithCookie Html))
  -> Handler (WithCookie Html)
withUser settings Nothing _action = redirectToRoot settings
withUser settings (Just cookie) action
  = maybe (redirectToRoot settings) action (parseUser cookie)

startHandler :: Env -> Maybe Text -> Handler (WithCookie Html)
startHandler Env{..} mCookie = do
  user <- getOrCreateUser mCookie
  pure
    $ addHeader @"Set-Cookie" (userToSetCookie user)
    $ renderStartPage settings

firstQuestionHandler :: Env -> Maybe Text -> Handler (WithCookie Html)
firstQuestionHandler env mCookie = withUser (settings env) mCookie (firstQuestionForUser env)
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
        Nothing -> redirectToRoot settings
        Just newUserData -> pure
          $ addHeader @"Set-Cookie" (userToSetCookie user)
          $ renderQuestionPage settings newUserData

nextQuestionHandler :: Env -> Maybe Text -> AnswerInt -> Handler (WithCookie Html)
nextQuestionHandler env mCookie (AnswerInt answer)
  = withUser (settings env) mCookie (nextQuestionForUser env answer)
  where
    nextQuestionForUser Env{..} numAnswer user = do
      newGameState <- liftIO $ atomically $ do
        modifyTVar' analytics incrementNextQuestionPage
        oldUserState <- readTVar userState
        case findUserData user oldUserState of
          Nothing -> pure GameNotFound
          Just oldUserData -> do
            let newUserState = alterUserData numAnswer oldUserData
            case gameDataFromState newUserState of
              Nothing -> writeTVar userState $! HashMap.delete user oldUserState
              Just newUserData ->
                writeTVar userState $! HashMap.insert user newUserData oldUserState
            pure newUserState
      case newGameState of
        GameNotFound -> redirectToStart settings user
        GameOver oldUserData -> pure
          $ addHeader @"Set-Cookie" (userToSetCookie user)
          $ renderUserScore settings oldUserData
        GameInProgress newUserData -> pure
          $ addHeader @"Set-Cookie" (userToSetCookie user)
          $ renderQuestionPage settings newUserData

-- *** Redirects

redirectToRoot :: ServerSettings -> Handler (WithCookie Html)
redirectToRoot settings
  = noHeader @"Set-Cookie"
  <$> throwError (err301WithLoc $ encodeUtf8 $ makeAbsoluteUrl settings "/")

redirectToStart :: ServerSettings -> GameUserId -> Handler (WithCookie Html)
redirectToStart settings user
  =   addHeader @"Set-Cookie" (userToSetCookie user)
  <$> throwError (err301WithLoc $ encodeUtf8 $ makeAbsoluteUrl settings "/game/")

err301WithLoc :: ByteString -> ServerError
err301WithLoc loc = err301 { errHeaders = [(hLocation, loc)] }

-- *** Renderers

makeAbsoluteUrl :: ServerSettings -> Text -> Text
makeAbsoluteUrl ServerSettings{..} uri = serverUrlPrefix <> uri

makeAbsoluteRootUrl :: ServerSettings -> Text
makeAbsoluteRootUrl = flip makeAbsoluteUrl "/"

makeAbsoluteGameUrl :: ServerSettings -> Text
makeAbsoluteGameUrl = flip makeAbsoluteUrl "/game/"

withGameTemplate :: ServerSettings -> Html -> Html
withGameTemplate s@ServerSettings{..} content = toHtml $ H.html $ do
  H.head $ do
    H.title $ "Game"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.link
      ! A.href (toValue $ makeAbsoluteUrl s "/fonts/HalvarBreitschriftDEMO-Regular.woff")
      ! A.rel "stylesheet"
    H.link
      ! A.href (toValue $ makeAbsoluteUrl s "/fonts/HalvarBreitschriftDEMO-Bold.woff")
      ! A.rel "stylesheet"
    H.style $ toMarkup pageStyle
  H.body $ content

-- **** Home page

renderTextLogo1 :: Text -> Html
renderTextLogo1 txt =
  H.div ! A.class_ "qbox pad" $ do
    H.div ! A.class_ "text-logo-line1 position-logo-line1" $ toMarkup txt

renderTextLogo2 :: Text -> Html
renderTextLogo2 txt =
  H.div ! A.class_ "qbox pad" $ do
    H.div ! A.class_ "text-logo-line2 position-logo-line2" $ toMarkup txt

renderDescription :: Text -> Html
renderDescription txt= do
  H.div ! A.class_ "qbox pad" $ do
    H.div ! A.class_ "text-description position-text" $ toMarkup txt

renderFooterText :: Text -> Html
renderFooterText txt= do
  H.div ! A.class_ "qbox pad" $ do
    H.div ! A.class_ "text-footer position-text" $ toMarkup txt

renderFooterCode :: Text -> Html
renderFooterCode txt= do
  H.div ! A.class_ "qbox pad" $ do
    H.div ! A.class_ "text-footer-code position-text" $ toMarkup txt

renderStartPage :: ServerSettings -> Html
renderStartPage settings = withGameTemplate settings $ do
  renderTextLogo1 "Haskell"
  renderTextLogo2 "Quiz Game"
  renderDescription (quizDescription settings)
  renderLink settings "Play"
  renderFooterText (quizFooterBegin settings)
  renderFooterCode (quizFooterCode settings)
  renderFooterText (quizFooterEnd settings)

-- **** Question page

renderProgress :: Bool -> Int -> Integer -> Html
renderProgress isResult current total =
  H.div ! A.class_ "qbox pad" $ do
    let currentClass = if isResult then "text-current-result" else "text-current-number"
        positionClass = if isResult then "position-progress-result" else "position-progress"
    H.div ! A.class_ positionClass $ do
      H.span ! A.class_ currentClass $ toMarkup (show current)
      H.span ! A.class_ "text-total-number" $ toMarkup $ "/ " <> show total

renderQuestionText :: Bool -> Text -> Html
renderQuestionText forExplanation txt =
  H.div ! A.class_ "qbox pad" $ do
    let positionStyle = if forExplanation then "position-explanation" else "position-question"
        questionStyle = "text-question " <> positionStyle :: Text
    H.div ! A.class_ (toValue questionStyle) $ toMarkup txt

renderAnswer :: Bool -> Int -> Text -> Html
renderAnswer ch num txt =
  H.div ! A.class_ "qbox position-answer" $ do
    H.label ! A.class_ "container" ! A.for (toValue num) $ do
      H.input
        ! A.id (toValue num)
        ! A.type_ "radio"
        ! A.name "q"
        !? (ch, A.checked "")
        ! A.value (toValue num)
      H.div ! A.class_ "checkmark" $ ""
      H.div ! A.class_ "text-answer position-text-answer" $ toMarkup txt

renderQuestionPage :: ServerSettings -> UserData -> Html
renderQuestionPage settings UserData{..} = withGameTemplate settings $ do
  case userDataCurrentQuestion of
    Nothing -> do
      renderQuestionText False "No more questions left."
      renderAgainLink settings "Play again"

    Just QuestionBool{..} -> do
      renderProgress False (length userDataAnswers + 1) userDataTotalQuestions
      renderQuestionText False questionBoolText
      H.form ! A.action (toValue $ makeAbsoluteGameUrl settings) ! A.method "post" $ do
        renderAnswer True 1 "True"
        renderAnswer False 0 "False"
        renderButton "Next"

    Just QuestionChoice{..} -> do
      renderProgress False (length userDataAnswers + 1) userDataTotalQuestions
      renderQuestionText False questionChoiceText
      H.form ! A.action (toValue $ makeAbsoluteGameUrl settings) ! A.method "post" $ do
        forM_ questionChoiceChoices $
          \Choice{..} -> renderAnswer
            (if choiceNumber == 1 then True else False)
            (fromIntegral choiceNumber)
            choiceText
        renderButton "Next"

-- **** Score page

renderExplanation :: Bool -> Text -> Html
renderExplanation True txt = renderQuestionText True txt
renderExplanation False txt =
  H.div ! A.class_ "qbox pad" $ do
    H.div ! A.class_ "text-wrong-answer position-explanation" $ toMarkup txt

chooseScorePicture :: PictureSettings -> Double -> Text
chooseScorePicture PictureSettings{..} userScore =
  if goodScore < userScore
    then theGood
    else if badScore < userScore
           then theBad
           else theUgly

renderPicture :: ServerSettings -> Int -> Integer -> Html
renderPicture ServerSettings{..} score total = case quizPicture of
  Nothing -> return ()
  Just ps@PictureSettings{..} -> case goodScore < badScore of
    -- invalid settings
    True -> return ()
    False -> do
      let userScore :: Double
          userScore = fromIntegral score / fromIntegral total

          pictureUrl = chooseScorePicture ps userScore
      H.div ! A.class_ "qbox pad" $ do
        H.img ! A.src (toValue pictureUrl) ! A.class_ "score-img"

renderBrightLine, renderDarkLine :: Html
renderBrightLine = H.div ! A.class_ "qbox pad" $ do
  H.hr ! A.class_ "bright-line"
renderDarkLine = H.div ! A.class_ "qbox pad" $ do
  H.hr ! A.class_ "dark-line"

renderUserScore :: ServerSettings -> UserData -> Html
renderUserScore settings UserData{..} = withGameTemplate settings $ do
  case userDataAnswers of
    [] -> do
      renderDescription
        "Sorry. Looks like no answers available at the moment. Try again maybe?"
      renderAgainLink settings "Play again"
    _  -> do
      let score = length $ filter answerIsRight userDataAnswers
      renderTextLogo1 "Your"
      renderTextLogo2 "Score is"
      renderProgress True score userDataTotalQuestions
      renderPicture settings score userDataTotalQuestions
      renderBrightLine
      H.div $ do
        forM_ userDataAnswers $ \Answer{..} -> H.tr $ do
          H.div $ renderQuestionText True (questionText answerQuestion)
          H.div $ if answerIsRight
            then renderExplanation True "OK"
            else renderExplanation False $ explainError answerQuestion
          renderDarkLine
      renderAgainLink settings "Play again"

-- **** Helpers

renderButton :: Text -> Html
renderButton txt =
  H.div ! A.class_ "qbox pad" $ do
    H.div ! A.class_ "position-button" $ do
      H.input
        ! A.class_ "qel-button text-button"
        ! A.type_ "submit"
        ! A.value (toValue txt)

renderLink :: ServerSettings -> Text -> Html
renderLink settings txt = do
  H.a ! A.href (toValue $ makeAbsoluteGameUrl settings) $ do
    H.div ! A.class_ "qbox pad" $ do
      H.div ! A.class_ "position-link" $ do
        H.div ! A.class_ "qel-button text-button text-play-button"
          $ toMarkup txt

renderAgainLink :: ServerSettings -> Text -> Html
renderAgainLink settings txt = do
  H.a ! A.href (toValue $ makeAbsoluteGameUrl settings) $ do
    H.div ! A.class_ "qbox pad" $ do
      H.div ! A.class_ "position-link" $ do
        H.div ! A.class_ "qel-button text-button text-again-button"
          $ toMarkup txt

