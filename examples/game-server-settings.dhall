{ serverPort = env:HASKELL_GAME_SERVER_PORT
, serverUrlPrefix = env:HASKELL_GAME_URL as Text
, questionsPerGame = 10
, usersPath = "./examples/game/users.dhall"
, questionsPath = "./examples/game/questions.dhall"
, analyticsPath = "./examples/game/analytics.dhall"
}
