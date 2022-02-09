{ serverPort = env:HASKELL_GAME_SERVER_PORT
, serverUrlPrefix = env:HASKELL_GAME_URL as Text
, questionsPerGame = 10
, usersPath = "./examples/game/users.dhall"
, questionsPath = "./examples/game/questions.dhall"
, analyticsPath = "./examples/game/analytics.dhall"
, pageStyle =
    ''
      body { background-color: #000000; font-family: Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace; }
      .qbox { margin: auto; text-align: center; width: 34em; }
      .qel  { padding: 1em; border: 0.3em green solid; border-radius: 3em; }
      .wel  { padding: 1em; border: 0.3em red solid; border-radius: 3em; } }
      .pad { padding: 0.3em; }
      .text {
          font-size: 1.25em;
          font-family: Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          color: #2ca32c;
          text-align: left;
          overflow-wrap: break-word; 
          overflow: hidden;
          white-space: pre-wrap;
      }
      @media screen and (max-device-width: 667px) and (orientation: portrait) {
          .text { font-size: 1em; }
          .qbox { width: 95%; }
      }

      .button { background-color: #000000; }
      .button:hover { background-color: green; color: black; }
      
      .container {
          display: block;
          position: relative;
          padding-left: 2em;
          margin-bottom: 0.75em;
          cursor: pointer;
          -webkit-user-select: none;
          -moz-user-select: none;
          -ms-user-select: none;
          user-select: none;
      }
      .container input { position: absolute; opacity: 0; cursor: pointer; }
      .checkmark {
          position: absolute;
          top: 0;
          left: 0;
          height: 1em;
          width: 1em;
          background-color: black;
          border-radius: 50%;
          border: 0.3em green solid;
      }
      .ctext { padding-left: 0.7em; }
      .container:hover input ~ .checkmark { background-color: green; }
      .container input:checked ~ .checkmark { background-color: green; }
      .checkmark:after { content: ""; position: absolute; display: none; }
      .container input:checked ~ .checkmark:after { display: block; }
    ''
, quizDescription =
''
The purpose of this game is to demonstrate the abilities of "GameBot" example from telegram-bot-simple Haskell package. 

Press "Play" to play a game.

Do you like it? Share on a social media!

Do you have ideas for a new question? Please share them via @HaskellTelegramSimpleBot 

    "/feedback <your suggestion>" 

command. Enjoy!
''
}
