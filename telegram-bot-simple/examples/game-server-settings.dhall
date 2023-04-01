{ serverPort = env:HASKELL_GAME_SERVER_PORT
, serverUrlPrefix = env:HASKELL_GAME_URL as Text
, questionsPerGame = 10
, usersPath = "./examples/game/users.dhall"
, questionsPath = "./examples/game/questions.dhall"
, analyticsPath = "./examples/game/analytics.dhall"
, pageStyle =
    ''
      @font-face {
        font-family: "Halvar Breitschrift DEMO";
        src: url("/fonts/HalvarBreitschriftDEMO-Regular.woff") format('woff');
      }

      @font-face {
        font-family: "Halvar Breitschrift DEMO Bold";
        src: url("/fonts/HalvarBreitschriftDEMO-Bold.woff") format('woff');
        font-weight: bold;
      }

      @font-face {
        font-family: "Manrope";
        src: url("/fonts/Manrope-VariableFont_wght.woff") format('woff');
      }

      @font-face {
        font-family: "Larabiefont";
        src: url("/fonts/Larabie_rg.woff") format('woff');
      }

      body { background-color: #000000; font-family: Halvar Breitschrift DEMO,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace; }
      .qbox { margin: auto; text-align: center; width: 50%; }
      /* Responsiveness */
      @media screen and (min-width: 800px; ) { .qbox { width: 50%; } }
      @media screen and (min-width: 600px; ) { .qbox { width: 480px; } }
      @media screen and (min-width: 480px; ) { .qbox { width: 420px; } }
      @media screen and (min-width: 414px; ) { .qbox { width: 360px; } }
      @media screen and (min-width: 320px; ) { .qbox { width: 320px; } }

      /* specifically for button */
      .qel-button { width: 100px; height: 100px; border: 1px solid #5BBF3B; box-sizing: border-box; border-radius: 50%; }
      .qel-button:focus {
          color: #5BBF3B80;
          border: 1px solid rgba(91, 191, 59, 0.5);

      }
      .qel-button:hover {
          color: #5BBF3B80;
          border: 1px solid rgba(91, 191, 59, 0.5);

      }
      .pad { padding: 0.3em; }
      .text-logo-line1 {
        font-family: Halvar Breitschrift DEMO Bold;
        font-size: 33px;
        font-weight: 700;
        line-height: 100%;
        letter-spacing: 0.02em;
        text-align: left;
        text-transform: uppercase;
        -webkit-text-stroke: 1px #5BBF3B;
      }
      .text-logo-line2 {
        font-family: Halvar Breitschrift DEMO Bold,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
        font-size: 33px;
        font-weight: 700;
        line-height: 100%;
        letter-spacing: 0.02em;
        text-align: left;
        text-transform: uppercase;
        color: #5BBF3B;
      }
      .text-description {
          font-family: Manrope,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 14px;
          font-weight: 560;
          line-height: 19px;
          letter-spacing: 0em;
          text-align: left;
          color: #FFFFFF;
      }
      .text-footer {
          font-family: Manrope,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 14px;
          font-weight: 400;
          line-height: 19px;
          letter-spacing: 0em;
          text-align: left;
          color: #FFFFFF;
      }
      .text-footer-code {
          font-family: Larabiefont,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 14px;
          font-weight: 400;
          line-height: 17px;
          letter-spacing: 0em;
          text-align: left;
          color: #5BBF3B;
      }
      .text-current-number {
          font-family: Halvar Breitschrift DEMO Bold,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 40px;
          font-weight: 700;
          line-height: 40px;
          letter-spacing: 0em;
          text-align: left;
          -webkit-text-stroke: 1px #5BBF3B;
          padding-right: 5px;
      }
      .text-current-result {
          font-family: Halvar Breitschrift DEMO Bold,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 40px;
          font-weight: 700;
          line-height: 40px;
          letter-spacing: 0em;
          text-align: left;
          color: #5BBF3B;
          padding-right: 5px;
      }
      .text-total-number {
          font-family: Manrope,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 20px;
          font-weight: 400;
          line-height: 27px;
          letter-spacing: 0em;
          text-align: left;
          color: #FFFFFF;
          opacity: 0.6;
          /* Inside auto layout */
          flex: none;
          order: 1;
          flex-grow: 0;
      }
      .text-question {
          font-family: Larabiefont,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 20px;
          font-weight: 400;
          line-height: 24px;
          letter-spacing: 0em;
          text-align: left;
          color: #5BBF3B;
          /* Inside auto layout */
          flex: none;
          order: 0;
          flex-grow: 0;
          white-space: pre-wrap;
          overflow-wrap: break-word;
      }
      .text-wrong-answer {
          font-family: Larabiefont,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 20px;
          font-weight: 400;
          line-height: 24px;
          letter-spacing: 0em;
          text-align: left;
          color: #9E0A36;
          /* Inside auto layout */
          flex: none;
          order: 0;
          flex-grow: 0;
          white-space: pre-wrap;
          overflow-wrap: break-word;
      }
      .text-answer {
          font-family: Larabiefont,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-size: 15px;
          font-weight: 400;
          line-height: 18px;
          letter-spacing: 0em;
          text-align: left;
          color: #FFFFFF;
          /* Inside auto layout */
          flex: none;
          order: 1;
          flex-grow: 0;
          white-space: pre-wrap;
          overflow-wrap: break-word;
      }

      .position-logo-line1 { padding-left: 30px; }
      .position-logo-line2 {
          padding-left: 70px;
          padding-bottom: 10px;
      }
      .position-text {
          padding-top: 10px;
          padding-bottom: 10px;
          padding-left: 70px;
      }
      .position-link {
          padding-top: 10px;
          padding-bottom: 10px;
          padding-left: 70px;
      }
      .position-progress {
          padding-left: 30px;
          text-align: left;
      }
      .position-progress-result {
          padding-left: 70px;
          padding-top: 5px;
          text-align: left;
      }
      .position-question {
          padding-left: 60px;
          padding-right: 62px;
          padding-top: 20px;
          padding-bottom: 20px;
      }
      .position-explanation {
          padding-left: 70px;
          padding-right: 30px;
          padding-top: 15px;
      }
      .position-answer {
          padding-left: 60px;
      }
      .position-text-answer {
          padding-left: 30px;
          padding-top: 10px;
          padding-bottom: 10px;
      }
      .position-button {
          padding-top: 20px;
          padding-bottom: 10px;
          padding-left: 60px;
          text-align: left;
          overflow-wrap: break-word; 
      }
      .bright-line {
          margin-top: 30px;
          margin-left: 70px;
          margin-right: 30px;
          border-color: #5BBF3B;
          border-style: inset;
          border-width: 1px;
      }
      .dark-line {
          margin-top: 30px;
          margin-left: 70px;
          margin-right: 30px;
          border-color: #5BBF3B;
          opacity: 0.3;
          border-style: inset;
          border-width: 1px;
      }
      .score-img {
          padding-left: 70px;
          padding-right: 30px;
          padding-top: 15px;
      }

      .text-button {
          font-size: 17px;
          font-family: Halvar Breitschrift DEMO,Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;
          font-weight: 400;
          color:  #5BBF3B;
          background-color: #000000;
          /* identical to box height */
          letter-spacing: 0.06em;
          text-transform: uppercase;
          overflow-wrap: break-word;
          overflow: hidden;
          white-space: pre-wrap;
          line-height: 19.69px;
          height: 100px;
          text-align: center;
      }
      .text-play-button { padding-top: 40px; }
      .text-again-button { padding-top: 30px; }

      /** scrollbar **/
      /* width */
      ::-webkit-scrollbar { width: 2px; }

      /* Track */
      ::-webkit-scrollbar-track {
          box-shadow: inset 0 0 2px white; 
          border-radius: 1px;
      }
 
      /* Handle */
      ::-webkit-scrollbar-thumb {
          background: #5BBF3B; 
          border-radius: 1px;
      }

      /* Handle on hover */
      ::-webkit-scrollbar-thumb:hover { background: #9E0A36; }

      /** Radio button **/
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
          top: 12px;
          left: 30px;
          height: 15px;
          width: 15px;
          background-color: #FFFFFF;
          border-radius: 50%;
      }
      .container:hover input ~ .checkmark {
          border-radius: 50%;
          background: #5BBF3B;
      }
      .container input:checked ~ .checkmark { background-color: #FFFFFF; }
      .checkmark:after { content: ""; position: absolute; display: none; }
      .container input:checked ~ .checkmark:after { display: block; }
      .container .checkmark:after {
          left: 26.67%;
          right: 26.67%;
          top: 26.67%;
          bottom: 26.67%;
          border-radius: 50%;
          background: #5BBF3B;
      }
      /** Link styling **/
      a { text-decoration: none; }
    ''
, quizDescription =
''
The purpose of this game is to demonstrate the abilities of "GameBot" example from telegram-bot-simple Haskell package. 
''
, quizFooterBegin =
''
Do you have ideas for a new question? Please share them via @HaskellTelegramSimpleBot
''
, quizFooterCode =
''
/feedback <your suggestion>
''
, quizFooterEnd =
''
command. Enjoy!
''
, quizPicture = Some
    { goodScore = 0.79
    , badScore = 0.39
    , theGood = "/img/game/the_good.png"
    , theBad = "/img/game/the_bad.png"
    , theUgly = "/img/game/the_ugly.png"
    }
}
