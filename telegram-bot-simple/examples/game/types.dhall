let Question
      = < QuestionBool :
            { questionBoolText : Text
            , questionBoolAnswerIsTrue : Bool
            , questionBoolExplanation : Text
            }
        | QuestionChoice :
            { questionChoiceText : Text
            , questionChoiceChoices :
                 List
                   { choiceText : Text
                   , choiceNumber : Integer
                   , choiceIsCorrect : Bool
                   }
            , questionChoiceExplanation : Text
            }
        >
let Answer
      = { answerQuestion : Question
        , answerIsRight : Bool
        , answerExplanationOnError : Text
        }
let UserData
      = { userDataCurrentQuestion : Optional Question
        , userDataQuestions : List Question
        , userDataAnswers : List Answer
        , userDataTotalQuestions : Integer
        }
in 

{ question = Question, answer = Answer, userData = UserData }
