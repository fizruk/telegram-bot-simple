{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Telegram.Bot.API.Passport where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Types
import Telegram.Bot.API.MakingRequests

-- * Methods

-- ** 'setPassportDataErrors'
--
-- Informs a user that some of the Telegram Passport elements they provided contains errors. The user will not be able to re-submit their Passport to you until the errors are fixed (the contents of the field for which you returned the error must change). Returns True on success.

type SetPassportDataErrors
  =  "setPassportDataErrors"
  :> RequiredQueryParam "user_id" UserId
  :> RequiredQueryParam "errors" [PassportElementError]
  :> Get '[JSON] (Response Bool)

-- | Use this if the data submitted by the user doesn't satisfy the standards your service requires for any reason. For example, if a birthday date seems invalid, a submitted document is blurry, a scan shows evidence of tampering, etc. Supply some details in the error message to make sure the user knows how to correct the issues.
setPassportDataErrors :: UserId -> [PassportElementError] -> ClientM (Response Bool)
setPassportDataErrors = client (Proxy @SetPassportDataErrors)
