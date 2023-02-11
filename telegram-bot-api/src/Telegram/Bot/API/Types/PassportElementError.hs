{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Telegram.Bot.API.Types.PassportElementError where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (..))

import qualified Data.Text.Lazy as TL

import Telegram.Bot.API.Types.EncryptedPassportElement
import Telegram.Bot.API.Internal.Utils

-- ** 'PassportElementError'

data PassportErrorSource
  = PassportErrorSourceData
  | PassportErrorSourceFrontSide
  | PassportErrorSourceReverseSide
  | PassportErrorSourceSelfie
  | PassportErrorSourceFile
  | PassportErrorSourceFiles
  | PassportErrorSourceTranslationFile
  | PassportErrorSourceTranslationFiles
  | PassportErrorSourceUnspecified
  deriving (Generic, Show)

instance ToJSON   PassportErrorSource where toJSON = gtoJSON
instance FromJSON PassportErrorSource where parseJSON = gparseJSON

data PassportElementError
  = PassportElementError
    { passportElementErroSource       :: PassportErrorSource -- ^ Error source, must be one of 'PassportErrorSource'.
    , passportElementErrorType        :: PassportElementType -- ^ The section of the user's Telegram Passport which has the error, one of 'PassportElementType'.
    , passportElementErrorName        :: Text                -- ^ Name of the data field which has the error.
    , passportElementErrorHash        :: Maybe Text          -- ^ Base64-encoded data hash.
    , passportElementErrorMessage     :: Text                -- ^ Error message.
    , passportElementErrorFileHash    :: Maybe Text          -- ^ Base64-encoded hash of the file with the reverse side of the document.
    , passportElementErrorFileHashes  :: Maybe [Text]        -- ^ List of base64-encoded file hashes.
    , passportElementErrorElementHash :: Maybe Text          -- ^ Base64-encoded element hash.
    }
    deriving (Generic, Show)

instance ToHttpApiData PassportElementError where
  toUrlPiece = TL.toStrict . encodeToLazyText

instance ToHttpApiData [PassportElementError] where
  toUrlPiece = TL.toStrict . encodeToLazyText

instance ToJSON   PassportElementError where toJSON = gtoJSON
instance FromJSON PassportElementError where parseJSON = gparseJSON
