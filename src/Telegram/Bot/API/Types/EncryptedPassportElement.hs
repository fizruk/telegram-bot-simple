{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.EncryptedPassportElement where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.PassportFile
import Telegram.Bot.API.Internal.Utils

-- ** 'EncryptedPassportElement'

-- | Contains information about documents or other Telegram Passport elements shared with the bot by the user.
data EncryptedPassportElement = EncryptedPassportElement
  { encryptedPassportElementType        :: PassportElementType  -- ^ One of “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport”, “address”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”, “phone_number”, “email”.
  , encryptedPassportElementData        :: Maybe Text           -- ^ Base64-encoded encrypted Telegram Passport element data provided by the user, available for “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport” and “address” types. Can be decrypted and verified using the accompanying 'EncryptedCredentials'.
  , encryptedPassportElementPhoneNumber :: Maybe Text           -- ^ User's verified phone number, available only for “phone_number” type.
  , encryptedPassportElementEmail       :: Maybe Text           -- ^ User's verified email address, available only for “email” type.
  , encryptedPassportElementFiles       :: Maybe [PassportFile] -- ^ Array of encrypted files with documents provided by the user, available for “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration” and “temporary_registration” types. Files can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementFrontSide   :: Maybe PassportFile   -- ^ Encrypted file with the front side of the document, provided by the user. Available for “passport”, “driver_license”, “identity_card” and “internal_passport”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementReverseSide :: Maybe PassportFile   -- ^ Encrypted file with the reverse side of the document, provided by the user. Available for “driver_license” and “identity_card”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementSelfie      :: Maybe PassportFile   -- ^ Encrypted file with the selfie of the user holding a document, provided by the user; available for “passport”, “driver_license”, “identity_card” and “internal_passport”. The file can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementTranslation :: Maybe [PassportFile] -- ^ Array of encrypted files with translated versions of documents provided by the user. Available if requested for “passport”, “driver_license”, “identity_card”, “internal_passport”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration” and “temporary_registration” types. Files can be decrypted and verified using the accompanying EncryptedCredentials.
  , encryptedPassportElementHash        :: Text                 -- ^ Base64-encoded element hash for using in 'PassportElementErrorUnspecified'.
  } deriving (Generic, Show)


-- | One of “personal_details”, “passport”, “driver_license”, “identity_card”, “internal_passport”, “address”, “utility_bill”, “bank_statement”, “rental_agreement”, “passport_registration”, “temporary_registration”, “phone_number”, “email”.
data PassportElementType
  = PassportElementTypePersonalDetails
  | PassportElementTypePassport
  | PassportElementTypeDriverLicense
  | PassportElementTypeIdentityCard
  | PassportElementTypeInternalPassport
  | PassportElementTypeAddress
  | PassportElementTypeUtilityBill
  | PassportElementTypeBankStatement
  | PassportElementTypeRentalAgreement
  | PassportElementTypePassportRegistration
  | PassportElementTypeTemporaryRegistration
  | PassportElementTypePhoneNumber
  | PassportElementTypeEmail
  deriving (Generic, Show)

foldMap deriveJSON'
  [ ''EncryptedPassportElement
  , ''PassportElementType
  ]
