{-# LANGUAGE OverloadedStrings #-}

module TwilioParser where

import Data.Aeson
import Control.Applicative
import Control.Monad

data InboundSMS = InboundSMS {
  smsBody :: SMSBody
} deriving (Show)

data SMSBody = SMSBody {
  from :: [String],
  text :: [String]
} deriving (Show)

instance FromJSON SMSBody where
  parseJSON (Object v) = SMSBody <$> (v .: "From") <*> (v .: "Body")
  parseJSON _ = mzero

instance FromJSON InboundSMS where
  parseJSON (Object v) = do
    smsBody <- parseJSON =<< (v .: "form")
    return $ InboundSMS smsBody
  parseJSON _ = mzero
