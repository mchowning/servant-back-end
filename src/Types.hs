{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


data Vehicle = Vehicle { dbId   :: Maybe Int
                       , vin    :: Text
                       , year   :: Int
                       , model  :: Text
                       , issues :: [Issue] } deriving (Eq, Show, Generic)


data Issue = Issue { issueType :: IssueType
                   , priority  :: Priority } deriving (Eq, Show, Generic, Ord)


data IssueType = Battery
               | Brakes
               | Electrical deriving (Eq, Show, Generic, Ord)


data Priority = High | Med | Low deriving (Eq, Show, Generic, Ord)


instance ToJSON Vehicle
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON Issue
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON IssueType
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON Priority
  where
    toJSON = genericToJSON defaultOptions
instance FromJSON Vehicle
instance FromJSON Issue
instance FromJSON IssueType
instance FromJSON Priority
