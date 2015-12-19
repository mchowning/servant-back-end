{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics


data Artist = Artist { artistName :: Text
                     , albums     :: [Album] } deriving (Eq, Show, Generic)


data Album = Album { albumName :: Text
                   , year      :: Int
                   , albumLen  :: Len
                   , songs     :: [Song] } deriving (Eq, Show, Generic)


data Len = Len { mins :: Int
               , secs :: Int } deriving (Eq, Show, Generic)


data Song = Song { songName :: Text
                 , songLen  :: Len } deriving (Eq, Show, Generic)


instance ToJSON Artist
  where
    toJSON = genericToJSON defaultOptions


instance ToJSON Album
  where
    toJSON = genericToJSON defaultOptions


instance ToJSON Len
  where
    toJSON = genericToJSON defaultOptions


instance ToJSON Song
  where
    toJSON = genericToJSON defaultOptions


data SortBy = Name | Year | Length
