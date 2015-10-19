{-# LANGUAGE DataKinds                                                     #-}
{-# LANGUAGE DeriveGeneric                                                 #-}
{-# LANGUAGE OverloadedStrings                                             #-}
{-# LANGUAGE TypeFamilies                                                  #-}
{-# LANGUAGE TypeOperators                                                 #-}

module MediumAPI where

import           Control.Applicative
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics

import           Servant
import           Servant.Client

type API = "v1" :> "me" :> Header "Authorization" Token :> Get '[JSON] User

type Token = Text

data User = User { id       :: Text
                 , username :: Text
                 , name     :: Text
                 , url      :: Text
                 , imageUrl :: Text
                 }
                 deriving (Show, Read, Eq, Generic)

instance FromJSON User where
    parseJSON (Object o) = do
      Object o' <- o .: "data"
      User <$> o' .: "id"
           <*> o' .: "username"
           <*> o' .: "name"
           <*> o' .: "url"
           <*> o' .: "imageUrl"
    parseJSON _          = error "Expected an object"

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.medium.com" 443

me ::  Maybe Token -> EitherT ServantError IO User

api :: Proxy API
api = Proxy

me = client api baseUrl
