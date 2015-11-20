{-# LANGUAGE DataKinds                                                     #-}
{-# LANGUAGE StandaloneDeriving                                            #-}
{-# LANGUAGE DeriveGeneric                                                 #-}
{-# LANGUAGE OverloadedStrings                                             #-}
{-# LANGUAGE RecordWildCards                                               #-}
{-# LANGUAGE TypeFamilies                                                  #-}
{-# LANGUAGE TypeOperators                                                 #-}

module MediumAPI where

import           Control.Applicative
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.ByteString                           (intercalate)
import           Data.Default.Class
import           Data.Monoid
import           Data.Proxy
import           Data.String
import           Data.Text                                 (Text)
import           Data.Text.Encoding                        (encodeUtf8)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           GHC.Generics

import           Network.HTTP.Client                       hiding (Proxy)

import           Servant
import           Servant.Client


data TokenRequest = TokenRequest { authCode      :: Text
                                 , clientId      :: Text
                                 , clientSecret  :: Text
                                 , redirectUri   :: Text
                                 }

instance ToFormUrlEncoded TokenRequest where
    toFormUrlEncoded TokenRequest{..} = [ ("code", authCode)
                                        , ("client_id", clientId)
                                        , ("client_secret", clientSecret)
                                        , ("grant_type", "authorization_code")
                                        , ("redirect_uri", redirectUri)
                                        ]

newtype Token = Token { token :: Text } deriving (Show, Read, Eq)

instance ToText Token where
    toText Token{..} = "Bearer " <> token

data ContentFormat = Html | Markdown
                   deriving (Show, Read, Eq, Generic)

instance ToJSON ContentFormat where
    toJSON Html     = "html"
    toJSON Markdown = "markdown"

instance FromJSON ContentFormat where
    parseJSON (String "html")     = return Html
    parseJSON (String "markdown") = return Markdown
    parseJSON _                   =
      error "Expected valid string value for content format"

data PublishStatus = Public | Draft | Unlisted
                   deriving (Show, Read, Eq, Generic)

instance ToJSON PublishStatus where
    toJSON Public   = "public"
    toJSON Draft    = "draft"
    toJSON Unlisted = "unlisted"

instance FromJSON PublishStatus where
    parseJSON (String "public")   = return Public
    parseJSON (String "draft")    = return Draft
    parseJSON (String "unlisted") = return Unlisted
    parseJSON _                   =
      error "Expected valid string value for publish status"

data License = AllRightsReserved | Cc40By | Cc40BySa | Cc40ByNd | Cc40ByNc | Cc40ByNcNd | Cc40ByNcSa | Cc40Zero | PublicDomain
             deriving (Show, Read, Eq, Generic)

instance ToJSON License where
    toJSON AllRightsReserved = "all-rights-reserved"
    toJSON Cc40By            = "cc-40-by"
    toJSON Cc40BySa          = "cc-40-by-sa"
    toJSON Cc40ByNd          = "cc-40-by-nd"
    toJSON Cc40ByNc          = "cc-40-by-nc"
    toJSON Cc40ByNcNd        = "cc-40-by-nc-nd"
    toJSON Cc40ByNcSa        = "cc-40-by-nc-sa"
    toJSON Cc40Zero          = "cc-40-zero"
    toJSON PublicDomain      = "public-domain"

instance FromJSON License where
    parseJSON (String "all-rights-reserved") = return AllRightsReserved
    parseJSON (String "cc-40-by")            = return Cc40By
    parseJSON (String "cc-40-by-sa")         = return Cc40BySa
    parseJSON (String "cc-40-by-nd")         = return Cc40ByNd
    parseJSON (String "cc-40-by-nc")         = return Cc40ByNc
    parseJSON (String "cc-40-by-nc-nd")      = return Cc40ByNcNd
    parseJSON (String "cc-40-by-nc-sa")      = return Cc40ByNcSa
    parseJSON (String "cc-40-zero")          = return Cc40Zero
    parseJSON (String "public-domain")       = return PublicDomain
    parseJSON _                              =
      error "Expected valid string value for license"

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

data Publication = Publication { publicationId               :: Text
                               , publicationName             :: Text
                               , publicationDescription      :: Text
                               , publicationUrl              :: Text
                               , publicationImgUrl           :: Text
                               } deriving (Show, Read, Eq)

instance FromJSON Publication where
    parseJSON (Object o) = Publication <$> o .: "id"
                                       <*> o .: "name"
                                       <*> o .: "description"
                                       <*> o .: "url"
                                       <*> o .: "imageUrl"
    parseJSON _          = error "Expected an object"

-- Since the publications endpoint (like all) returns the data wrapped in
-- a @data@ envelope…
newtype PubList = PubList [Publication] deriving (Show, Read, Eq)

instance FromJSON PubList where
    parseJSON (Object o) = do
      o' <- o .: "data"
      PubList <$> parseJSON o'
    parseJSON _          = error "Expected an object"


data NewPost = NewPost { title         :: Text
                       , contentFormat :: ContentFormat
                       , content       :: Text
                       , tags          :: [Text]
                       , canonicalUrl  :: Maybe Text
                       , publishStatus :: PublishStatus
                       , license       :: License
                       }
                       deriving (Show, Read, Eq, Generic)

instance ToJSON NewPost


data CreatedPost = CreatedPost { postId            :: Text
                               , postTitle         :: Text
                               , authorId          :: Text
                               , postTags          :: [Text]
                               , mediumUrl         :: Text
                               , postCanonicalUrl  :: Maybe Text
                               , postPublishStatus :: PublishStatus
                               , publishedAt       :: Maybe Integer
                               , postLicense       :: License
                               , licenseUrl        :: Text
                               }
                               deriving (Show, Read, Eq, Generic)

instance FromJSON CreatedPost where
    parseJSON (Object o) = do
      Object o' <- o .: "data"
      CreatedPost <$> o' .:  "id"
                  <*> o' .:  "title"
                  <*> o' .:  "authorId"
                  <*> o' .:  "tags"
                  <*> o' .:  "url"
                  <*> o' .:? "canonicalUrl"
                  <*> o' .:  "publishStatus"
                  <*> o' .:? "publishedAt"
                  <*> o' .:  "license"
                  <*> o' .:  "licenseUrl"
    parseJSON _          = error "Expected an object"

data Scope = BasicProfile | ListPublications | PublishPost | UploadImage
           deriving (Show, Read, Eq)

scopeString :: IsString a => Scope -> a
scopeString BasicProfile     = "basicProfile"
scopeString ListPublications = "listPublications"
scopeString PublishPost      = "publishPost"
scopeString UploadImage      = "uploadImage"

instance ToJSON Scope where
    toJSON = scopeString

instance FromJSON Scope where
    parseJSON "basicProfile"      = return BasicProfile
    parseJSON "listPublications"  = return ListPublications
    parseJSON "publishPost"       = return PublishPost
    parseJSON "uploadImage"       = return UploadImage
    parseJSON _                   = error "Invalid scope value"

data TokenResp = TokenResp { tokenType    :: Text
                           , accessToken  :: Text
                           , refreshToken :: Text
                           , scope        :: [Scope]
                           , expiresAt    :: UTCTime
                           }
                           deriving (Show, Eq)

instance FromJSON TokenResp where
    parseJSON (Object o) = TokenResp "Bearer"
                                     <$> o .: "access_token"
                                     <*> o .: "refresh_token"
                                     <*> o .: "scope"
                                     <*> (milliToUtc <$> o .: "expires_at")
    parseJSON _          = error "Expected an object"

data RefreshRequest = RefreshRequest { refreshToken' :: Text
                                     , clientId'     :: Text
                                     , clientSecret' :: Text
                                     }
                                     deriving (Show, Read, Eq)

instance ToFormUrlEncoded RefreshRequest where
    toFormUrlEncoded RefreshRequest{..} = [ ("refresh_token", refreshToken')
                                          , ("client_id",     clientId')
                                          , ("client_secret", clientSecret')
                                          , ("grant_type",    "refresh_token")
                                          ]


milliToUtc :: POSIXTime -> UTCTime
milliToUtc = posixSecondsToUTCTime . (/ 1000)

defaultPost :: NewPost
defaultPost = NewPost "" Html "" [] Nothing Public AllRightsReserved

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.medium.com" 443

-- TODO: scope list should be non-empty
authCodeUrl :: Text -> [Scope] -> Text -> Text -> String
authCodeUrl clientId requestedScope stateText redirectUrl =
    show . getUri $
      setQueryString [ ("client_id",    Just $ encodeUtf8 clientId)
                     , ("state",        Just $ encodeUtf8 stateText)
                     , ("redirect_uri", Just $ encodeUtf8 redirectUrl)
                     , ("scope",        Just scopeList)
                     , ("responseType", Just "code")
                     ]
                     def { host = "medium.com"
                         , port = 443
                         , secure = True
                         , path = "/m/oauth/authorize"
                         }
  where scopeList = intercalate "," $ map scopeString requestedScope

type EitherIO x = EitherT x IO

me ::  Maybe Token -> EitherIO ServantError User
publications :: Text -> Maybe Token -> EitherIO ServantError PubList
posts :: Text -> Maybe Token -> NewPost -> EitherIO ServantError CreatedPost
tokenFromAuthCode :: TokenRequest -> EitherIO ServantError TokenResp
refreshAuthToken :: Maybe Token -> RefreshRequest -> EitherIO ServantError TokenResp

type API = "v1" :> "me" :> Header "Authorization" Token :> Get '[JSON] User
         :<|> "v1" :> "users" :> Capture "authorId" Text :> "posts" :> Header "Authorization" Token :> ReqBody '[JSON] NewPost :> Post '[JSON] CreatedPost
         :<|> "v1" :> "users" :> Capture "userId" Text :> "publications" :> Header "Authorization" Token :> Get '[JSON] PubList
         :<|> "v1" :> "tokens" :> ReqBody '[FormUrlEncoded] TokenRequest :> Post '[JSON] TokenResp
         :<|> "v1" :> "tokens" :> Header "Authorization" Token :> ReqBody '[FormUrlEncoded] RefreshRequest :> Post '[JSON] TokenResp

me :<|> posts :<|> publications :<|> tokenFromAuthCode :<|> refreshAuthToken = client api baseUrl

api :: Proxy API
api = Proxy

