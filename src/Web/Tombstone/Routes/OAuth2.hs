{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Tombstone.Routes.OAuth2
    ( routes
    , GithubUser(..)
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text.Encoding      as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.OAuth.OAuth2
import           Web.Spock.Safe
-------------------------------------------------------------------------------
import           Web.Tombstone.Types
-------------------------------------------------------------------------------


routes :: SpockT AppM ()
routes =
  get "callback" $ do
    code <- T.encodeUtf8 <$> param' "code"
    oa <- mkGHOAuth2
    liftIO $ withManager tlsManagerSettings $ \mgr -> do
      let state = "testGithubApi"
      let (url, body') = accessTokenUrl oa code
      token <- doJSONPostRequest mgr oa url (body' ++ [("state", state)])
      print (token :: OAuth2Result AccessToken)
      case token of
        Right at  -> userInfo mgr at >>= print
        Left _    -> putStrLn "no access token found yet"


-------------------------------------------------------------------------------
mkGHOAuth2 :: ActionT AppM OAuth2
mkGHOAuth2 = do
    Config {..} <- asConfig <$> getState
    return $ OAuth2 { oauthClientId = tBS $ githubClientIdText configGHClientId
                    , oauthClientSecret = tBS $ githubSecretIdText configGHSecretId
                    , oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize"
                    , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                    , oauthCallback = Just "http://localhost:8000/oauth2/callback" --TODO: host in config
                    }
  where
    tBS = T.encodeUtf8


-------------------------------------------------------------------------------
userInfo :: Manager -> AccessToken -> IO (OAuth2Result GithubUser)
userInfo mgr token = authGetJSON mgr token "https://api.github.com/user"


-------------------------------------------------------------------------------
data GithubUser = GithubUser { gName      :: FullName
                             , gLogin     :: GithubLogin
                             , gAvatarUrl :: URL
                             , gEmail     :: Email
                             , gHireable  :: Bool
                             } deriving (Show, Eq)


-------------------------------------------------------------------------------
instance FromJSON GithubUser where
  parseJSON = withObject "GithubUser" parseGithubUser
    where
      parseGithubUser o = GithubUser
                          <$> o .: "name"
                          <*> o .: "login"
                          <*> o .: "avatar_url"
                          <*> o .: "email"
                          <*> o .: "hireable"
