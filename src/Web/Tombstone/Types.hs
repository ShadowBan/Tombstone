{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Tombstone.Types
    ( GithubClientId(..)
    , GithubSecretId(..)
    , Config(..)
    , AppState(..)
    , SessionData(..)
    , AppM
    ) where


-------------------------------------------------------------------------------
import           Data.Text                  (Text)
import           Data.Time
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Word
import           Web.Spock.Safe
-------------------------------------------------------------------------------


newtype GithubClientId = GithubClientId {
      githubClientIdText :: Text
    } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
newtype GithubSecretId = GithubSecretId {
      githubSecretIdText :: Text
    } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
data Config = Config {
      configPort             :: Int
    , configGHClientId       :: GithubClientId
    , configGHSecretId       :: GithubSecretId
    , configPGHost           :: String
    , configPGPort           :: Word16
    , configPGUser           :: String
    , configPGPassword       :: String
    , configPGDatabase       :: String
    , configPGStripes        :: Int
    , configPGConnsPerStripe :: Int
    , configPGKeepalive      :: NominalDiffTime
    }


-------------------------------------------------------------------------------
data AppState = AppState {
      asConfig :: Config
    }

-------------------------------------------------------------------------------
data SessionData = SessionData


-------------------------------------------------------------------------------
type AppM = WebStateM Connection SessionData AppState
