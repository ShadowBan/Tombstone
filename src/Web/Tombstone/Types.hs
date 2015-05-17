{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Web.Tombstone.Types
    ( GithubClientId(..)
    , GithubSecretId(..)
    , Config(..)
    , AppState(..)
    , SessionData(..)
    , AppM
    , FullName(..)
    , URL(..)
    , Email(..)
    , GithubLogin(..)
    , BountyDescription(..)
    , CompensationRequirements(..)
    , mkCompensation
    , compensationCurrency
    , compensationMagnitude
    , Currency(..)
    ) where


-------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Word
import           Opaleye
import           Opaleye.Internal.RunQuery
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
data SessionData = SessionData {
      sdUserId :: Maybe Int --TODO: key type from opaleye
    }


-------------------------------------------------------------------------------
type AppM = WebStateM Connection SessionData AppState


-------------------------------------------------------------------------------
newtype Email = Email {
      emailText :: Text
    } deriving (Show, Eq, Ord, FromJSON)


deriving instance QueryRunnerColumnDefault PGText Email


-------------------------------------------------------------------------------
newtype FullName = FullName {
      fullNameText :: Text
    } deriving (Show, Eq, Ord, FromJSON)


deriving instance QueryRunnerColumnDefault PGText FullName


-------------------------------------------------------------------------------
newtype GithubLogin = GithubLogin {
      githubLoginText :: Text
    } deriving (Show, Eq, Ord, FromJSON)


deriving instance QueryRunnerColumnDefault PGText GithubLogin


-------------------------------------------------------------------------------
newtype URL = URL {
      urlText :: Text
    } deriving (Show, Eq, Ord, FromJSON)


deriving instance QueryRunnerColumnDefault PGText URL

-------------------------------------------------------------------------------
newtype BountyDescription = BountyDescription {
      bountyDescriptionText :: Text
    }


-------------------------------------------------------------------------------
data CompensationRequirements = Salary Compensation
                              | Hourly Compensation


-------------------------------------------------------------------------------
data Compensation = Compensation Currency Int


-------------------------------------------------------------------------------
mkCompensation :: Currency -> Int -> Maybe Compensation
mkCompensation c n
  | n >= 0    = Just $ Compensation c n
  | otherwise = Nothing


-------------------------------------------------------------------------------
compensationCurrency :: Compensation -> Currency
compensationCurrency (Compensation c _) = c


-------------------------------------------------------------------------------
compensationMagnitude :: Compensation -> Int
compensationMagnitude (Compensation _ n) = n


-------------------------------------------------------------------------------
data Currency = USDCents
