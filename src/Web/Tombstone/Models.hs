module Web.Tombstone.Models
    ( User(..)
    , Email(..)
    , UserName(..)
    , GithubLogin(..)
    , Bounty(..)
    , BountyTitle(..)
    , BountyDescription(..)
    , CompensationRequirements(..)
    , Compensation
    , mkCompensation
    , compensationCurrency
    , compensationQuantity
    , Currency(..)
    ) where


-------------------------------------------------------------------------------
import           Data.Text (Text)
-------------------------------------------------------------------------------


data User = User {
      userName      :: UserName
      userAvatarUrl :: URL
    , userEmail     :: Maybe Email
    , githubLogin   :: GithubLogin
    , hireable      :: Bool
    }


-------------------------------------------------------------------------------
newtype Email = Email {
      emailText :: Text
    }


-------------------------------------------------------------------------------
newtype UserName = UserName {
      userNameText :: Text
    }


-------------------------------------------------------------------------------
newtype GithubLogin = GithubLogin {
      githubLoginText :: Text
    }


-------------------------------------------------------------------------------
newtype URL = URL {
      urlText :: Text
    }


-------------------------------------------------------------------------------
data Bounty = Bounty {
      bountyTitle :: BountyTitle
    , bountyDescription :: BountyDescription
    --TODO: user key
    , bountyClaimed :: Bool
    , bountyCompensation :: Maybe CompensationRequirements
    }


-------------------------------------------------------------------------------
newtype BountyTitle = BountyTitle {
      bountyTitleText :: Text
    }


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
compensationCurrency :: Currency
compensationCurrency (Compensation c _) = c


-------------------------------------------------------------------------------
compensationQuantity :: Int
compensationQuantity (Compensation _ n) = n


-------------------------------------------------------------------------------
data Currency = USD
