module Web.Tombstone.Models
    ( User(..)
    , Email(..)
    , FullName(..)
    , GithubLogin(..)
    , Bounty(..)
    , BountyDescription(..)
    , CompensationRequirements(..)
    , Compensation
    , mkCompensation
    , compensationCurrency
    , compensationQuantity
    , Currency(..)
    ) where


-------------------------------------------------------------------------------
import           Data.Text           (Text)
-------------------------------------------------------------------------------
import           Web.Tombstone.Types
-------------------------------------------------------------------------------


data User = User {
      userName      :: FullName
    , userAvatarUrl :: URL
    , userEmail     :: Maybe Email
    , githubLogin   :: GithubLogin
    , hireable      :: Bool
    }


-------------------------------------------------------------------------------
data Bounty = Bounty {
      bountyDescription  :: BountyDescription
    , bountyClaimed      :: Bool
    , bountyCompensation :: Maybe CompensationRequirements
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
compensationCurrency :: Compensation -> Currency
compensationCurrency (Compensation c _) = c


-------------------------------------------------------------------------------
compensationQuantity :: Compensation -> Int
compensationQuantity (Compensation _ n) = n


-------------------------------------------------------------------------------
data Currency = USD
