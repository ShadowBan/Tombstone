{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Web.Tombstone.Schema
    ( -- * User
      usersTable
    , runUsersQuery
    , UserId'(..)
    , User
    , UserColumn
    -- * Bounties
    , bountiesTable
    , BountyId'(..)
    , Bounty
    , BountyColumn
    -- * Utilities
    , printSql
    ) where


-------------------------------------------------------------------------------
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Database.PostgreSQL.Simple      (Connection)
import           GHC.Int
import           Opaleye
-------------------------------------------------------------------------------
import           Web.Tombstone.Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- User
-------------------------------------------------------------------------------
data User' a b c d e f = User {
      userId          :: a
    , userName        :: b
    , userAvatarUrl   :: c
    , userEmail       :: d
    , userGithubLogin :: e
    , userHireable    :: f
    } deriving (Show)


-- can't use newtype for now :(
data UserId' a = UserId a deriving (Show, Eq, Ord)


type UserIdColumn = UserId' (Column PGInt8)


type UserColumn = User'
                  UserIdColumn
                  (Column PGText)
                  (Column PGText)
                  (Column (Nullable PGText))
                  (Column PGText)
                  (Column PGBool)

type UserId = UserId' Int64
type User = User'
            UserId
            FullName
            URL
            (Maybe Email)
            GithubLogin
            Bool


$(makeAdaptorAndInstance "pUser" ''User')
$(makeAdaptorAndInstance "pUserId" ''UserId')


usersTable :: Table UserColumn UserColumn
usersTable = Table "users" (pUser User { userId = pUserId (UserId (required "id"))
                                       , userName = required "name"
                                       , userAvatarUrl = required "avatar_url"
                                       , userEmail = required "email"
                                       , userGithubLogin = required "github_login"
                                       , userHireable = required "hireable"
                                       })



--TODO: drop, just for testing
runUsersQuery
  :: Connection
  -> Query UserColumn
   -> IO [User]
runUsersQuery = runQuery


-------------------------------------------------------------------------------
-- Bounties
-------------------------------------------------------------------------------
data Bounty' a b c d e = Bounty {
      bountyId           :: a
    , bountyDescription  :: b
    , bountyClaimed      :: c
    , bountyCompensation :: d
    , bountyUserId       :: e
    } deriving (Show)


-- can't use newtype for now :(
data BountyId' a = BountyId a deriving (Show, Eq, Ord)


type BountyIdColumn = BountyId' (Column PGInt8)

--TODO: composite column for compensation?

type BountyColumn = Bounty'
                    BountyIdColumn
                    (Column PGText)
                    (Column PGBool)
--TODO: composite column for compensation?
                    (Column PGText)
                    UserIdColumn

type BountyId = BountyId' Int64
type Bounty = Bounty'
              BountyId
              BountyDescription
              Bool
              CompensationRequirements
              UserId


$(makeAdaptorAndInstance "pBounty" ''Bounty')
$(makeAdaptorAndInstance "pBountyId" ''BountyId')


bountiesTable :: Table BountyColumn BountyColumn
bountiesTable = Table "bounties" (pBounty Bounty { bountyId = pBountyId (BountyId (required "id"))
                                                 , bountyDescription = required "description"
                                                 , bountyClaimed = required "claimed"
                                                 , bountyCompensation = required "compensation"
                                                 , bountyUserId = pUserId (UserId (required "user_id"))
                                                 })


-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres
