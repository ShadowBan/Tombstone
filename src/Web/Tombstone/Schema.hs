{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Web.Tombstone.Schema
    ( usersTable
    , runUsersQuery
    , UserId'(..)
    , User
    , UserColumn
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



runUsersQuery
  :: Connection
  -> Query UserColumn
   -> IO [User]
runUsersQuery = runQuery

-------------------------------------------------------------------------------
printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres
