{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import qualified Configuration.Dotenv          as DE
import           Control.Error
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple    as PG
import           Network.Wai.Middleware.Static
import qualified Opaleye                       as O
import           Options.Applicative           as O
import           System.Environment
import           Web.Spock.Safe
-------------------------------------------------------------------------------
import qualified Web.Tombstone.Routes.OAuth2   as OAuth2
import qualified Web.Tombstone.Routes.Sessions as Sessions
import           Web.Tombstone.Schema
import           Web.Tombstone.Types
-------------------------------------------------------------------------------


main :: IO ()
main = do
  DE.loadFile True ".env"
  CLIOptions {..} <- execParser opts
  cfg <- Config <$> pure cliPort
                <*> (GithubClientId . T.pack <$> envKey "GITHUB_CLIENT_ID")
                <*> (GithubSecretId . T.pack <$> envKey "GITHUB_SECRET_ID")
                <*> (fromMaybe "localhost" <$> lookupEnv "POSTGRES_HOST")
                <*> (maybe 5432 read <$> lookupEnv "POSTGRES_PORT")
                <*> envKey "POSTGRES_USER"
                <*> envKey "POSTGRES_PASSWORD"
                <*> envKey "POSTGRES_DATABASE"
                <*> (maybe 1 read <$> lookupEnv "POSTGRES_STRIPES")
                <*> (maybe 10 read <$> lookupEnv "POSTGRES_CONNS_PER_STRIPE")
                <*> (maybe 300 (fromInteger . read) <$> lookupEnv "POSTGRES_KEEPALIVE")
  run cfg
  where
    envKey k = do
      res <- runEitherT $ noteT ("Missing env var " <> k) (MaybeT $ lookupEnv k)
      case res of
        Right x -> return x
        Left e -> error e
    opts = info (helper <*> configParser)
      ( fullDesc <>
        progDesc "Start the tombstone server" <>
        O.header "tombstone - the backend server for the tombstone web app" )


-------------------------------------------------------------------------------
run :: Config -> IO ()
run cfg@Config {..} =
    runSpock configPort $ spock sessionConfig pool appState $ do
      middleware $ staticPolicy $ addBase "public"
      app
  where
    pool = PCConn connBuilder
    appState = AppState { asConfig = cfg
                        }
    connInfo = ConnectInfo { connectHost     = configPGHost
                           , connectPort     = configPGPort
                           , connectUser     = configPGUser
                           , connectPassword = configPGPassword
                           , connectDatabase = configPGDatabase
                           }
    connBuilder = ConnBuilder { cb_createConn        = PG.connect connInfo
                              , cb_destroyConn       = PG.close
                              , cb_poolConfiguration = poolCfg
                              }
    poolCfg = PoolCfg { pc_stripes      = configPGStripes
                      , pc_resPerStripe = configPGConnsPerStripe
                      , pc_keepOpenTime = configPGKeepalive
                      }
    sessionConfig = SessionCfg { sc_cookieName = "tombstone"
                               , sc_sessionTTL = 60 * 60 * 24 * 14
                               , sc_sessionIdEntropy = 1 -- wut
                               , sc_sessionExpandTTL = False
                               , sc_emptySession = SessionData Nothing
                               , sc_persistCfg = Nothing -- no persistence for now
                               }


-------------------------------------------------------------------------------
app :: SpockT AppM ()
app = do
  subcomponent "oauth2" $ OAuth2.routes
  subcomponent "sessions" $ Sessions.routes
  get "debug" $ do
    res <- runQuery $ \c -> runUsersQuery c $ O.queryTable usersTable
    liftIO $ print (res :: [User])


-------------------------------------------------------------------------------
configParser :: Parser CLIOptions
configParser = CLIOptions
  <$> option auto (long "port" <>
                   short 'p' <>
                   metavar "PORT" <>
                   value 8000 <>
                   showDefault <>
                   help "Server port number")


-------------------------------------------------------------------------------
data CLIOptions = CLIOptions {
      cliPort :: Int
    }
