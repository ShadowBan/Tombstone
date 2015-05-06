module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import qualified Configuration.Dotenv                 as DE
import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Data.Monoid
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           System.Environment
import           System.Exit
import           System.IO
-------------------------------------------------------------------------------


main :: IO ()
main = do
    DE.loadFile True ".env"
    hst <- envKey "POSTGRES_HOST"
    prt <- read <$> envKey "POSTGRES_PORT"
    usr <- envKey "POSTGRES_USER"
    pwd <- envKey "POSTGRES_PASSWORD"
    db  <- envKey "POSTGRES_DATABASE"
    let connInfo =
          ConnectInfo { connectHost     = hst
                      , connectPort     = prt
                      , connectUser     = usr
                      , connectPassword = pwd
                      , connectDatabase = db
                      }
    bracket (connect connInfo) close $ \conn -> do
      let ctx = MigrationContext { migrationContextCommand = MigrationInitialization
                                 , migrationContextVerbose = True
                                 , migrationContextConnection = conn
                                 }
      handleMigrationErrors =<< runMigration ctx
      handleMigrationErrors =<< runMigration ctx { migrationContextCommand = MigrationDirectory "migrations" }
  where
    handleMigrationErrors (MigrationError e) = do
      hPutStrLn stderr e
      exitFailure
    handleMigrationErrors MigrationSuccess = return ()
    envKey k = do
      res <- runEitherT $ noteT ("Missing env var " <> k) (MaybeT $ lookupEnv k)
      case res of
        Right x -> return x
        Left e -> error e

