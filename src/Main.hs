{-# LANGUAGE RecordWildCards #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Network.Wai.Middleware.Static
import           Options.Applicative           as O
import           Web.Spock.Safe
-------------------------------------------------------------------------------


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (helper <*> configParser)
      ( fullDesc <>
        progDesc "Start the tombstone server" <>
        O.header "tombstone - the backend server for the tombstone web app" )


-------------------------------------------------------------------------------
run :: Config -> IO ()
run Config {..} = runSpock configPort $ spockT id $ do
  middleware $ staticPolicy $ addBase "public"
  app


-------------------------------------------------------------------------------
app :: SpockT IO ()
app = return ()


-------------------------------------------------------------------------------
configParser :: Parser Config
configParser = Config
  <$> option auto (long "port" <>
                   short 'p' <>
                   metavar "PORT" <>
                   value 8000 <>
                   showDefault <>
                   help "Server port number")

-------------------------------------------------------------------------------
data Config = Config {
      configPort :: Int
    }
