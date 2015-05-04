{-# LANGUAGE OverloadedStrings #-}
module Web.Tombstone.Routes.Sessions
    ( routes
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Monoid
import           Lucid
import           Web.Spock.Safe
-------------------------------------------------------------------------------
import           Web.Tombstone.Types
-------------------------------------------------------------------------------


routes :: SpockT AppM ()
routes =
  get "new" $ do
    cid <- configGHClientId . asConfig <$> getState
    renderHTML $ newSessionView cid


renderHTML :: MonadIO m => Html a -> ActionT m a
renderHTML h = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes $ renderBS h


newSessionView :: GithubClientId -> Html ()
newSessionView (GithubClientId cid) =
  doctypehtml_ $
    html_ $
      body_ $ do
        p_ "Well, hello there!"
        p_ $ do
          "We're going to now talk to the GitHub API. Ready? "
          a_ [href_ $ "https://github.com/login/oauth/authorize?scope=user:email&client_id=" <> cid] "Click here"
          " to begin!"
