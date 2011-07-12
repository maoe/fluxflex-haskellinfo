{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withHaskellInfo
    , withDevelApp
    ) where

import HaskellInfo
import Settings
import Yesod.Helpers.Static
import Data.ByteString (ByteString)
import Network.Wai (Application)
import Data.Dynamic (Dynamic, toDyn)

-- Import all relevant handler modules here.
import Handler.Index

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in HaskellInfo.hs. Please see
-- the comments there for more details.
mkYesodDispatch "HaskellInfo" resourcesHaskellInfo

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withHaskellInfo :: (Application -> IO a) -> IO a
withHaskellInfo f = do
    let h = HaskellInfo s
    toWaiApp h >>= f
  where
    s = static Settings.staticdir

withDevelApp :: Dynamic
withDevelApp = toDyn (withHaskellInfo :: (Application -> IO ()) -> IO ())
