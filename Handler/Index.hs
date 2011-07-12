{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Index where

import HaskellInfo
import Model

getIndexR :: Handler RepHtml
getIndexR = do
  defaultLayout $ do
    setTitle "Haskell info"
    addWidget $(widgetFile "index")

