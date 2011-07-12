{-# LANGUAGE TemplateHaskell #-}
module Model.Internal
  ( compiler
  , programConfiguration
  , mkModel
  ) where

import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Verbosity (silent)
import Language.Haskell.TH
import Model.Internal.Configure

mkConfigure

mkModel :: Q [Dec]
mkModel = do
  pi <- runIO $ getInstalledPackages silent compiler [GlobalPackageDB, UserPackageDB] programConfiguration
  [d| packageIndex = read $(litE (StringL (show pi))) :: PackageIndex |]
