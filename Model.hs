{-# LANGUAGE TemplateHaskell #-}
module Model
  ( -- * Compiler specific info
    compilerName
  , compilerVersion
  , languages
  , extensions
    -- * Package specific info
  , packages
  , PackageIdentifier(..)
  , packageName
  , module Data.Version
  ) where

import Control.Applicative
import Data.Version (Version, showVersion)
import Distribution.Compiler (CompilerFlavor)
import Distribution.InstalledPackageInfo (InstalledPackageInfo, InstalledPackageInfo_(..))
import Distribution.Package (PackageId, PackageIdentifier(..), PackageName(..))
import Distribution.Simple.Compiler (Compiler(..), CompilerId(..), compilerId)
import Distribution.Simple.PackageIndex (allPackages)
import Language.Haskell.Extension (Language, Extension)
import qualified Model.Internal as I

I.mkModel

compilerName :: CompilerFlavor
compilerVersion :: Version
CompilerId compilerName compilerVersion = compilerId I.compiler

languages :: [Language]
languages = fst <$> compilerLanguages I.compiler

extensions :: [Extension]
extensions = fst <$> compilerExtensions I.compiler


packages :: [PackageId]
packages = sourcePackageId <$> allPackages packageIndex

packageName :: PackageId -> String
packageName package = name
  where PackageName name = pkgName package