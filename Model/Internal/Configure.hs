{-# LANGUAGE TemplateHaskell #-}
module Model.Internal.Configure
  ( mkConfigure
  ) where
import Distribution.Simple.Compiler (Compiler, PackageDB(..), buildCompilerFlavor, compilerId, compilerLanguages, compilerExtensions)
import Distribution.Simple.Configure (configCompiler, getInstalledPackages)
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Program.Db
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Verbosity (silent)
import Language.Haskell.TH

configure :: IO (Compiler, ProgramConfiguration)
configure = configCompiler (Just buildCompilerFlavor) Nothing Nothing defaultProgramConfiguration silent

mkConfigure :: Q [Dec]
mkConfigure = do
  (c, pc) <- runIO configure
  compiler <- [d| compiler = read $(litE (StringL (show c))) :: Compiler |]
  progConf <- [d| programConfiguration = read $(litE (StringL (show pc))) :: ProgramConfiguration |]
  return $ concat[compiler, progConf]
