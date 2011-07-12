{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withHaskellInfo)
import Network.Wai.Handler.FastCGI (run)

main :: IO ()
main = withHaskellInfo run
#else
import Controller (withHaskellInfo)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withHaskellInfo $ run port . debug
#endif
