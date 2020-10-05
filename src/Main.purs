module Main where

import Prelude

import API (API, api)
import Config (Config, fromPort, loadConfig)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.Wai (Application, responseStr)
import Network.Wai.Middleware.Logger (dev, loggerMiddleware)
import Network.Warp as Warp
import Node.Jwt (Secret(..))
import Node.Process (stdout)
import Swerve.Server (swerveContext)
import Type.Proxy (Proxy(..))
import Utils (verifyToken)

main :: Effect Unit
main = do
  econfig <- loadConfig
  case econfig of 
    Left e      -> error "Could not parse environment variables!"
    Right config -> do 
      let beforeMainLoop = Console.log $ "Listening on port " <> show config.port
      void $ Warp.runSettings Warp.defaultSettings { beforeMainLoop = beforeMainLoop, port = fromPort config.port } 
           $ loggerMiddleware dev stdout
           $ app config 

app :: Config -> Application 
app config req res = swerveContext (Proxy :: _ API) { verifiedToken: verifyToken (Secret config.secretToken) } (api config) req res 
