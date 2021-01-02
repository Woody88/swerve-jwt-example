module Main where

import Prelude

import API (_API, api)
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
import Swerve.Server
import Type.Proxy (Proxy(..))
import TypedEnv (envErrorMessage)
import Utils (checkBasicAuth)


main :: Effect Unit
main = do
  econfig <- loadConfig
  case econfig of 
    Left e      -> error $ "Could not parse environment variables!: " <> envErrorMessage e
    Right config -> do 
      let port = 8000
      let beforeMainLoop = Console.log $ "Listening on port " <> show port
      _ <- Warp.runSettings Warp.defaultSettings { beforeMainLoop = beforeMainLoop, port = port } 
          $ loggerMiddleware dev stdout
          $ app config 
      pure unit 

app :: Config -> Application 
app = serveWithContext _API { basicAuth: checkBasicAuth} <<< api

-- app :: Config -> Application 
-- app config req res = swerveContext (Proxy :: _ API) { verifiedToken: verifyToken (Secret config.secretToken) } (api config) req res 
