module API where 

import API.User (UserAPI, userAPI)
import API.Login (LoginAPI, loginAPI)
import Config 
import Swerve.API 
import Swerve.Server (Server)
import Swerve.Server (eval) as Server
import Type.Proxy (Proxy(..))

  
type API = UserAPI :<|> LoginAPI

_API = Proxy :: _ API

api :: Config -> Server API
api cfg = Server.eval (userAPI :<|> loginAPI cfg.secretToken)