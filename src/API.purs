module API where 

import API.Login (LoginAPI, loginAPI)
import API.User (UserAPI, userAPI)
import Swerve.API.Combinators (type (:<|>), (:<|>))

  
type API = LoginAPI :<|> UserAPI

api config = { loginAPI: loginAPI config.secretToken, userAPI }