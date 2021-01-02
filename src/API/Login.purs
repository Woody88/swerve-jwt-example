module API.Login where 

import Prelude

import Config (Config)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (asks)
import Data.Binary.Base64 as Base64
import Data.Either (Either(..), note)
import Data.TextDecoder (decodeUtf8)
import Data.User (Users, User, mkUser)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Node.Jwt (Secret(..))
import Swerve.API
import Swerve.Server
import Swerve.Server (lift, eval) as Server
import Utils (generateToken)
import Type.Proxy

type SecretToken = String 
type LoginAPI = (BasicAuth "user" User :> Login) :<|> Logout

type Authorization = String 
type Login 
  =  "login" 
  :> Post JSON (Ok String + Nil)

type Logout
  =  "logout" 
  :> Header "authorization" String 
  :> Get JSON (Ok String + Nil)

loginAPI :: SecretToken -> Server LoginAPI
loginAPI secret = Server.lift (login :<|> logout) 
  where 
    logout :: Authorization -> Handler (Ok String + Nil)
    logout hdr = pure <<< respond (Proxy :: _ Ok') $ "logout"

    login :: User -> Handler (Ok String + Nil)
    login user = do 
      token <- liftAff $ generateToken (Secret secret)
      pure <<< respond (Proxy :: _ Ok') $ token