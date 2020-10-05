module API.Login where 

import Prelude

import Config (Config)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (asks)
import Data.Binary.Base64 as Base64
import Data.Either (Either(..), note)
import Data.TextDecoder (decodeUtf8)
import Data.User (Users, mkUser)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Node.Jwt (Secret(..))
import Swerve.API.Combinators (type (:>), type (:<|>), (:<|>))
import Swerve.API.Header (Header)
import Swerve.API.MediaType (JSON, PlainText)
import Swerve.API.Name (type (:=))
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get, Post)
import Swerve.Server.Internal.Handler (Handler)
import Swerve.Server.Internal.ServerError (err400, err401, err501)
import Utils (generateToken, parseBasicAuth)

type SecretToken = String 
type LoginAPI = "loginAPI" := (Login :<|> Logout)

type Login 
  =  Post "/login" 
  :> Header "authorization" String 
  :> Resource String PlainText

type Logout
  =  Post "/logout" 
  :> Header "authorization" String 
  :> Resource String PlainText

login :: SecretToken -> Handler Login String
login secret = do 
  encAuth <- asks $ _.header.authorization
  let decAuth = Base64.decode encAuth >>= decodeUtf8 >>= (note (error "Failed to parse Authentication") <<<  parseBasicAuth)
  case decAuth of
    Left e -> throwError err400
    Right auth -> do 
      liftAff $ generateToken (Secret secret) 

logout :: Handler Logout String
logout = throwError $ err501

loginAPI secretToken = login secretToken :<|> logout