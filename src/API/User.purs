module API.User where

import Prelude

import Data.User (Users, User, mkUser)
-- import Node.Jwt (Verified, Token)
import Swerve.API 
import Swerve.Server 
import Swerve.Server (lift) as Server
import Type.Proxy (Proxy(..))

type UserAPI = "users" :> (GetUsers :<|> GetUser)

type UserId = Int 
type GetUsers = Get JSON (Ok Users + Nil)
type GetUser  = Capture "userId" UserId :> Get JSON (Ok User + Nil)

userAPI :: Server UserAPI
userAPI = Server.lift (users :<|> user) 
  where 
    users :: Handler (Ok Users + Nil)
    users = pure <<< respond (Proxy :: _ Ok') $ [ mkUser 1 "Woodson", mkUser 2 "Thomas" ]

    user :: UserId -> Handler (Ok User + Nil)
    user id = pure <<< respond (Proxy :: _ Ok') $ mkUser id "Woodson"