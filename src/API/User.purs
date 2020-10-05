module API.User where

import Prelude

import Data.User (Users, mkUser)
import Node.Jwt (Verified, Token)
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>), type (:<|>), (:<|>))
import Swerve.API.Guard (Guard)
import Swerve.API.MediaType (JSON, PlainText)
import Swerve.API.Name (type (:=))
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get)
import Swerve.Server.Internal.Handler (Handler)

type UserAPI = "userAPI" := Guard "verifiedToken" (Token () Verified) :> GetUsers

type GetUsers 
  =  Get "/users" 
  :> Resource Users JSON

getUsers :: Handler GetUsers Users
getUsers = pure [ mkUser "1" "Woodson", mkUser "2" "Thomas" ]

userAPI :: Token () Verified -> Handler GetUsers Users
userAPI token = getUsers