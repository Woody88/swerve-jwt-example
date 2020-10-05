module Utils where

import Data.Debug
import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Utils as String
import Effect.Aff (Aff, throwError)
import Network.Wai (Request(..))
import Node.Jwt (Secret(..), Token, Verified, defaultClaims, defaultHeaders)
import Node.Jwt as JWT
import Swerve.Server.Internal.ServerError (ServerError(..), err401, err403)

type JWTToken = String 
type Username = String 
type Password = String 

data BasicAuth = BasicAuth Username Password

derive instance genericBasicAuth :: Generic BasicAuth _

instance debugBasicAuth :: Debug BasicAuth where
  debug = genericDebug

parseBasicAuth :: String -> Maybe BasicAuth
parseBasicAuth str = String.words str # case _ of 
  ["Bearer", userpwd ] | [user, pwd ] <- String.split (Pattern ":") userpwd 
    -> Just $ BasicAuth user pwd 
  otherwise            
    -> Nothing

generateToken :: Secret -> Aff JWTToken
generateToken secret = JWT.sign secret defaultHeaders defaultClaims

verifyToken :: Secret -> Request -> Aff (Either ServerError (Token () Verified))
verifyToken secret (Request req) = let 
  etoken = lmap (const err403) (accessToken >>= getBearer >>= JWT.verify' secret)
  in pure etoken 
  where 
    accessToken = note (pure "error") (Map.lookup (wrap "authorization") $ Map.fromFoldable $ req.headers) 
    getBearer str = String.words str # case _ of 
      ["Bearer", token ] -> Right token
      otherwise          -> Left $ (pure "error") 
