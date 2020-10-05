module Config where

import Prelude

import Data.Either (Either)
import Data.Foldable (find)
import Data.Int as Int
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Process as Process
import Type.Data.Row (RProxy(..))
import TypedEnv (class ParseValue, type (<:), EnvError, envErrorMessage)
import TypedEnv as TypedEnv

newtype Port = Port Int

fromPort :: Port -> Int 
fromPort (Port int) = int 
 
derive newtype instance showInt :: Show Port

instance parseValuePort :: ParseValue Port where
  parseValue = map Port <<< find (_ <= 65535) <<< Int.fromString


type ConfigT 
  = ( secretToken :: String <: "SECRET_TOKEN" 
    , port        :: Port   <: "PORT"
    )

type Config = { port :: Port, secretToken :: String }

loadConfig :: Effect (Either EnvError Config)
loadConfig = do 
  Console.log "Loading configs..." 
  TypedEnv.fromEnv (RProxy :: RProxy ConfigT) <$> Process.getEnv