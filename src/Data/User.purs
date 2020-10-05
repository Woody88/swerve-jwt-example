module Data.User where

import Simple.JSON as Json

type Users = Array User 

newtype User 
  = User { id   :: String 
         , name :: String  
         }

derive newtype instance readForeignUser :: Json.ReadForeign User
derive newtype instance writeForeignUser :: Json.WriteForeign User

mkUser :: String -> String -> User 
mkUser id name = User { id, name }

