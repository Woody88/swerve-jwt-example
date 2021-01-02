{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "b64"
  , "console"
  , "crypto"
  , "debugged"
  , "effect"
  , "encoding"
  , "heterogeneous"
  , "leibniz"
  , "lists"
  , "node-jwt"
  , "node-process"
  , "prelude"
  , "psci-support"
  , "strings"
  , "strings-extra"
  , "swerve"
  , "typedenv"
  , "wai-logger"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
